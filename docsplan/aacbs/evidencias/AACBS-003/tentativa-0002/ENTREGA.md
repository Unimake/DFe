# AACBS-003 — Correções após revisão

Estado: **DELIVERED_FOR_REVIEW**, 24/11/2026. Os quatro achados da revisão foram confirmados e corrigidos. Retrabalho solicitado pelo DEV: “Análise os problemas encontrados na revisão acima e se coerentes corrija.” Aprovação humana da etapa continua pendente; AACBS-004 não foi iniciada.

## Base e escopo

HEAD `09494afb10a1201d1a10a1b637b20614697cad40`, com alterações pendentes da AACBS-003. O [checkpoint final](CHECKPOINT-FINAL.csv) identifica 119 arquivos de código, testes, schemas, configurações, projeto e scripts com SHA-256. Todos ficaram estáveis durante a validação e foram reconferidos na entrega. A versão externa `2026.11.24.1644` do projeto e `Exemplos/DiagnosticoTlsEFrete` foram preservados.

Rodada inteiramente offline em Windows, .NET 10, PowerShell 7.6 e runner xUnit v3. Nenhuma chamada à Receita, importação de configuração privada, leitura de credenciais ou alteração de estado real. A [prova real Beta anterior](../tentativa-0001/ENTREGA.md) permanece histórica: não representa execução real deste binário corrigido. O retorno real anterior foi vazio; itens/valores continuam cobertos por cenários sintéticos.

## Achados e correções

| Achado confirmado | Comportamento corrigido | Prova |
|---|---|---|
| AN.xml externo podia substituir endpoints autenticados AACBS | Configuração AACBS carregada exclusivamente dos recursos embutidos, inclusive método de autenticação; HTTPS validado antes de usar credenciais/token. Preservados proxy, timeout, credenciais e observação do consumidor; nome da operação atualizado ao reutilizar Configuracao | AN.xml de versão superior substitui NFe no controle positivo, mas não AACBS. Requests controlados confirmam destinos e métodos oficiais. HTTP rejeitado antes de autenticar/enviar |
| AACBSToken deslocava membros da interface AutoDual | Propriedade opcional mantida na API .NET, marcada ComVisible(false) no INTEROP; COM usa ClientID/ClientSecret e autenticação integrada | Atributo validado e ITypeInfo nativo x64: 204 funções, nenhum AACBSToken, ProvedorCIOT recupera DISPID 1610743978 e offsets 1416/1424 |
| Getters COM de XML perdiam CR literal na releitura | GetConteudoXMLOriginal/GetConteudoXMLAssinado usam escrita XML que preserva CR/CRLF no namespace AACBS | Teste INTEROP de releitura preservando caracteres; controle NFe mantém exatamente OuterXml e assinaturas públicas existentes |
| Switch falso selecionava operação real no roteiro | Modo explicitamente selecionado exige valor verdadeiro antes de importar módulo, abrir configuração ou carregar DLL; ausência de modo mantém Verificar | Autenticar e Debitos falsos, diretos e por splatting, rejeitados antes de qualquer acesso privado/transporte |

Regressões novas em `AACBS/Servicos/AACBSRevisaoSegurancaTest.cs`. A primeira validação detectou uma exigência excessiva de IsWellFormedOriginalString para tíquetes já escapados; removida essa exigência, preservando HTTPS, autoridade e fragmento. Os testes do pipeline confirmam os tíquetes válidos. A configuração isolada exigiu copiar o nome interno da operação; regressão cobre reutilização NFe/AACBS e observação correta.

O roteiro revelou falha intermitente preexistente de File.Replace, HRESULT 1175. A gravação cifrada agora repete somente a substituição local atômica, no máximo cinco tentativas com intervalo de 50 ms, para erros de compartilhamento/bloqueio/1175 e apenas enquanto o temporário existir. Não repete requests nem operações de negócio. Testes cobrem recuperação, esgotamento, ausência do temporário e erro de acesso sem repetição. Sem fallback de gravação não atômica.

## Verificação executada

| Comando ou cenário | Resultado | Evidência |
|---|---|---|
| dotnet build source/Unimake.DFe.Test/Unimake.DFe.Test.csproj --no-restore --no-incremental -c Debug | Exit 0; 4 avisos, 0 erros; inclui biblioteca netstandard2.0 | [build-debug.log](build-debug.log) |
| dotnet source/Unimake.DFe.Test/bin/Debug/net10.0/Unimake.DFe.Test.dll -trait DFe=AACBS | 341 testes, zero falhas/erros/ignorados | [testes-debug.log](testes-debug.log) |
| Runner Debug, sete classes focadas de XML/validação/observação | 132 testes, zero falhas/erros/ignorados | [regressao.log](regressao.log) |
| pwsh -NoProfile -File docsplan/aacbs/scripts/Executar-AACBS003.ps1 -Offline -Dll source/Unimake.DFe.Test/bin/Debug/net10.0/Unimake.Business.DFe.dll | Exit 0; DPAPI/journal sintéticos e ciclo dos cinco serviços com transporte controlado; contextos JSON 10/11 isolados | [roteiro-offline.log](roteiro-offline.log) |
| dotnet build source/Unimake.DFe.Test/Unimake.DFe.Test.csproj --no-restore --no-incremental -c INTEROP_Release | Exit 0; 15 avisos, 0 erros | [build-interop.log](build-interop.log) |
| dotnet source/Unimake.DFe.Test/bin/INTEROP_Release/net10.0/Unimake.DFe.Test.dll -trait DFe=AACBS | 344 testes, zero falhas/erros/ignorados | [testes-interop.log](testes-interop.log) |
| ITypeInfo nativo x64 de Configuracao, CLR Windows, sem registro | 204 funções; ClientSecret/ProvedorCIOT com posições restauradas ao ocultar AACBSToken | [interop-itypeinfo.txt](interop-itypeinfo.txt) |

Regressões: ValidarSchemaTest, ValidarConfigTest, NormalizacaoXMLTest, NFe.Serializacao.DesserializacaoSeguraTest, DCe.Serializacao.SerializacaoDesserializacaoTest, Utility.Rede.CanalObservacoesDisponibilidadeTest e MigracaoDiagnosticoSessaoTest. Não foi executada a suíte integral. As 92 regressões anteriores de transporte permanecem na tentativa anterior; seus componentes não mudaram neste retrabalho. Avisos preexistentes e PostBuild legado preservados. Sem pacote, publicação, commits ou alteração de consumidor UniNFe/PRO.

A comparação COM foi entre o binário com o defeito e o corrigido, sem registro no Windows; não constitui comparação de IID com pacote publicado. O hash do binário final consta da evidência nativa.

## Agentes e revisão

Orquestrador: Configuracao.AACBSToken, getters comuns, metadados internos, roteiro PowerShell e documentação. `aacbs003_corrigir_endpoints` (gpt-6.2-sol/medium): base AACBS e resolução embutida. `aacbs003_testes_revisao` (gpt-6.2-sol/medium): regressões dos achados. `aacbs002_executor` reutilizado (gpt-6-luna/low): único executor de builds/testes do checkout estável. `revisao_aacbs003_core` (gpt-6.2-astra/high): revisão do reparo e ITypeInfo; `revisao_aacbs003_roteiro` (gpt-6.2-sol/medium): revisão do roteiro e persistência. Revisão final sem falha remanescente nos quatro pontos. Revisão e execução não aprovam a etapa.

## Retomada e gate humano

Quatro achados tratados. Decisão do DEV: **AGUARDANDO**. A compatibilidade NI 8/14 já aprovada não amplia outras famílias. Novo retrabalho deverá preservar as tentativas anteriores e abrir tentativa-0003. Reversão, se necessária, deve retirar somente estas correções, preservando alterações externas e a implementação anterior; nenhuma reversão foi executada. A aprovação explícita de AACBS-003 libera AACBS-004 conforme o plano. Até lá, sucessora PLANNED.
