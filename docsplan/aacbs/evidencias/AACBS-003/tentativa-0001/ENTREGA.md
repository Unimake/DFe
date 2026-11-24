# AACBS-003 — Autenticação, transporte e débitos

Estado: **DELIVERED_FOR_REVIEW**, 24/11/2026. Implementação, validação offline e ciclo real de débitos em Produção Beta concluídos. Metadados locais atualizados com autorização expressa, preservando a credencial. O XML real foi validado, gravado cifrado e relido independentemente. A apuração real está vazia; os cenários com itens são sintéticos. Aprovação da etapa pendente do DEV; AACBS-004 não foi iniciada.

## Entrega e cobertura

Cinco serviços públicos em `Servicos/AACBS`, configuração nacional AN e recursos embutidos: `Autenticar`, `ConsultarDebitos`, `ConsultarSituacaoSolicitacao`, `ConsultarLinkDownload` e `BaixarArquivoRetorno` para débitos. Recebem objeto/XML, validam envio, usam o executor compartilhado, convertem JSON explicitamente e validam XML/Result. Espera e persistência continuam no consumidor.

| Responsabilidade | Prova offline |
|---|---|
| Autenticação | Basic com form-encoding UTF-8 dos componentes; cache/expiração, isolamento, concorrência, duas tentativas/hora e ausência de repetição automática |
| Fluxo de débitos | Abertura, situação, conclusão, link, arquivo vazio/com itens; HTTP, XML/XSD, Result e gravação; correlação de tíquete/CNPJ, precisão, datas e códigos |
| Falhas e segurança | Limpeza na reexecução, SEM_RETORNO, erro fiscal preservado; conteúdo incompatível com caminho/família sem valores; sentinelas de Trace/exceções; PRR bloqueada antes da autenticação |
| Transporte | Proxy explícito; download sem Bearer, certificado, credenciais padrão, cookies ou redirecionamento; leitura após headers preserva HTTP em falhas e tem timeout; vida do cliente até Dispose |
| Roteiro local | DPAPI em bytes UTF-8, cache/journal entre processos, cota, retomada, POST ambíguo sem replay, limite de espera, URL expirada; XML >64 KiB e CR/CRLF, tíquete ISO e nomes JSON duplicados escapados |

A URL assinada não é reescrita. A compatibilidade `netstandard2.0`/Framework impede garantir a forma literal de toda URI: se `System.Uri` alteraria o endereço ou o alvo caminho/query, o download é recusado antes do GET, com mensagem sanitizada. A limitação é explícita, sem enviar uma assinatura alterada.

Infraestrutura compartilhada limitada a opções internas de cookies/redirecionamento/leitura após headers (defaults antigos preservados), propriedade opcional `Configuracao.AACBSToken`, setter protegido de HTTP e supressão de Trace de XML somente AACBS. Nenhuma mudança de XMLBase, pacote, consumidor ou classificador de disponibilidade.

## Fontes e revisão

[Revalidação das 23 fontes](REVALIDACAO-FONTES.csv): 23/23 hashes iguais à baseline aprovada. Pesquisa adicional confirmou ambientes distintos, possibilidade de credenciais compartilhadas e ausência de seletor PRR documentado. O PDF público Receita Integra tem bytes distintos da cópia CGIBS, mas texto integral idêntico, conforme o dossiê de pesquisa.

Revisão independente encontrou e teve corrigidos: inteiros matemáticos JSON (55.0/5.5e1), possível normalização da URL assinada, diagnóstico do campo inválido, limite de SecureString na persistência, perda de CR na gravação, conversão automática do tíquete ISO e duplicatas de nomes escapados nos metadados. Cada correção tem regressão. Revisor reconferiu o código final e os quatro ajustes do roteiro, sem novos achados verificáveis; revisão estática não substitui os runners abaixo.

## Validação executada

Base Git do checkpoint: `09494afb10a1201d1a10a1b637b20614697cad40`. [Checkpoint final](CHECKPOINT-FINAL.csv): 114 arquivos de código/contratos/testes/projetos e quatro scripts do roteiro, **118/118 hashes inalterados** entre a validação final e o consumo real concluído. Os [40 artefatos Debug](ARTEFATOS-NI.csv) também permaneceram iguais. Documentação e evidências posteriores não modificam esse código.

| Comando / filtro | Resultado | Evidência |
|---|---|---|
| dotnet build Unimake.DFe.Test.csproj --no-restore --no-incremental -c Debug | Exit 0; 4 avisos preexistentes, 0 erros; inclui biblioteca netstandard2.0 | [build-debug.log](build-debug.log) |
| Runner Debug/net10.0 -trait DFe=AACBS | 338 casos, 0 falhas/erros/ignorados | [testes-debug.log](testes-debug.log) |
| Quatro classes de transporte/proxy/observações, mais ApiTimeoutTest e EFreteTransporteHttpTest | 77 + 15 casos, 0 falhas/erros/ignorados; somente dados sintéticos, reflexão e loopback | [regressao-transporte.log](regressao-transporte.log) |
| pwsh Executar-AACBS003.ps1 -Offline -Dll saída Debug/net10.0/Unimake.Business.DFe.dll | Exit 0; DPAPI/journal e ciclo dos cinco serviços com transporte controlado; dependências JSON isoladas, sem API | [roteiro-offline.log](roteiro-offline.log) |
| dotnet build Unimake.DFe.Test.csproj --no-restore --no-incremental -c INTEROP_Release | Exit 0; 15 avisos preexistentes, 0 erros | [build-interop.log](build-interop.log) |
| Runner INTEROP_Release/net10.0 -trait DFe=AACBS | 340 casos, 0 falhas/erros/ignorados | [testes-interop.log](testes-interop.log) |
| pwsh Executar-AACBS003.ps1 -Verificar | Exit 0; leitura local e DPAPI verificadas; ambiente PRR/rota pendente, sem requests | [configuracao-local.log](configuracao-local.log) |

O runner foi executado pela DLL xUnit v3 diretamente; não houve suíte integral. Primeira compilação detectou dois usos de métodos exclusivos COM nos testes Debug, corrigidos pelos acessos comuns já existentes. O primeiro loopback foi bloqueado pelo sandbox; a repetição com permissão para sockets locais passou sem alterar código/testes. Não houve rejeição do auto-review. Os logs finais conservam avisos preexistentes de EnviNFe/INTEROP e a mensagem do PostBuild legado `*Undefined*Build.bat`; o build retorna 0, sem alteração desse fluxo externo ao escopo.

Antes da compatibilidade NI, passaram 313/315 casos AACBS; a validação final acrescentou 25 casos líquidos para esse desvio. As 92 regressões de transporte foram mantidas porque seus componentes não mudaram depois dessa execução. UniNFe/PRO e pacote não foram alterados/testados: não existe consumidor AACBS integrado nesta etapa, e a consolidação/empacotamento pertence à 005.

## Execução real em Produção Beta

O DEV autorizou a mudança de ambiente. A verificação local Beta passou sem exibir dados e a credencial DPAPI foi preservada: [configuracao-beta.log](configuracao-beta.log).

A primeira tentativa do roteiro falhou sem resposta HTTP por conflito local de `System.Text.Json`: PowerShell carregou versão 10, enquanto a DLL exige 11. O erro foi reproduzido sem credenciais, usando executor controlado, antes de acionar o transporte. A reserva dessa tentativa permanece no journal. O ajuste ficou somente no suporte local: `AssemblyLoadContext` isolado para a DLL e suas dependências. O modo `-Offline -Dll` passou a executar os cinco serviços com o transporte controlado, além das provas de DPAPI/journal, confirmando versões 10/11 separadas. Revisão independente do ajuste sem achados; [evidência offline](roteiro-isolamento-0001.log).

A retomada da autenticação foi feita uma única vez, sem zerar contadores: **HTTP 200**, token utilizável em cache cifrado, processo exit 0. Nenhum token/credencial foi exibido: [autenticar-real-0002.log](autenticar-real-0002.log).

O ciclo real fez uma abertura de débitos (**201**), uma consulta de situação (**200, Concluida**), uma consulta de link (**200**) e um download (**200**). O último passo teve falha técnica de interpretação/validação e não disponibilizou XML fiscal; o processo terminou com exit 1 e `DownloadIndisponivel`, preservando o tíquete. Não houve repetição do POST nem nova autenticação: [debitos-real-0001.log](debitos-real-0001.log). A investigação controlada usa somente GET do mesmo tíquete e diagnóstico protegido para identificar o campo/formato divergente, sem considerar HTTP 200 como prova de arquivo fiscal válido.

A [reconciliação sanitizada](reconciliacao-download-0001.json) fez exatamente um GET adicional de link e um de arquivo, ambos 200. O arquivo tem 203 bytes, JSON UTF-8 válido, sem BOM/compressão, e apuração vazia. A divergência é `$.ni`: a API devolveu a matriz completa (14 dígitos), correspondente ao mesmo contribuinte e tíquete; HTML/schema/contrato aprovado exigem oito. Em cópia somente em memória, a substituição isolada pela base de oito permitiu validar todos os demais campos e o XML. O original permanece intacto e cifrado, sem XML real emitido, sem nova abertura ou token. O hash versionado é somente do arquivo cifrado; scripts diagnósticos temporários foram removidos para evitar repetição acidental.

A [proposta de compatibilidade 8/14](PROPOSTA-NI-RETORNO.md) foi aprovada explicitamente pelo DEV. O ajuste é local ao ni do arquivo de débitos, preservando o valor recebido e a correlação com a base do pedido, sem ampliar outros contratos. A auditoria de impacto registrou a cadeia XSD e os consumidores, sem alteração de tipo público, ordem, COM ou recursos. Revisor independente não encontrou problemas; os testes adicionais e a validação final passaram. Não houve flexibilização silenciosa ou truncamento do identificador real.

O [checkpoint inicial de consumo real](CHECKPOINT-REAL-0001.csv) e os [artefatos iniciais](ARTEFATOS-REAL.csv) conservam a identidade da autenticação e da primeira leitura. A retomada depois do ajuste usa o checkpoint final e ARTEFATOS-NI, sem misturar os resultados das duas versões.

**Retomada final comprovada:** o JSON original cifrado passou na nova DLL sem substituição de campos. O estado de download foi reconciliado explicitamente sob mutex, mantendo o tíquete e os contadores; executou-se um GET de link e um de download, ambos **HTTP 200**. Não houve nova autenticação, abertura ou consulta de situação. O journal terminou em `Finalizada`, com XML validado e gravado por DPAPI: [retomada-debitos-ni-0001.json](retomada-debitos-ni-0001.json).

O orquestrador fez uma [verificação independente sem rede](VERIFICACAO-REAL-FINAL.json), relendo o journal e o XML cifrado: XSD e desserialização válidos, NI de 14 dígitos preservado e igual à matriz, raiz e tíquete correspondentes, `InnerText` igual após round-trip e hash do arquivo cifrado coincidente. Resultado: **zero períodos e zero itens de débito**. Essa massa não demonstra itens/valores reais; estes continuam comprovados apenas por testes sintéticos.

Contagens do consumo confirmado: uma resposta de autenticação 200; uma abertura de débitos 201; uma consulta de situação 200; três GETs de link e três GETs de arquivo (original, diagnóstico e retomada), todos 200. O journal conserva duas tentativas locais de autenticação, incluindo a falha de carregamento anterior ao transporte. A cota não foi zerada e nenhuma abertura foi repetida.

## Responsáveis e limites da etapa

`aacbs003_servicos` (gpt-6.2-astra/high) produziu serviços/configuração e conduziu o diagnóstico/retomada críticos; `aacbs003_testes` e `aacbs003_ni_testes` (gpt-6.2-sol/medium) escreveram provas offline; `aacbs003_roteiro` (gpt-6.2-sol/medium) produziu e corrigiu suporte local. Orquestrador integrou arquivos compartilhados, testes adicionais, fontes, auditoria real sem rede e evidências. `aacbs003_revisao` (gpt-6.2-astra/high) fez revisão independente; `aacbs002_executor` (gpt-6-luna/low), reutilizado, foi o único executor de builds/runners, além da autenticação e do primeiro acompanhamento real. Diagnóstico/retomada tiveram transferência explícita da execução real, sem simultaneidade. Autoria paralela em subárvores distintas; quatro slots no máximo.

Versão externa do projeto e `Exemplos/DiagnosticoTlsEFrete` preservadas. Nenhum commit, publicação ou alteração de UniNFe/PRO. Credencial e estado operacional continuam ignorados em `.local/aacbs`; nenhum valor foi exibido ou copiado ao dossiê. O verificador local não comprova validade remota das chaves.

Conferência documental final: 24 Markdown, 153 links locais sem destino ausente e 120 registros da matriz CONSULTAS-CAMPOS com 12 colunas válidas. `git diff --check` aprovado; nenhum arquivo privado rastreado. Os hashes finais de 118 fontes/scripts e 40 artefatos foram reconferidos após a consolidação, sem divergências.

## Gate humano e continuidade

Etapa entregue para revisão humana, **aprovação aguardando o DEV**. O desvio NI 8/14 foi aprovado pontualmente e não equivale à aprovação da AACBS-003. A aprovação desta etapa libera a AACBS-004, conforme a regra original do DEV; nenhuma implementação ou emissão de DARF da sucessora foi antecipada.

O [roteiro](../../../scripts/ROTEIRO-AACBS003.md) continua no mesmo usuário/máquina, com a DLL do checkpoint, cache e journal protegidos. O tíquete está finalizado; uma repetição simples de Débitos retorna o estado concluído localmente. Não usar NovaSolicitacao apenas para repetir esta evidência. Se o código relevante mudar, repetir os checks afetados; manter o orçamento e DARF desabilitado até cenário específico da etapa apropriada.
