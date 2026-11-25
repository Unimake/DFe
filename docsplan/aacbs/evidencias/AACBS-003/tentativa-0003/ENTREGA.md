# AACBS-003 — Contexto da autenticação auxiliar

Estado: **DELIVERED_FOR_REVIEW**, 24/11/2026. DEV solicitou corrigir o achado da revisão final: falha OAuth identificada como resposta de ConsultarDebitos antes de ocorrer POST fiscal. Escopo somente AACBS-003; AACBS-004 não iniciada.

## Base e correção

HEAD `09494afb10a1201d1a10a1b637b20614697cad40`, com alterações pendentes desta etapa. O [checkpoint final](CHECKPOINT-FINAL.csv) identifica 119 arquivos com SHA-256. Em relação à tentativa-0002, mudaram somente `Servicos/AACBS/ServicoBase.cs`, `Servicos/AACBS/RespostaAACBS.cs` e `AACBS/Servicos/AACBSAutenticacaoTest.cs`. A versão externa do projeto e `Exemplos/DiagnosticoTlsEFrete` foram preservados.

O achado era coerente. O mapper aceitava scope vazio, proibido quando presente no XSD. A falha posterior na validação do XML auxiliar usava a operação fiscal ao montar a exceção. Agora o mapper rejeita esse valor com `AACBS Autenticar: resposta incompatível com o contrato em $.scope.` O campo continua opcional e os valores não vazios são preservados. A validação posterior do XML auxiliar recebe explicitamente o contexto OAuth e também identifica Autenticar em falhas de contrato que cheguem a essa camada.

Não houve alteração de API pública/COM, XSD, endpoints, autenticação de sucesso, transporte compartilhado ou roteiro local. Nenhuma chamada real à Receita, leitura de credenciais, importação da configuração privada ou modificação do journal foi executada. A prova Beta da tentativa-0001 permanece histórica; não representa execução real deste novo binário.

## Testes e revisão

Três casos novos, com transporte controlado e somente dados sintéticos:

- ConsultarDebitos.Executar com token Bearer e scope vazio: erro Autenticar em $.scope, uma chamada somente ao emissor, nenhum POST fiscal, HTTP fiscal zero, XML nulo, SEM_RETORNO e exceção sem sentinelas/credenciais.
- ConsultarDebitos.Executar com error vazio em HTTP 400: mapper chega à validação XSD, que preserva contexto Autenticar em $, sem POST fiscal e com HTTP fiscal zero. Esse caso protege o fallback de validação separadamente da correção do mapper.
- Autenticar.Executar com scope vazio em HTTP 200: mensagem com operação/campo corretos, HTTP 200 preservado, XML nulo e SEM_RETORNO.

Casos preexistentes mantêm scope válido, scope ausente e extensões OAuth desconhecidas. Revisão independente do reparo por `revisao_aacbs003_core` (gpt-6.2-astra/high) confirmou ambos os caminhos, sem achado remanescente nesse escopo.

Propriedade: orquestrador implementou os dois arquivos de serviço/mapper e documentação; `aacbs003_testes_revisao` (gpt-6.2-sol/medium) escreveu os testes; `aacbs002_executor` reutilizado (gpt-6-luna/low) é o único executor dos builds/runners do checkpoint estável.

## Validação executada

Os 119 hashes do checkpoint permaneceram iguais antes e depois dos checks. Ambiente Windows, .NET 10, biblioteca netstandard2.0/C# 7.3, runner xUnit v3 executado diretamente pela DLL:

| Comando | Resultado | Evidência |
|---|---|---|
| dotnet build source/Unimake.DFe.Test/Unimake.DFe.Test.csproj --no-restore --no-incremental -c Debug | Exit 0; 4 avisos preexistentes, 0 erros | [build-debug.log](build-debug.log) |
| dotnet source/Unimake.DFe.Test/bin/Debug/net10.0/Unimake.DFe.Test.dll -trait DFe=AACBS | 344 testes, 0 erros/falhas/ignorados | [testes-debug.log](testes-debug.log) |
| dotnet build source/Unimake.DFe.Test/Unimake.DFe.Test.csproj --no-restore --no-incremental -c INTEROP_Release | Exit 0; 15 avisos preexistentes, 0 erros | [build-interop.log](build-interop.log) |
| dotnet source/Unimake.DFe.Test/bin/INTEROP_Release/net10.0/Unimake.DFe.Test.dll -trait DFe=AACBS | 347 testes, 0 erros/falhas/ignorados | [testes-interop.log](testes-interop.log) |

Todos os checks passaram na primeira execução desta tentativa. Não houve necessidade de repetir inspeção nativa COM, regressões de transporte compartilhado ou roteiro PowerShell: esses componentes não mudaram, e suas evidências estão na [tentativa-0002](../tentativa-0002/ENTREGA.md). Sem suíte integral, pacote, publicação, alteração de UniNFe/PRO ou commits.

## Gate humano

Correção entregue para revisão em 24/11/2026. Aprovação do DEV: AGUARDANDO. A aprovação técnica dos agentes não libera a AACBS-004. Novo retrabalho após esta entrega deverá abrir tentativa-0004, preservando o histórico; reversão deve retirar apenas estes três incrementos e os registros correspondentes, mantendo a implementação anterior e alterações externas. Nenhuma reversão executada.

Aprovação recebida em 24/11/2026: DEV aprovou AACBS-003 e liberou AACBS-004, com consultas reais em Produção Beta e confirmação específica antes de emitir DARF. Transição registrada no PDCA.
