# AACBS-004 — Correções da revisão

Estado **BLOCKED** nos critérios reais preservados da etapa, 25/11/2026. Os três achados da revisão foram confirmados, corrigidos e validados offline. A entrega desses reparos está disponível para revisão; isso não conclui nem aprova a AACBS-004 e não libera AACBS-005.

## Base e escopo

DEV: “Analise os problemas detectados na revisão acima e se coerentes, corrija.” A [tentativa-0001](../tentativa-0001/ENTREGA.md) permanece histórica, com suas fontes, provas reais e impedimentos. Base Git: aba5ab09510d3ea11459edcbdc4fb76cb3f7b350. O [checkpoint final](CHECKPOINT-FINAL.csv) identifica 131 arquivos, inalterados durante a validação. Em comparação ao checkpoint anterior, somente quatro arquivos de código/teste mudaram: AACBS-RoteiroLocal.psm1, Executar-AACBS004.ps1, Testar-ConsultasAACBS004.ps1 e AACBSFluxoConsultasTest.cs.

[Contexto externo](CONTEXTO-EXTERNO.csv): os dois arquivos QRCode de outro trabalho foram preservados e tiveram hashes estáveis durante os builds, sem edição ou testes desse domínio. A versão externa do projeto e Exemplos/DiagnosticoTlsEFrete também foram preservados. Os hashes registram contexto de compilação, não autoria ou inclusão dessas mudanças no escopo dos reparos.

Nenhuma chamada real, leitura de credenciais reais, alteração de metadados/journal/cotas ou novo acesso ao arquivo fiscal cifrado. Os XMLs, XSDs, mappers e serviços C# de produção da AACBS não mudaram. A compatibilidade NI de créditos continua aguardando o DEV; não foi presumida. Nenhum DARF, pagamento, pacote, publicação ou commit.

## Tratamento dos achados

| Achado | Correção | Prova de regressão |
|---|---|---|
| Entrada 004 usava o próprio worktree como raiz do acesso, diferente da 003 | RepositorioLocal sem default derivado do script; quando não informado, a entrada resolve o checkout principal pela lista de worktrees, seguindo a regra 003. Override explícito preservado | Cópia temporária da entrada executada em runspace isolado com Git e módulo simulados: duas raízes com espaços, uma chamada ao Git, default principal e override sem nova resolução. Não cria worktree ou altera Git real |
| Diagnóstico descartava campos OAuth com sublinhado | Os dois filtros de caminho aceitam sublinhado nos nomes, mantendo ancoragem e restrições de formato | Get-ContractFailure e stream de informação de Write-Observation reconhecem scope, access_token, token_type e expires_in. Mensagem arbitrária, query e sufixo malicioso continuam rejeitados, sem copiar valores remotos |
| A03 não cobria as três formas de webhook em cada abertura | Nove casos novos em ExecutarAberturaPreservaWebhookEContextoDaFamilia: créditos/pagamentos/recolhimentos com URL ausente, vazia e HTTPS | Construtor com objeto XML tipado, distinção de presença no XML, Executar, um POST fiscal, rota/CNPJ com zero inicial, JSON exato com escapes/query, HTTP 201, tíquete, estimativa e retorno XSD |

O ciclo existente conserva sua assinatura e cobertura do construtor string, inclusive na chamada pelo host PowerShell com dependências isoladas. O teste de worktree usa temporários próprios, Git fake e módulo stub; não importa acesso real. Os dois pontos de sanitização são testados separadamente para evitar que um aceite a chave e o outro suprima o campo.

## Verificação executada

Windows, PowerShell 7.6, testes net10.0/xUnit v3, biblioteca netstandard2.0/C# 7.3. Um executor de builds/runners por checkout.

| Comando | Resultado | Evidência |
|---|---|---|
| dotnet build source/Unimake.DFe.Test/Unimake.DFe.Test.csproj --no-restore --no-incremental -c Debug | Exit 0; 4 avisos preexistentes, zero erros | [build-debug.log](build-debug.log) |
| dotnet source/Unimake.DFe.Test/bin/Debug/net10.0/Unimake.DFe.Test.dll -class Unimake.DFe.Test.AACBS.Servicos.AACBSFluxoConsultasTest | 21 casos, zero falhas/erros/ignorados | [testes-debug.log](testes-debug.log) |
| pwsh -NoProfile -File docsplan/aacbs/scripts/Executar-AACBS004.ps1 -Offline -Dll source/Unimake.DFe.Test/bin/Debug/net10.0/Unimake.Business.DFe.dll | Exit 0; regressões PowerShell e quatro fluxos sintéticos/ALC, sem API real | [roteiro-offline.log](roteiro-offline.log) |
| Mesmo build, -c INTEROP_Release | Exit 0; 15 avisos preexistentes, zero erros | [build-interop.log](build-interop.log) |
| Mesmo runner INTEROP_Release/net10.0, mesma classe | 26 casos, zero falhas/erros/ignorados | [testes-interop.log](testes-interop.log) |

Todos os checks finais passaram na primeira sequência. O autor das regressões PowerShell também executou uma prova preliminar sem DLL, exit 0, antes do checkpoint. Os 131 hashes do checkpoint e os dois do contexto externo ficaram iguais antes/depois, reconferidos pelo orquestrador. Não houve suíte integral nem repetição dos 428/436 casos anteriores, medição de volume, inspeção COM nativa ou chamadas reais: os componentes de produção correspondentes não mudaram. Os números 21/26 são da classe afetada, sem somar execuções manuais do roteiro aos casos do runner.

## Agentes e revisão independente

Orquestrador: resolução do checkout, filtros do módulo e documentação/evidências. aacbs004_corrigir_webhooks (gpt-6.2-sol/medium): arquivo C# de teste. aacbs004_corrigir_testes_ps (gpt-6.2-sol/medium): regressões PowerShell. aacbs002_executor reutilizado (gpt-6-luna/low): único executor dos builds, classe focada e roteiro com DLL. revisao004_final_roteiro (gpt-6.2-sol/medium) e revisao004_final_core (gpt-6.2-sol/high): reconferiram os reparos e o cumprimento do A03, sem falhas remanescentes nesse escopo. Revisão técnica não equivale à aprovação humana da etapa.

## Retomada e gate humano

Os três achados estão tratados. AACBS-004 permanece BLOCKED exclusivamente nas condições reais documentadas antes desta correção: decisão NI 8/14 para créditos, qualificação dos HTTP 404 de pagamentos/recolhimentos e dados/confirmação específica de cenário DARF. Nada disso foi tratado como autorização por presunção ou tempo decorrido. A AACBS-005 permanece PLANNED.

Novo retrabalho após esta entrega abre tentativa-0003. Se uma reversão for solicitada, retirar somente os quatro deltas desta tentativa e seus registros, preservando a implementação anterior e alterações externas; nenhuma reversão foi executada. O PDCA registra o início do retrabalho e o retorno ao bloqueio real após a correção offline. Aprovação da AACBS-004: AGUARDANDO, sem pedido de avanço enquanto seus critérios obrigatórios estiverem pendentes.
