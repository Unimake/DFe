# AACBS-004 — Demais famílias e DARFs

Estado **BLOCKED**, 24/11/2026: execução autorizada concluída até os impedimentos reais abaixo; a etapa não está entregue como concluída nem aprovada. DEV aprovou AACBS-003 e liberou AACBS-004 com consultas reais em Produção Beta. Antes de qualquer emissão RAD/PCONT, confirmar cenário e parâmetros com o DEV. Nenhuma autorização de emissão foi presumida pela implementação offline. AACBS-005 não foi iniciada.

## Base, fontes e entrega

Base Git `e2ce94299` (após commits da AACBS-003). [Revalidação pública](REVALIDACAO-FONTES.csv): 23/23 fontes com hashes idênticos à baseline aprovada. O [checkpoint final](CHECKPOINT-FINAL.csv) identifica 131 arquivos com SHA-256; ficaram estáveis durante a validação. Versão externa do projeto e Exemplos/DiagnosticoTlsEFrete preservados.

Cinco serviços novos: ConsultarCreditos, ConsultarPagamentos, ConsultarRecolhimentos, GerarDarfAdquirente e GerarDarfContribuinte. Todos recebem objeto/XML, usam a base AACBS e o transporte compartilhado, validam XML de envio/retorno e oferecem Result concreto e INTEROP. Configuração nacional embutida completa as dez unidades do catálogo. Dois arquivos parciais de mapper reutilizam as regras exatas de número/instante/objeto, sem expor JSON ao consumidor.

O download seleciona explicitamente a família do pedido e correlaciona tíquete e contribuinte. A apuração pode estar vazia; listas internas obrigatórias não podem. São preservados opcionais, zeros, códigos, datas/offset/frações e valores decimais. RAD envia array ordenado e recebe resultado global; PCONT envia objeto por período. Datas DARF seguem as duas grafias em escolha exclusiva já aprovadas. Não há pagamento ou download de PDF/PIX.

A compatibilidade NI 8/14 permanece exclusiva de débitos; demais famílias continuam com oito dígitos. Não houve alteração dos modelos XML/XSD da etapa 002 nem de transporte compartilhado ou APIs públicas existentes.

Roteiro local: quatro famílias com journal/cache/mutex comum e cota de abertura separada por API, preservando execuções antigas. SemAguardar permite sair antes da janela de 120 segundos e retomar explicitamente. Modo falso é rejeitado antes de importar configuração. DARF não é emitido por esse roteiro. Falhas de contrato preservam somente operação e caminho estrutural sanitizado, extraídos do formato controlado da DLL, sem mensagem remota ou exceção bruta. XML operacional é gravado somente sob pedido, cifrado por DPAPI fora do Git.

## Validação offline

| Verificação | Resultado | Evidência |
|---|---|---|
| dotnet build source/Unimake.DFe.Test/Unimake.DFe.Test.csproj --no-restore --no-incremental -c Debug | Exit 0; zero avisos/erros na execução final | [build-debug.log](build-debug.log) |
| Runner direto Debug/net10.0, -trait DFe=AACBS | 428 testes, zero falhas/erros/ignorados | [testes-debug.log](testes-debug.log) |
| Mesmo runner, método MilCreditosMantemContagemEContratoNoRoundTrip | 1 teste; 1.000 créditos; 565.208 bytes JSON UTF-8, 689.452 bytes XML; 1.512 ms externos e working set de pico 128.847.872 bytes do processo | [volume.log](volume.log) |
| pwsh -NoProfile -File docsplan/aacbs/scripts/Executar-AACBS004.ps1 -Offline -Dll saída Debug/net10.0/Unimake.Business.DFe.dll | Exit 0; journal, cache e ciclos das quatro famílias sintéticos, incluindo contextos JSON isolados do host | [roteiro-offline.log](roteiro-offline.log) |
| Mesmo build, -c INTEROP_Release | Exit 0; 15 avisos preexistentes, zero erros | [build-interop.log](build-interop.log) |
| Runner direto INTEROP_Release/net10.0, -trait DFe=AACBS | 436 testes, zero falhas/erros/ignorados | [testes-interop.log](testes-interop.log) |

Incremento: 84 casos Debug/89 INTEROP, nas três classes AACBSFluxoConsultasTest, AACBSArquivosConsultasTest e AACBSDarfTest. A [matriz](MATRIZ-ACEITE.md) relaciona as dez unidades aos testes e às provas reais. O teste de volume demonstra apenas a massa medida; o working set inclui o host/runner e não mede só a memória líquida do mapper. Não representa escala ilimitada.

Falhas corrigidas antes do real: primeiro build detectou uso de GerarXMLString, exclusivo COM, no teste Debug; substituído por geração DOM disponível nas duas compilações. A revisão detectou parâmetros diferentes da fixture na invocação ALC, busca de recursos pela pasta do PowerShell e expectativas que ecoavam nomes desconhecidos; corrigidos sem flexibilizar sanitização. AACBSFixture usa a pasta de sua assembly, mantendo o runner normal e o host isolado. Uma captura inicial do executor usou caminho de log incorreto; repetida corretamente, sem mudar fontes para contornar falha. Logs finais acima pertencem ao checkpoint estável.

Regressões compartilhadas/inspeção nativa COM da AACBS-003 não foram repetidas porque esses componentes não mudaram. Os testes AACBS anteriores foram repetidos com o incremento. Sem suíte integral, pacote, alteração de consumidores UniNFe/PRO, commits ou publicação.

## Agentes e revisão

Orquestrador: fontes, base/dispatcher/configuração nacional, roteiro de consultas, helper de fixtures, integração e evidências. aacbs004_consultas, aacbs004_darf e aacbs004_testes (gpt-6.2-sol/medium): respectivamente três consultas/arquivos, dois DARFs e testes novos. aacbs004_revisao (gpt-6.2-astra/high): revisão independente dos contratos, código e suporte local; sem bloqueios remanescentes no checkpoint revisado. aacbs002_executor reutilizado (gpt-6-luna/low): único executor de builds/runners e, depois do offline verde, das consultas reais autorizadas. Nenhum agente aprova etapa.

## Prova real e pendências

A rodada autorizada foi executada sequencialmente pelo único executor, após a prova offline. A verificação local passou sem exibir valores: [config-real.log](config-real.log). O ambiente permaneceu Beta, com DARF desabilitado. O cache precisou de uma emissão OAuth HTTP 200 e foi reutilizado pelas consultas.

| Família | Resultado efetivo | Limite / evidência |
|---|---|---|
| Créditos | Uma abertura HTTP 201; uma situação HTTP 200 Concluida; um link HTTP 200; um download HTTP 200, rejeitado em $.ni | [real-creditos.log](real-creditos.log); journal DownloadIndisponivel, sem XML operacional disponibilizado |
| Pagamentos | Uma tentativa de POST HTTP 404, corpo incompatível com contrato; sem tíquete confirmado | [real-pagamentos.log](real-pagamentos.log); AberturaAmbigua preservada, sem repetir POST |
| Recolhimentos | Uma tentativa de POST HTTP 404, corpo incompatível com contrato; sem tíquete confirmado | [real-recolhimentos.log](real-recolhimentos.log); AberturaAmbigua preservada, sem repetir POST |

Não houve 401/403/429. As linhas RespostaInterpretada e Concluida são observações do mesmo GET de situação; as linhas ContratoIncompativel e AberturaAmbigua são observações do mesmo POST. Contagem real da rodada: 1 emissão OAuth, 3 POSTs de abertura (um por família), 1 GET de situação, 1 de link e 1 de arquivo, total 7 requests. Não contar ocorrências textuais de HTTP como requests adicionais.

O orquestrador assumiu depois do encerramento do executor e realizou diagnóstico explícito do mesmo tíquete de créditos: somente 1 GET adicional de link e 1 GET de arquivo, sem autenticação, situação ou abertura. O [diagnóstico sanitizado](DIAGNOSTICO-CREDITOS.json) comprova ni string de 14 dígitos, igual à matriz e à raiz do pedido, tíquete correspondente e dois períodos de apuração. Original de 8.969 bytes cifrado por DPAPI, hash somente do artefato cifrado na evidência; journal inalterado. Reserva diagnóstica cifrada impede replay; script temporário removido. Total da etapa até aqui: 9 requests reais (inclui diagnóstico), sem DARF. Nenhum payload real foi versionado.

A [proposta NI de créditos](PROPOSTA-NI-CREDITOS.md) aguarda aprovação específica do DEV; o código ainda exige oito. Não estender a decisão de débitos por presunção. Validação de uma cópia alterada em memória, quando feita para qualificação, não representa sucesso do XML operacional original.

A [pesquisa dos 404](PESQUISA-404.md) confirma que verbo, host, rota, CNPJ e corpo coincidem com as fontes. A causa dos 404 permanece indeterminada. Publicações oficiais distinguem documentação de implantação, sem confirmação localizada da ativação dessas duas consultas no Beta. Não inferir falta de permissão/implantação nem inventar URL ou repetir o POST como sonda.

DARF RAD e PCONT reais aguardam dados para preparar cenários e confirmação específica do DEV antes de emitir. Implementação e testes simulados não equivalem a emissão real comprovada nem dispensam esse critério. A emissão habilitada por cenário não incluirá pagamento. AACBS-004 permanece em execução enquanto houver critérios reais obrigatórios pendentes, salvo exceção explícita do DEV. Não avançar para AACBS-005.

A [verificação independente offline](VERIFICACAO-CREDITOS-NI8.json) confirmou que o original é rejeitado em $.ni. Alterando SOMENTE esse campo para oito dígitos em cópia de memória, todos os demais campos ficaram equivalentes e o parser/XSD/conteúdo aceitaram dois períodos e 27 créditos. Zero requests, alterações de journal ou gravações de XML operacional; original cifrado intacto. Isso qualifica a proposta, sem validar operacionalmente o original de 14 dígitos nem aprovar a mudança.

## Condições de retomada

1. Resposta explícita do DEV à compatibilidade 8/14 somente em créditos. Se aprovada, aplicar o delta específico, acrescentar regressões e validar o original cifrado sem nova abertura; se mantido o contrato, aguardar adequação oficial.
2. Qualificar os HTTP 404 de pagamentos/recolhimentos com informação oficial ou habilitação aplicável. Não repetir os POSTs nem mudar host/rota por tentativa; estados ambíguos e cotas preservados.
3. Receber período PCONT, DF-e(s) RAD, opção total/parcial e valor quando parcial para preparar pedidos concretos. Apresentar esses pedidos ao DEV e obter sua confirmação antes de qualquer emissão. Nenhum DARF foi emitido ou pago.

O orquestrador abriu solicitações assíncronas separadas para os dados dos DARFs e a decisão NI; nenhuma resposta foi presumida por tempo decorrido. Não há novos testes reais programados enquanto esses critérios estiverem bloqueados. AACBS-005 continua PLANNED. Entrega técnica parcial disponível para inspeção, sem dispensar os critérios reais pendentes.
