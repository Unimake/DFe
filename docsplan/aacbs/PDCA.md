# PDCA da integração AACBS

Este arquivo é a fonte única dos estados e do histórico das etapas. O escopo e os critérios de cada entrega ficam em [PLANO.md](PLANO.md). Aprovação técnica de agentes ou testes verdes não substituem a aprovação humana do DEV.

## Etapas

| Etapa | Entrega abrangente | Estado | Dependência |
|---|---|---|---|
| AACBS-000 | Planejamento e revisão das fontes, decisões, requisitos e estratégia de execução | APPROVED | Aprovação explícita do DEV em 24/11/2026 |
| AACBS-001 | Qualificação das fontes e especificação dos contratos de todas as famílias, campo a campo; autenticação somente documental | APPROVED | Aprovação explícita do DEV, incluindo datas e retorno global RAD |
| AACBS-002 | Classes XML, XSDs de envio e retorno, validação, integração com recursos da DLL e testes | APPROVED | Aprovação explícita do DEV após correções e nova revisão do ticket |
| AACBS-003 | Infraestrutura, autenticação, configuração e transporte; débitos com prova offline seguida de ciclo real | APPROVED | Aprovação explícita do DEV em 24/11/2026, liberando AACBS-004 |
| AACBS-004 | Créditos, pagamentos, recolhimentos e DARF RAD/PCONT; cobertura integral e provas reais incrementais | APPROVED | Aprovação explícita do DEV em 25/11/2026; limitações reais registradas para consolidação na 005 |
| AACBS-005 | Consolidação dos resultados reais, INTEROP, pacote, documentação de uso, revisão e entrega final | BLOCKED | RAD-01 autorizada e executada uma vez: HTTP 404, sem retorno fiscal validado; resultado indeterminado exige reconciliação. Demais pendências reais preservadas |

## Estados

| Estado | Significado |
|---|---|
| PLANNED | Pendente de execução; predecessora ainda não aprovada ou etapa ainda não iniciada |
| IN_PROGRESS | Executando somente o escopo liberado |
| BLOCKED | Condição externa ou decisão indispensável impede o trabalho necessário; motivo e retomada registrados |
| DELIVERED_FOR_REVIEW | Entrega e evidências disponíveis, aguardando revisão humana |
| REWORK | DEV solicitou correção ou revisão da entrega; abrir nova tentativa |
| APPROVED | DEV aprovou explicitamente a entrega e liberou a próxima etapa |

Fluxo usual: `PLANNED → IN_PROGRESS → DELIVERED_FOR_REVIEW → APPROVED`. Retrabalho segue `DELIVERED_FOR_REVIEW → REWORK → IN_PROGRESS`; bloqueio pode interromper a execução e volta a `IN_PROGRESS` quando sua condição for resolvida. Registrar todas as transições com motivo verificável.

## Gate humano e avanço

- Somente o DEV aprova uma etapa. O agente registra a aprovação citando a resposta real e a data; nunca preenche aprovação por presunção, silêncio ou resultado automático.
- Conforme decisão do DEV, aprovar a etapa N libera executar N+1. Registrar aprovação e início em transições distintas, sem solicitar uma segunda autorização ritual.
- Entregar N não libera N+1. Nenhum agente começa arquivos, implementação ou testes da sucessora antes da resposta humana que aprova N.
- O início da sucessora também depende dos requisitos técnicos da etapa. Se faltar fonte, contrato ou ambiente indispensável, registrar o bloqueio e a condição de retomada.
- Dentro da etapa já liberada, o orquestrador pode executar testes offline e reais previstos, corrigir falhas e repetir os checks afetados dentro do ambiente, operações e orçamento definidos, sem confirmação por chamada. Isso não antecipa a sucessora. Disponibilizar credenciais não equivale a aprovar uma etapa.
- Testes reais ficam limitados à empresa e ao ambiente explicitamente configurados, sem fallback para produção. DARF RAD/PCONT exige cenário explicitamente habilitado e não inclui efetuar pagamento. Os limites e a retomada são coordenados pelo executor, com dados operacionais locais ignorados pelo Git.
- Achados técnicos posteriores exigem correção e revalidação do que foi afetado. Não reescrever resultados históricos nem transformar uma validação não executada em aprovação.
- Uma aprovação de AACBS-005 encerra este plano. Publicação NuGet e expansão IBS dependem de escopo e autorização próprios.

## Evidências e tentativas

Cada etapa usa `evidencias/<etapa>/tentativa-NNNN/ENTREGA.md`, seguindo [EVIDENCE-TEMPLATE.md](EVIDENCE-TEMPLATE.md). A primeira tentativa é `tentativa-0001`. Retrabalho abre o próximo número e preserva a entrega anterior. O dossiê identifica a base, os arquivos testados, os resultados efetivos e a aprovação humana quando recebida.

## Histórico

| Data | Etapa | De | Para | Motivo e autoridade | Tentativa |
|---|---|---|---|---|---|
| 2026-11-12 | AACBS-000 | — | PLANNED | DEV solicitou plano, etapas com aprovação humana, evidências e agentes para a integração | tentativa-0001 |
| 2026-11-12 | AACBS-000 | PLANNED | IN_PROGRESS | Investigação documental e do repositório concluída para iniciar a consolidação do plano após a entrevista | tentativa-0001 |
| 2026-11-12 | AACBS-000 | IN_PROGRESS | DELIVERED_FOR_REVIEW | Plano consolidado, revisões cruzadas e correção integradas; 10 documentos e 27 links locais conferidos; dossiê disponível | tentativa-0001 |
| 2026-11-24 | AACBS-000 | DELIVERED_FOR_REVIEW | REWORK | DEV solicitou rever o plano porque a documentação mudou e definir onde fornecer as chaves para futuros testes autônomos; não houve aprovação da AACBS-000 | tentativa-0002 |
| 2026-11-24 | AACBS-000 | REWORK | IN_PROGRESS | Revisão documental autorizada pelo pedido atual: inventário de dez serviços, testes reais incrementais, configuração local e agentes; nenhuma etapa 001+ iniciada | tentativa-0002 |
| 2026-11-24 | AACBS-000 | IN_PROGRESS | DELIVERED_FOR_REVIEW | Fontes e plano atualizados; preparo de credenciais DPAPI, revisões cruzadas e verificações sintéticas concluídos; 12 Markdown e 45 links locais conferidos | tentativa-0002 |
| 2026-11-24 | AACBS-000 | DELIVERED_FOR_REVIEW | APPROVED | DEV: "Aprovo a AACBS-000 e libero a execução da AACBS-001." | tentativa-0002 |
| 2026-11-24 | AACBS-001 | PLANNED | IN_PROGRESS | Execução documental da AACBS-001 liberada na mesma mensagem; configuração local conferida pelo modo -Verificar, exit code 0, sem valores ou requests | tentativa-0001 |
| 2026-11-24 | AACBS-001 | IN_PROGRESS | DELIVERED_FOR_REVIEW | Especificação e matrizes concluídas: 23 fontes com hashes, 269 registros, 24 raízes e dez serviços; revisão independente e verificações documentais concluídas; dossiê disponível | tentativa-0001 |
| 2026-11-24 | AACBS-001 | DELIVERED_FOR_REVIEW | APPROVED | DEV: "Aprovo a AACBS-001 e libero a execução da AACBS-002. Pode seguir com as propostas documentadas para as datas e o retorno global do DARF RAD." | tentativa-0001 |
| 2026-11-24 | AACBS-002 | PLANNED | IN_PROGRESS | Implementação offline dos modelos, XSDs, catálogo e validação liberada pela mesma resposta; sem autenticação ou transporte de negócio | tentativa-0001 |
| 2026-11-24 | AACBS-002 | IN_PROGRESS | DELIVERED_FOR_REVIEW | Modelos, schemas, catálogo e validação entregues; checkpoint de 94 arquivos, 226 testes AACBS e 66 regressões sem falhas, build INTEROP aprovado e revisão independente sem pendências materiais | tentativa-0001 |
| 2026-11-24 | AACBS-002 | DELIVERED_FOR_REVIEW | REWORK | DEV solicitou analisar e corrigir os achados da revisão do ticket ID #189855 | tentativa-0002 |
| 2026-11-24 | AACBS-002 | REWORK | IN_PROGRESS | Correções dos contratos XML e regressões offline; AACBS-003 não iniciada | tentativa-0002 |
| 2026-11-24 | AACBS-002 | IN_PROGRESS | DELIVERED_FOR_REVIEW | Quatro achados corrigidos: 239 testes Debug, 241 INTEROP e 66 regressões aprovados; builds sem erros | tentativa-0002 |
| 2026-11-24 | AACBS-002 | DELIVERED_FOR_REVIEW | APPROVED | DEV: Aprovo a AACBS-002 e libero a execução da AACBS-003. Pode seguir conforme o plano. | tentativa-0002 |
| 2026-11-24 | AACBS-003 | PLANNED | IN_PROGRESS | Execução do fluxo de débitos, autenticação e infraestrutura liberada na mesma mensagem | tentativa-0001 |
| 2026-11-24 | AACBS-003 | IN_PROGRESS | BLOCKED | Cinco serviços e roteiro implementados; 313 testes Debug, 315 INTEROP, 92 regressões e roteiro offline aprovados. Pesquisa oficial não qualificou rota PRR; acesso local verificado sem requests. Consumo real aguarda URL oficial ou seleção explícita do DEV por Beta | tentativa-0001 |
| 2026-11-24 | AACBS-003 | BLOCKED | IN_PROGRESS | DEV: "Autorizo os testes em Produção Beta. Vamos testar tudo, na dúvida vai me perguntando." Metadados locais atualizados para Beta, credencial preservada; autenticação e fluxo de débitos dentro dos limites já definidos, sem liberar etapa 004 ou emissão de DARF | tentativa-0001 |
| 2026-11-24 | AACBS-003 | IN_PROGRESS | DELIVERED_FOR_REVIEW | Ciclo real Beta finalizado: autenticação 200, uma abertura 201, situação concluída, link/download 200 e XML real cifrado validado; apuração vazia. Desvio NI 14 aprovado e corrigido com 338 testes Debug/340 INTEROP; 92 regressões de transporte válidas e roteiro offline aprovados. Aguardando revisão humana, sem iniciar 004 | tentativa-0001 |
| 2026-11-24 | AACBS-003 | DELIVERED_FOR_REVIEW | REWORK | DEV solicitou analisar e corrigir os quatro problemas da revisão: endpoints/HTTPS, interface COM de Configuracao, preservação de CR nos getters COM e switches falsos do roteiro | tentativa-0002 |
| 2026-11-24 | AACBS-003 | REWORK | IN_PROGRESS | Correções delimitadas e testes offline; sem novas chamadas reais, sem acesso às credenciais e sem iniciar 004 | tentativa-0002 |
| 2026-11-24 | AACBS-003 | IN_PROGRESS | DELIVERED_FOR_REVIEW | Quatro achados corrigidos: 341 testes Debug, 344 INTEROP, 132 regressões e roteiro offline aprovados; ITypeInfo nativo confirmado e 119 hashes estáveis. Sem novas chamadas reais; aguarda DEV | tentativa-0002 |
| 2026-11-24 | AACBS-003 | DELIVERED_FOR_REVIEW | REWORK | DEV solicitou corrigir o achado da nova revisão: falha da autenticação auxiliar atribuída à consulta de débitos e campo scope vazio sem caminho específico | tentativa-0003 |
| 2026-11-24 | AACBS-003 | REWORK | IN_PROGRESS | Correção de contexto OAuth e testes controlados; sem chamadas reais ou início da 004 | tentativa-0003 |
| 2026-11-24 | AACBS-003 | IN_PROGRESS | DELIVERED_FOR_REVIEW | Achado OAuth corrigido, 3 regressões novas, 344 testes Debug/347 INTEROP aprovados e 119 hashes estáveis; revisão independente concluída, sem novas chamadas reais | tentativa-0003 |
| 2026-11-24 | AACBS-003 | DELIVERED_FOR_REVIEW | APPROVED | DEV: Aprovo a AACBS-003 e libero a execução da AACBS-004. Consultas reais Beta autorizadas; confirmar cenário/parâmetros antes de qualquer DARF | tentativa-0003 |
| 2026-11-24 | AACBS-004 | PLANNED | IN_PROGRESS | Implementação das demais famílias e dois DARFs, prova offline seguida de consultas reais Beta. Emissão DARF ainda não autorizada por cenário | tentativa-0001 |
| 2026-11-24 | AACBS-004 | IN_PROGRESS | BLOCKED | 428 testes Debug/436 INTEROP e roteiro offline aprovados. Créditos concluídos remotamente mas NI 14 diverge do contrato 8; original cifrado e proposta aguardam DEV. Pagamentos/recolhimentos deram 404, sem replay. DARFs aguardam dados e confirmação específica antes de emissão; 005 não iniciada | tentativa-0001 |
| 2026-11-25 | AACBS-004 | BLOCKED | REWORK | DEV solicitou analisar e corrigir os três achados da revisão das alterações pendentes | tentativa-0002 |
| 2026-11-25 | AACBS-004 | REWORK | IN_PROGRESS | Correção de resolução do checkout, filtro OAuth e testes de webhook; sem chamadas reais e sem presumir decisões pendentes | tentativa-0002 |
| 2026-11-25 | AACBS-004 | IN_PROGRESS | BLOCKED | Três achados corrigidos: 21 testes Debug/26 INTEROP da classe afetada e roteiro offline passaram; revisão independente e 131 hashes + 2 externos estáveis. Sem chamadas reais. Bloqueios anteriores de NI, HTTP 404 e cenários DARF preservados | tentativa-0002 |
| 2026-11-25 | AACBS-004 | BLOCKED | APPROVED | DEV: Aprovo a AACBS-004 e libero a execução da AACBS-005. Limitações reais permanecem documentadas; não há confirmação específica de NI ou cenário de emissão DARF nessa mensagem | tentativa-0002 |
| 2026-11-25 | AACBS-005 | PLANNED | IN_PROGRESS | Consolidação, documentação de uso, smoke tests, compatibilidade e empacotamento oficial DryRun; nenhuma nova abertura/emissão por iniciativa da consolidação | tentativa-0001 |

AACBS-000, AACBS-001 e AACBS-002 aprovadas explicitamente pelo DEV; tentativas anteriores preservadas. AACBS-003 aprovada pelo DEV após revisão e correções. A aprovação pontual do NI 8/14 foi uma decisão de contrato e não substitui a aprovação da etapa. AACBS-004 foi aprovada pelo DEV em 25/11/2026, e AACBS-005 foi entregue para revisão humana; as limitações reais permanecem documentadas. Os resultados reais e a limitação da massa vazia estão no [dossiê](evidencias/AACBS-003/tentativa-0001/ENTREGA.md); a [pesquisa dos ambientes](evidencias/AACBS-003/tentativa-0001/PESQUISA-AMBIENTES.md) conserva a limitação da rota PRR.

O [retrabalho da AACBS-003](evidencias/AACBS-003/tentativa-0002/ENTREGA.md) consolida as quatro correções anteriores e sua validação offline. A prova real da tentativa-0001 permanece histórica; não houve novo consumo real no retrabalho.

A entrega corrente está na [tentativa-0003](evidencias/AACBS-003/tentativa-0003/ENTREGA.md): correção do diagnóstico OAuth auxiliar e testes controlados concluídos; aprovada explicitamente pelo DEV. Não houve novas chamadas reais.

Na tentativa-0001, a AACBS-004 alcançou o limite dos testes reais então qualificáveis. A [entrega parcial e condições de retomada](evidencias/AACBS-004/tentativa-0001/ENTREGA.md) registra os impedimentos daquela rodada; a aprovação posterior está no histórico abaixo. Nenhuma emissão DARF ou repetição de abertura foi executada.

As [correções da revisão AACBS-004](evidencias/AACBS-004/tentativa-0002/ENTREGA.md) foram concluídas offline em 25/11/2026. Essas correções, por si, não dispensaram as condições reais da tentativa-0001; a aprovação posterior pertence ao DEV e está registrada abaixo.

O DEV aprovou expressamente a AACBS-004 em 25/11/2026 e liberou a AACBS-005. Os registros históricos de bloqueio foram preservados. As limitações reais são transferidas à matriz final, sem declarar testes não executados como aprovados; decisões específicas pendentes continuam identificadas.

Decisão específica recebida em 25/11/2026 durante AACBS-005: DEV aprovou NI 8/14 somente no retorno de créditos, preservando o valor integral e a raiz do pedido. Implementação e validação do original cifrado pertencem à tentativa-0001 da AACBS-005; essa decisão não autoriza emissões DARF.

| Data | Etapa | De | Para | Registro | Tentativa |
|---|---|---|---|---|---|
| 2026-11-25 | AACBS-005 | IN_PROGRESS | DELIVERED_FOR_REVIEW | NI 8/14 de créditos aprovado e original integral validado: 2 PAs/27 créditos, XML cifrado/journal finalizado, zero HTTP novo. 499 testes Debug/509 INTEROP, COM nativo dez operações, pacote oficial DryRun e revisão independente concluídos. Pagamentos/recolhimentos 404 e DARFs sem cenário confirmado permanecem lacunas reais, submetidas ao DEV | tentativa-0001 |

A [entrega da AACBS-005](evidencias/AACBS-005/tentativa-0001/ENTREGA.md), o [guia de uso](uso/README.md) e a [matriz final](evidencias/AACBS-005/tentativa-0001/MATRIZ-ACEITE-FINAL.md) estão disponíveis para revisão humana. Nenhum critério real pendente foi contado como sucesso. A aprovação pertence exclusivamente ao DEV; emissão real DARF continua sujeita à confirmação do cenário e dos parâmetros.

## Orientação posterior de ambiente — 25/11/2026

Após a [pesquisa aprofundada de PRR](evidencias/PESQUISA-PRR-2026-11-25/RESULTADO.md), o DEV determinou registrá-la no plano, continuar da forma atual e retomá-la depois. A execução permanece em Produção Beta dentro das autorizações existentes; PRR fica adiada, com responsável técnico e ponto de retomada no dossiê. A configuração local e a credencial foram preservadas; a pesquisa não gerou token nem chamada fiscal.

Este registro é uma orientação de continuidade, sem transição de aceite: AACBS-005 permanece DELIVERED_FOR_REVIEW. As lacunas reais de pagamentos/recolhimentos e DARF continuam identificadas. Emissão RAD/PCONT exige confirmação específica do cenário e dos parâmetros; não repetir as aberturas ambíguas apenas porque PRR foi adiada.

## Retomada autorizada da AACBS-005 — 25/11/2026

| Data | Etapa | De | Para | Registro | Tentativa |
|---|---|---|---|---|---|
| 2026-11-25 | AACBS-005 | DELIVERED_FOR_REVIEW | IN_PROGRESS | DEV autorizou investigar HTTP 404 de pagamentos/recolhimentos e preparar cenários reais RAD/PCONT. Antes de qualquer emissão, confirmar parâmetros com o DEV. Beta mantida; PRR adiada; tentativa anterior preservada | tentativa-0002 |

A [tentativa-0002](evidencias/AACBS-005/tentativa-0002/ENTREGA.md) concentra a retomada; a entrega técnica anterior permanece válida no checkpoint registrado, sem transformar suas lacunas reais em sucesso.
| Data | Etapa | De | Para | Registro | Tentativa |
|---|---|---|---|---|---|
| 2026-11-25 | AACBS-005 | IN_PROGRESS | DELIVERED_FOR_REVIEW | Rotas/contratos dos 404 conferidos sem defeito demonstrado do cliente; causa remota/tíquetes indeterminados. RAD-01 cifrado preparado, sem emissão e sem aprovação específica; PCONT sem saldo, DEV obterá outra empresa. Cinco novos testes Debug e cinco INTEROP aprovados, revisão independente e 143 hashes estáveis; zero chamadas fiscais | tentativa-0002 |

A [tentativa-0002](evidencias/AACBS-005/tentativa-0002/ENTREGA.md) e sua [matriz](evidencias/AACBS-005/tentativa-0002/MATRIZ-RETOMADA.md) registram o avanço possível enquanto a massa real é providenciada. A aprovação da etapa e a confirmação de emissão continuam distintas e pertencem ao DEV.
## RAD-01 — confirmação e execução pontual em 25/11/2026

| Data | Etapa | De | Para | Registro | Tentativa |
|---|---|---|---|---|---|
| 2026-11-25 | AACBS-005 | DELIVERED_FOR_REVIEW | IN_PROGRESS | DEV confirmou os parâmetros privados e ser adquirente; autorizou uma tentativa RAD-01 total em Beta vinculada ao hash D663880BB7351232C3896C18BD5D60F16B1A42E41D3C259F3E6294F74056F4D1 | tentativa-0003 |
| 2026-11-25 | AACBS-005 | IN_PROGRESS | BLOCKED | Autenticação aceita, uma execução RAD recebeu HTTP 404 sem XML/código fiscal validado. Journal ResultadoIndeterminado bloqueia nova tentativa; registros anteriores preservados, sem pagamento ou replay. Reconciliação/ativação/habilitação dependem de esclarecimento da Receita | tentativa-0003 |

A [tentativa-0003](evidencias/AACBS-005/tentativa-0003/ENTREGA.md) registra a autorização e o resultado. Não há confirmação de geração nem prova de ausência de processamento. Não repetir a RAD-01 ou tratar uma nova aprovação como reconciliação do envio anterior. PCONT segue sem cenário elegível e PRR permanece adiada.
## Consolidação documental entregue — 25/11/2026

O DEV solicitou preparar o encerramento técnico e a comunicação à Receita, sem chamadas, novas emissões ou envio externo. Foram entregues o [encerramento com ressalvas](ENCERRAMENTO-TECNICO.md), a [solicitação consolidada](encaminhamento-receita/SOLICITACAO-CONSOLIDADA.md) e o [guia de encaminhamento/anexos](encaminhamento-receita/README.md). O DEV é o responsável por encaminhar e retornar o protocolo/resposta para análise técnica. Não existe protocolo recebido ou resposta da Receita nesta consolidação.

**Estado operacional mantido: BLOCKED.** A consolidação está disponível para revisão técnica/humana, sem transição para APPROVED nem declaração de validação real integral. Retomada condicionada à reconciliação da RAD-01, esclarecimento das duas aberturas de consulta e/ou massa elegível PCONT. A [matriz de condições e responsáveis](ENCERRAMENTO-TECNICO.md#condições-de-retomada-e-responsáveis) distingue recuperação de documento existente, abertura nova e eventual nova emissão sujeita à confirmação específica. Os registros antigos, cotas e bloqueios permanecem preservados.
## Solicitação enviada pelo DEV — 25/11/2026

Após a revisão do texto para até 5.000 caracteres, o DEV informou: "Enviado, agora temos que aguardar." Encaminhamento registrado como enviado pelo DEV, aguardando resposta; protocolo, horário e canal efetivamente utilizado ainda não informados. Não houve envio pelo Codex, nova chamada fiscal, geração ou alteração dos journals. Estado AACBS-005 mantido BLOCKED nos critérios reais. O envio não equivale à reconciliação ou aprovação de nova tentativa.