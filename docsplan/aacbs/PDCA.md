# PDCA da integração AACBS

Este arquivo é a fonte única dos estados e do histórico das etapas. O escopo e os critérios de cada entrega ficam em [PLANO.md](PLANO.md). Aprovação técnica de agentes ou testes verdes não substituem a aprovação humana do DEV.

## Etapas

| Etapa | Entrega abrangente | Estado | Dependência |
|---|---|---|---|
| AACBS-000 | Planejamento e revisão das fontes, decisões, requisitos e estratégia de execução | APPROVED | Aprovação explícita do DEV em 24/11/2026 |
| AACBS-001 | Qualificação das fontes e especificação dos contratos de todas as famílias, campo a campo; autenticação somente documental | APPROVED | Aprovação explícita do DEV, incluindo datas e retorno global RAD |
| AACBS-002 | Classes XML, XSDs de envio e retorno, validação, integração com recursos da DLL e testes | APPROVED | Aprovação explícita do DEV após correções e nova revisão do ticket |
| AACBS-003 | Infraestrutura, autenticação, configuração e transporte; débitos com prova offline seguida de ciclo real | APPROVED | Aprovação explícita do DEV em 24/11/2026, liberando AACBS-004 |
| AACBS-004 | Créditos, pagamentos, recolhimentos e DARF RAD/PCONT; cobertura integral e provas reais incrementais | BLOCKED | Três achados corrigidos e validados offline; pendências reais de NI, HTTP 404 e cenários DARF preservadas |
| AACBS-005 | Consolidação dos resultados reais, INTEROP, pacote, documentação de uso, revisão e entrega final | PLANNED | AACBS-004 APPROVED |

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

AACBS-000, AACBS-001 e AACBS-002 aprovadas explicitamente pelo DEV; tentativas anteriores preservadas. AACBS-003 aprovada pelo DEV após revisão e correções. A aprovação pontual do NI 8/14 foi uma decisão de contrato e não substitui a aprovação da etapa. AACBS-004 está bloqueada nas condições documentadas; AACBS-005 permanece pendente. Os resultados reais e a limitação da massa vazia estão no [dossiê](evidencias/AACBS-003/tentativa-0001/ENTREGA.md); a [pesquisa dos ambientes](evidencias/AACBS-003/tentativa-0001/PESQUISA-AMBIENTES.md) conserva a limitação da rota PRR.

O [retrabalho da AACBS-003](evidencias/AACBS-003/tentativa-0002/ENTREGA.md) consolida as quatro correções anteriores e sua validação offline. A prova real da tentativa-0001 permanece histórica; não houve novo consumo real no retrabalho.

A entrega corrente está na [tentativa-0003](evidencias/AACBS-003/tentativa-0003/ENTREGA.md): correção do diagnóstico OAuth auxiliar e testes controlados concluídos; aprovada explicitamente pelo DEV. Não houve novas chamadas reais.

AACBS-004 alcançou o limite dos testes reais atualmente qualificáveis. A [entrega parcial e condições de retomada](evidencias/AACBS-004/tentativa-0001/ENTREGA.md) registram os impedimentos e as perguntas pendentes ao DEV. A etapa não está aprovada nem concluída; nenhuma emissão DARF ou repetição de abertura foi executada.

As [correções da revisão AACBS-004](evidencias/AACBS-004/tentativa-0002/ENTREGA.md) foram concluídas offline em 25/11/2026. Isso não dispensa as condições reais da tentativa-0001 nem aprova a etapa.
