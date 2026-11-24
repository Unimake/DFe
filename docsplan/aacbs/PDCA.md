# PDCA da integração AACBS

Este arquivo é a fonte única dos estados e do histórico das etapas. O escopo e os critérios de cada entrega ficam em [PLANO.md](PLANO.md). Aprovação técnica de agentes ou testes verdes não substituem a aprovação humana do DEV.

## Etapas

| Etapa | Entrega abrangente | Estado | Dependência |
|---|---|---|---|
| AACBS-000 | Planejamento e revisão das fontes, decisões, requisitos e estratégia de execução | APPROVED | Aprovação explícita do DEV em 24/11/2026 |
| AACBS-001 | Qualificação das fontes e especificação dos contratos de todas as famílias, campo a campo; autenticação somente documental | APPROVED | Aprovação explícita do DEV, incluindo datas e retorno global RAD |
| AACBS-002 | Classes XML, XSDs de envio e retorno, validação, integração com recursos da DLL e testes | DELIVERED_FOR_REVIEW | 24 raízes, 25 XSDs, 226 testes AACBS e 66 regressões aprovados; builds normal e INTEROP concluídos |
| AACBS-003 | Infraestrutura, autenticação, configuração e transporte; débitos com prova offline seguida de ciclo real | PLANNED | AACBS-002 APPROVED |
| AACBS-004 | Créditos, pagamentos, recolhimentos e DARF RAD/PCONT; cobertura integral e provas reais incrementais | PLANNED | AACBS-003 APPROVED |
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

AACBS-000 e AACBS-001 aprovadas explicitamente pelo DEV; tentativas anteriores preservadas. AACBS-002 entregue para revisão humana em [seu dossiê](evidencias/AACBS-002/tentativa-0001/ENTREGA.md). AACBS-003 a AACBS-005 permanecem pendentes. Nenhum token foi obtido ou API de negócio consumida; autenticação, transporte e harness pertencem à AACBS-003.
