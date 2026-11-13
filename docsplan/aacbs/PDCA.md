# PDCA da integração AACBS

Este arquivo é a fonte única dos estados e do histórico das etapas. O escopo e os critérios de cada entrega ficam em [PLANO.md](PLANO.md). Aprovação técnica de agentes ou testes verdes não substituem a aprovação humana do DEV.

## Etapas

| Etapa | Entrega abrangente | Estado | Dependência |
|---|---|---|---|
| AACBS-000 | Planejamento, decisões, fontes iniciais, requisitos e estratégia de execução | DELIVERED_FOR_REVIEW | Pedido de planejamento do DEV |
| AACBS-001 | Qualificação das fontes e especificação dos contratos de todas as famílias, campo a campo; autenticação somente documental | PLANNED | AACBS-000 APPROVED |
| AACBS-002 | Classes XML, XSDs de envio e retorno, validação, integração com recursos da DLL e testes | PLANNED | AACBS-001 APPROVED |
| AACBS-003 | Infraestrutura, autenticação, configuração e transporte; primeiro fluxo completo de débitos | PLANNED | AACBS-002 APPROVED |
| AACBS-004 | Créditos, pagamentos e recolhimentos; cobertura integral dos endpoints e contratos do escopo | PLANNED | AACBS-003 APPROVED |
| AACBS-005 | Testes reais, INTEROP, pacote, documentação de uso, revisão e entrega final | PLANNED | AACBS-004 APPROVED |

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

Nenhuma aprovação humana está registrada nesta versão inicial. As demais etapas permanecem pendentes.
