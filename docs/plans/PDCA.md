# PDCA

| Etapa | Incremento | Estado | Dependências | Ambiente |
|---|---|---|---|---|
| ABI-000 | base do planejamento | DELIVERED_FOR_REVIEW | nenhuma | ENV-PLAN |
| ABI-001 | revisão decisória e congelamento | PLANNED | ABI-000 APPROVED | ENV-PLAN |
| ABI-002 | schemas e fundação do tipo fiscal | PLANNED | ABI-001 APPROVED | ENV-DLL |
| ABI-003 | modelo XML principal e retornos | PLANNED | ABI-002 APPROVED | ENV-DLL |
| ABI-004 | modelos de eventos NF-e ABI | PLANNED | ABI-003 APPROVED | ENV-DLL |
| ABI-005 | serviços publicados de homologação | PLANNED | ABI-004 APPROVED | ENV-DLL |
| ABI-006 | compatibilidade pública e handoff ao UniNFe | PLANNED | ABI-005 APPROVED | ENV-INTEGRATED |

## Histórico

| Data | Etapa | De | Para | Motivo/autoridade | AttemptId |
|---|---|---|---|---|---|
| 2026-09-10 | ABI-000 | — | PLANNED | geração autorizada pelo DEV no pedido atual | attempt-0001 |
| 2026-09-10 | ABI-000 | PLANNED | IN_PROGRESS | descoberta e geração do plano | attempt-0001 |
| 2026-09-10 | ABI-000 | IN_PROGRESS | DELIVERED_FOR_REVIEW | pacote gerado e linter verde; aguarda DEV | attempt-0001 |
| 2026-09-10 | ABI-000 | DELIVERED_FOR_REVIEW | REWORK | retrabalho solicitado pelo DEV para corrigir placeholders, fontes, referências, hashes e dossiê | attempt-0002 |
| 2026-09-10 | ABI-000 | REWORK | IN_PROGRESS | auditoria integral e correção exclusiva dos artefatos de planejamento | attempt-0002 |
| 2026-09-10 | ABI-000 | IN_PROGRESS | DELIVERED_FOR_REVIEW | retrabalho concluído, fontes e hashes registrados, auditorias e linter verdes; aguarda DEV | attempt-0002 |
