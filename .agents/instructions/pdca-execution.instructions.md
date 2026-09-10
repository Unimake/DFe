# Execução PDCA

- Estados: PLANNED, IN_PROGRESS, BLOCKED, DELIVERED_FOR_REVIEW, REWORK e APPROVED.
- Somente o DEV marca APPROVED. Aprovação e início da sucessora são transições separadas.
- 000 e 001 alteram apenas planejamento. Preserve AttemptId, evidence, rollback e tentativas anteriores.
- Ao entregar, sincronize manifesto/PDCA, arquive o dossiê e pare antes da próxima etapa.
