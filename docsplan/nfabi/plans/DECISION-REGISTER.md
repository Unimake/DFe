# Decisões

| ID | Decisão | Estado | Autoridade | Fonte/data | Necessária antes | Alternativas/recomendação | Impacto/fallback |
|---|---|---|---|---|---|---|---|
| DEC-001 | Implementar transporte apenas para serviços publicados; manter modelos de consulta/eventos | DECIDED | DEV | MOCs, portal oficial reconferido e execução autorizada da ABI-001 em 2026-09-14 | ABI-005 | somente Status e Autorização em homologação; consulta/eventos permanecem como contratos XML | serviços sem endpoint ficam sem transporte até publicação oficial |
| DEC-002 | Produção fica sem endpoint | DECIDED | DEV | pedido 2026-09-10 | ABI-005 | não copiar homologação | fail-closed |
| DEC-003 | Prefixo ABI | DECIDED | DEV | autorização explícita de `ABI-001` em 2026-09-14 | ABI-001 | manter `ABI` e os IDs `ABI-000` a `ABI-006` | nenhuma renumeração; congelamento efetivo no gate de aprovação da ABI-001 |
| DEC-004 | Pacote local é fonte de schemas e documentação deve ser revisitada | DECIDED | DEV | pedido 2026-09-10 | ABI-002 | copiar íntegro e comparar hash | divergência bloqueia/replaneja |
| DEC-005 | Restringir integralmente o plano NF-e ABI à DLL Unimake.DFe | DECIDED | DEV | retrabalho explícito da ABI-001 em 2026-09-14 | ABI-001 | nenhum produto consumidor, integração externa ou UI entra neste plano | qualquer integração será tratada em planejamento separado |
