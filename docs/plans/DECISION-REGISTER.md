# Decisões

| ID | Decisão | Estado | Autoridade | Fonte/data | Necessária antes | Alternativas/recomendação | Impacto/fallback |
|---|---|---|---|---|---|---|---|
| DEC-001 | Implementar transporte apenas para serviços publicados; manter modelos de consulta/eventos | PROPOSED | DEV | MOC e portal 2026-09-10 | ABI-005 | recomendado: somente status/autorização remotos | evita URLs fictícias; futura etapa após publicação |
| DEC-002 | Produção fica sem endpoint | DECIDED | DEV | pedido 2026-09-10 | ABI-005 | não copiar homologação | fail-closed |
| DEC-003 | Prefixo ABI | PROPOSED | DEV | planejador 2026-09-10 | ABI-001 | ABI recomendado | pode renomear uma vez na 001 |
| DEC-004 | Pacote local é fonte de schemas e documentação deve ser revisitada | DECIDED | DEV | pedido 2026-09-10 | ABI-002 | copiar íntegro e comparar hash | divergência bloqueia/replaneja |
