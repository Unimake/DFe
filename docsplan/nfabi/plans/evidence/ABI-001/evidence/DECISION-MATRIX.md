# Matriz decisória e de impacto - ABI-001

| Item | Resolução | Evidência | Propagação | Contingência |
|---|---|---|---|---|
| DEC-001 / Q-SCOPE-001 | transporte somente para Status e Autorização publicados em homologação; consulta/eventos permanecem como XML | MOCs, catálogo e ausência de endpoints publicados | ABI-004 e ABI-005 | aguardar publicação e replanejar transporte |
| DEC-002 | produção sem endpoint | autorização DEV já registrada | ABI-005 | falhar fechado |
| DEC-003 / Q-PFX-001 | prefixo `ABI`, IDs candidatos `ABI-000` a `ABI-006`, sem renumeração | autorização nominal da ABI-001 | manifests, PDCA e mapa de IDs | congelamento efetivo somente após aprovação da ABI-001 |
| DEC-004 | fonte local obrigatória e releitura/hash em ABI-002+ | snapshots sem divergência | ABI-002 a ABI-006 | bloquear e replanejar se houver mudança |
| Q-API-001 | INTEROP nas classes públicas equivalentes; sem COM artificial para tipos internos | AGENTS e RISK-004 | ABI-003, ABI-004 e ABI-006 | reverter membro incompatível |
| DEC-005 / Q-SCOPE-002 | plano restrito à DLL Unimake.DFe e aos testes deste repositório | retrabalho explícito do DEV | ABI-002 a ABI-006 | tratar qualquer produto consumidor em planejamento separado |
| Q-TEST-001 | teste online opt-in e autorizado, nunca gate único | LIM-003 e AGENTS | ABI-005 | registrar limitação ambiental e manter gates offline |

Nenhuma decisão autoriza código de produto na ABI-001, aprovação da própria etapa ou início da ABI-002.
