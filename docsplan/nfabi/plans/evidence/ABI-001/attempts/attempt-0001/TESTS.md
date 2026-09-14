# Unimake.DFe - NF-e ABI - Testes ABI-001

| Fase | Comando/cenário | Resultado |
|---|---|---|
| Fonte | SHA-256 individual e snapshots agregados dos 2 MOCs e 20 XSDs | PASS: zero divergência contra ABI-000 |
| XSD | parsing XML integral, raízes, namespaces e includes/imports | PASS: 20 de 20 arquivos válidos |
| Publicação | consulta ao Portal de Serviços e ao Portal de Documentos em 2026-09-14 | PASS: somente Status/Autorização em homologação; MOCs 1.00a e schemas 1.00 |
| Decisões | varredura de decisões e questões materiais `OPEN`/`PROPOSED` | PASS: nenhuma pendência material do DEV |
| Plano | `pwsh -NoProfile -File .agents/skills/plan-linter/scripts/test-plan.ps1 -RepositoryRoot .` | PASS: 7 etapas, prefixo ABI e 7 manifests |
| Diff | `git diff --check`, escopo e revisão integral do diff | PASS: somente planejamento; nenhuma alteração em `source/` |
| Gate | revisão independente em contexto distinto | PASS após sincronização final de readiness e rastreabilidade |

Build e testes do produto não foram executados porque ABI-001 proíbe alterações e validações de produto.
