# Unimake.DFe - NF-e ABI - Testes ABI-000

| Fase | Comando/cenário | Resultado |
|---|---|---|
| Plano | `pwsh -NoProfile -File .agents/skills/plan-linter/scripts/test-plan.ps1 -RepositoryRoot .` | PASS: 7 etapas e 7 manifests |
| Placeholders | varredura por expansões de template, aliases de raiz, placeholders e prefixos incorretos | PASS: nenhuma ocorrência nos artefatos do plano |
| Controles | varredura por `U+0000-U+0008`, `U+000B`, `U+000C`, `U+000E-U+001F` e `U+007F` | PASS: nenhuma ocorrência |
| Fontes | SHA-256 dos 2 MOCs e 20 XSDs e snapshots agregados | PASS: inventário reproduzido |
| Links | links locais pelo linter e consulta dos portais oficiais NF-e ABI | PASS |
| Diff | `git diff --check`, `git diff --stat` e revisão integral de `git diff` | PASS: somente planejamento |

Testes de produto não foram executados porque ABI-000 proíbe implementação e validação de produto.
