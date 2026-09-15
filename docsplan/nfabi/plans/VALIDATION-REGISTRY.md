# Registro de validação

| Etapa | Runner/cenário reproduzível | EnvironmentId | Resultado esperado |
|---|---|---|---|
| ABI-000 | `pwsh -NoProfile -File .agents/skills/plan-linter/scripts/test-plan.ps1 -RepositoryRoot .` | ENV-PLAN | linter retorna 0 |
| ABI-001 | `pwsh -NoProfile -File .agents/skills/plan-linter/scripts/test-plan.ps1 -RepositoryRoot .` | ENV-PLAN | zero decisão material aberta e linter retorna 0 |
| ABI-002 | build DLL + classe focada de validação NFeABI | ENV-DLL | build verde e XSDs/fixtures validados |
| ABI-003 | build DLL + execução direta da classe de serialização NFeABI | ENV-DLL | round-trip InnerText, XPath e chave 44 dígitos verdes |
| ABI-004 | build DLL + classe focada de eventos NFeABI | ENV-DLL | três eventos e retornos validam/round-trip |
| ABI-005 | testes offline de configuração + build; smoke homologação opcional/autorizado | ENV-DLL | status/autorização resolvem URL correta; produção falha fechada |
| ABI-006 | build DLL e testes NFeABI focados | ENV-DLL | todos os gates determinísticos da DLL verdes |
