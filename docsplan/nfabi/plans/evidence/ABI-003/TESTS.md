# Unimake.DFe - NF-e ABI - Testes ABI-003

| Fase | Comando/cenário | Resultado |
|---|---|---|
| Fonte | snapshots SHA-256 dos 22 arquivos e dos 20 XSDs, no Plan e no Check | PASS: zero divergência contra o catálogo aprovado |
| Build DLL | `dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore` | PASS: 0 erros, 4 avisos preexistentes |
| Build INTEROP | `dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore -p:DefineConstants=INTEROP` | PASS: 0 erros, 11 avisos preexistentes |
| Build testes | `dotnet build "source/Unimake.DFe.Test/Unimake.DFe.Test.csproj" --no-restore` | PASS: 0 erros, 28 avisos preexistentes |
| ABI-003 | runner xUnit v3, classe `Unimake.DFe.Test.NFeABI.Serializacao.SerializacaoNFeABITest` | PASS: 3/3 |
| Regressão schema | runner xUnit v3, classe `Unimake.DFe.Test.NFeABI.Validacao.SchemaFoundationTest` | PASS: 14/14 |
| Contrato XML | documento completo/simplificado, retorno/protocolo, consulta/status, `tMed=0`, chave e ponte COM | PASS |
| Plano | `pwsh -NoProfile -File .agents/skills/plan-linter/scripts/test-plan.ps1 -RepositoryRoot .` | PASS |
| Diff | `git diff --check`, varreduras de escopo/controles/placeholders e revisão integral | PASS |
| Gate crítico | revisão independente | PASS após correção dos findings |

Não houve teste online, certificado, endpoint nem acesso a produto externo. A suíte integral não foi executada; os testes determinísticos focados cobrem o incremento e a fundação de schema afetada.
