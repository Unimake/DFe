# Unimake.DFe - NF-e ABI - Testes ABI-004

| Fase | Comando/cenário | Resultado |
|---|---|---|
| Fonte | snapshots SHA-256 dos 22 arquivos e dos 20 XSDs, no Plan e no Check | PASS: zero divergência contra o catálogo aprovado |
| Build DLL | `dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore` | PASS: 0 erros, 4 avisos preexistentes |
| Build INTEROP | `dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore -p:DefineConstants=INTEROP` | PASS: 0 erros, 11 avisos preexistentes |
| Build testes | `dotnet build "source/Unimake.DFe.Test/Unimake.DFe.Test.csproj" --no-restore` | PASS: 0 erros, 28 avisos preexistentes |
| ABI-004 | runner xUnit v3, classe `Unimake.DFe.Test.NFeABI.Serializacao.EventosNFeABITest` | PASS: 9/9 |
| Regressão ABI-003 | runner xUnit v3, classe `Unimake.DFe.Test.NFeABI.Serializacao.SerializacaoNFeABITest` | PASS: 3/3 |
| Regressão schema | runner xUnit v3, classe `Unimake.DFe.Test.NFeABI.Validacao.SchemaFoundationTest` | PASS: 14/14 |
| Contrato | quatro fixtures, retorno/processado, Id, condicionais e precisão | PASS |
| Plano | `pwsh -NoProfile -File .agents/skills/plan-linter/scripts/test-plan.ps1 -RepositoryRoot .` | PASS |
| Diff | `git diff --check`, escopo, controles/placeholders e revisão integral | PASS |
| Gate crítico | revisão independente e revisitas após correções | PASS sem finding material remanescente |

Não houve teste online, certificado, endpoint nem acesso a produto consumidor externo. A suíte integral não foi executada; os testes determinísticos focados cobrem o incremento e as fundações afetadas.
