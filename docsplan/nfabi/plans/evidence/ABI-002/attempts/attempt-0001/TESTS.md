# Unimake.DFe - NF-e ABI - Testes ABI-002

| Fase | Comando/cenário | Resultado |
|---|---|---|
| Fonte | snapshots SHA-256 dos 22 arquivos e dos 20 XSDs, no Plan e no Check | PASS: zero divergência contra ABI-000 |
| Cópia XSD | nomes, bytes e SHA-256 origem/destino | PASS: 20/20 idênticos |
| XSD | parsing XML e compilação com includes/imports locais | PASS: 20/20 |
| Build DLL | `dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore` | PASS: 0 erros, 4 avisos preexistentes |
| Build testes | `dotnet build "source/Unimake.DFe.Test/Unimake.DFe.Test.csproj" --no-restore` | PASS: 0 erros |
| ABI-002 | runner xUnit v3, classe `Unimake.DFe.Test.NFeABI.Validacao.SchemaFoundationTest` | PASS: 14/14 |
| Regressão schema | runner xUnit v3, classe `Unimake.DFe.Test.Utility.Validacao.ValidarSchemaTest` | PASS: 3/3 |
| Regressão NFe | runner xUnit v3, classe `Unimake.DFe.Test.NFe.Serializacao.MonofasiaTest` | PASS: 11/11 |
| Regressão detector | runner xUnit v3, classe `Unimake.DFe.Test.Utility.Xml.TipoXMLTest` | PASS: 14/14 |
| Plano | `pwsh -NoProfile -File .agents/skills/plan-linter/scripts/test-plan.ps1 -RepositoryRoot .` | PASS: raiz `docsplan/nfabi`, 7 etapas e 7 manifests |
| Diff | `git diff --check` e revisão integral do diff | PASS |
| Gate crítico | revisão independente | PASS após correção de todos os findings |

Não houve teste online, certificado ou produto consumidor externo: a etapa é uma fundação offline de schema/tipo/validação.
