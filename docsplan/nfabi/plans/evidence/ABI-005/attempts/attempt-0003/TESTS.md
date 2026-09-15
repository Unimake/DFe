# Unimake.DFe - NF-e ABI - Testes ABI-005

| Fase | Comando/cenário | Resultado |
|---|---|---|
| Fonte | snapshots SHA-256 dos 22 arquivos e 20 XSDs no Plan e Check | PASS: zero divergência |
| WSDL | HTTPS mTLS nos dois `?wsdl`, A1 carregado diretamente do PFX | PASS: HTTP 200; hashes registrados |
| Build DLL | `dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore` | PASS: 0 erros, 4 avisos preexistentes |
| Build INTEROP | mesmo projeto com `-p:DefineConstants=INTEROP` | PASS: 0 erros, 11 avisos preexistentes |
| Build testes | `dotnet build "source/Unimake.DFe.Test/Unimake.DFe.Test.csproj" --no-restore` | PASS: 0 erros, 24 avisos preexistentes |
| Configuração estadual | conjunto de UFs, XML parse e SHA-256 contra `NFGas/AC.xml` | PASS: 27/27; nenhum ausente/extra; todos com hash `756B6744...` |
| ABI-005 | runner xUnit v3, `ServicosPublicadosTest` | PASS: 41/41 |
| Regressão ABI-003 | runner xUnit v3, `SerializacaoNFeABITest` | PASS: 3/3 |
| Regressão ABI-004 | runner xUnit v3, `EventosNFeABITest` | PASS: 9/9 |
| Regressão schemas | runner xUnit v3, `SchemaFoundationTest` | PASS: 14/14 |
| Gate crítico | revisão independente final | PASS sem finding material remanescente |
| Plano/diff | plan-linter, `git diff --check`, controles, segredos e escopo | PASS |

Total focado: 67/67 testes. A primeira versão da nova asserção usou um hostname esperado incorreto; a expectativa foi corrigida para o endpoint exato já comprovado pelo WSDL e a execução final ficou verde. Nenhuma operação fiscal real foi transmitida; nenhum novo acesso externo foi necessário nesta tentativa e nenhum produto consumidor externo foi acessado, compilado ou testado.
