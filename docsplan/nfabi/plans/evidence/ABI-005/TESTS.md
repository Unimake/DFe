# Unimake.DFe - NF-e ABI - Testes ABI-005

| Fase | Comando/cenário | Resultado |
|---|---|---|
| Fonte | snapshots SHA-256 dos 22 arquivos e 20 XSDs no Plan e Check | PASS: zero divergência |
| WSDL | HTTPS mTLS nos dois `?wsdl`, A1 carregado diretamente do PFX | PASS: HTTP 200; hashes registrados |
| Build DLL | `dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore` | PASS: 0 erros, 4 avisos preexistentes |
| Build INTEROP | mesmo projeto com `-p:DefineConstants=INTEROP` | PASS: 0 erros, 11 avisos preexistentes |
| Build testes | `dotnet build "source/Unimake.DFe.Test/Unimake.DFe.Test.csproj" --no-restore` | PASS: 0 erros, 28 avisos preexistentes |
| Configuração estadual | conjunto de UFs, XML parse e SHA-256 contra `NFGas/AC.xml` | PASS: 27/27; todos com hash `756B6744...` |
| ABI-005 offline | runner xUnit v3, `ServicosPublicadosTest` | PASS: 43/43 |
| Status homologação | runner xUnit v3, `StatusServicoTest` | PASS: 1/1; objeto tipado + `Executar()` |
| Autorização homologação | runner xUnit v3, `AutorizacaoSincTest` | PASS: 1/1; objeto tipado + `Executar()` |
| Regressão ABI-003 | runner xUnit v3, `SerializacaoNFeABITest` | PASS: 3/3 |
| Regressão ABI-004 | runner xUnit v3, `EventosNFeABITest` | PASS: 9/9 |
| Regressão schemas | runner xUnit v3, `SchemaFoundationTest` | PASS: 14/14 |
| Gate crítico | revisão independente final | PASS após correção do finding P2 |
| Plano/diff | plan-linter, `git diff --check`, controles, segredos e escopo | PASS |

Total focado: 71/71 testes. As primeiras chamadas reais expuseram `cUF=PR` nos retornos; após a compatibilidade centralizada, os testes online ficaram verdes. As regressões também cobrem XML prefixado e demonstram que o conteúdo bruto permanece inalterado. Não houve teste de produção, retry automático, acesso a produto consumidor externo ou execução da suíte integral.
