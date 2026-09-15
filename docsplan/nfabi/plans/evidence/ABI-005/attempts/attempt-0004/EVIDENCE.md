# Unimake.DFe - NF-e ABI - Evidência ABI-005

- Etapa: ABI-005
- Status: DELIVERED_FOR_REVIEW
- AttemptId: attempt-0004
- Estado-base: ABI-004 `APPROVED`; tentativas anteriores preservadas em `attempts/attempt-0001` a `attempts/attempt-0003`
- Início: 2026-09-14T19:31:44-03:00
- Término: 2026-09-14T19:37:49-03:00
- Próxima etapa iniciada: NÃO

## Resultado

A DLL publica `StatusServico` e `AutorizacaoSinc` da NFeABI em homologação, com resolução normal para as 27 UFs. A `attempt-0004` adicionou testes de integração equivalentes aos do BPe: objetos tipados, certificado configurado e chamada real a `Executar()`. O endpoint devolveu `cUF=PR` nos dois serviços, embora o modelo/XSD use código numérico; a desserialização passou a normalizar a sigla somente em uma cópia interna, inclusive quando o XML usa prefixo, preservando `RetornoWSString` e `RetornoWSXML` brutos.

Produção continua sem endpoint e falha fechada. Consulta, eventos remotos e autorização ZIP não foram implementados.

## Fontes normativas

- Fonte local: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi` — 22/22 arquivos, SHA-256 agregado `0383F95D81140925C4EF91046C2D723CA9D94F477E138069F7202D79D9CCCAD8` no Plan e no Check.
- Schemas: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi\PL_NFeABI_1.00` — 20/20 arquivos, SHA-256 agregado `BA44B39981C4DF3860A6BE9AA7C0739904D664377EABE963AB52AC8C2E74AE40` no Plan e no Check.
- WSDL Status: HTTP 200, SHA-256 `33EEED91FD925E88C139E15C08B1CA135EA1A995C99FC1990ECD80F437EFCDAF`.
- WSDL Autorização: HTTP 200, SHA-256 `B7A2BF6692DA4A2A5C199332D86AF45DF67A28C57A5C8F920160B326DDD813C8`.

O A1 foi carregado diretamente do PFX autorizado pelo DEV para os acessos de homologação. Não houve seleção no repositório do Windows; senha, certificado e identidade não foram persistidos no repositório ou nas evidências.

## Validação

- Integridade das 27 UFs: PASS; conjunto completo e SHA-256 `756B6744B329648FC4BED2861815A099A8D2FA1BD6043A2597EB5F9F5D854FB1` idêntico ao modelo NFGas.
- Build DLL normal: PASS, 0 erros e 4 avisos preexistentes.
- Build DLL INTEROP: PASS, 0 erros e 11 avisos preexistentes.
- Build de testes: PASS, 0 erros e 28 avisos preexistentes.
- `ServicosPublicadosTest`: PASS, 43/43; inclui `cUF` textual com namespace prefixado e preservação do retorno bruto.
- Integração `StatusServicoTest`: PASS, 1/1 em homologação com `Executar()`.
- Integração `AutorizacaoSincTest`: PASS, 1/1 em homologação com `Executar()`.
- Regressões ABI-003, ABI-004 e schemas: PASS, 3/3, 9/9 e 14/14.
- Total focado: PASS, 71/71.
- Revisão crítica independente: PASS após correção do seletor de `cUF` para o namespace oficial e inclusão das regressões prefixadas.
- `git diff --check`, controles, segredos, escopo, linter e revisão integral: PASS.

## Limitações e parada

O teste de autorização transmite a massa mínima somente uma vez por execução; o PASS comprova transporte e desserialização tipada, não aprovação fiscal do documento. Nenhum teste ou endpoint de produção foi criado. A mensagem pós-build preexistente sobre `*Undefined*Build.bat` não alterou o exit code. Produtos consumidores externos não foram acessados por exclusão explícita deste plano.

ABI-005 aguarda revisão do DEV; não está aprovada e ABI-006 permanece `PLANNED`, não iniciada.
