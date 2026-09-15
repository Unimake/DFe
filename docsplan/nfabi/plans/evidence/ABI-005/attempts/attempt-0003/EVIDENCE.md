# Unimake.DFe - NF-e ABI - Evidência ABI-005

- Etapa: ABI-005
- Status: DELIVERED_FOR_REVIEW
- AttemptId: attempt-0003
- Estado-base: ABI-004 `APPROVED`; tentativa bloqueada preservada em `attempts/attempt-0001`
- Início: 2026-09-14T18:30:00-03:00
- Término: 2026-09-14T18:45:17-03:00
- Próxima etapa iniciada: NÃO

## Resultado

A DLL agora publica `StatusServico` e `AutorizacaoSinc` da NFeABI com resolução normal por UF. Foram adicionados os 27 arquivos estaduais usados pelo padrão NFGas; cada arquivo é uma cópia byte a byte de `Servicos/Config/NFGas/AC.xml` e herda de `SVRS.xml`. O contrato usa SOAP 1.2, wrappers, elementos de retorno, SOAPActions e endpoints de homologação exatamente como publicados. Produção permanece sem endpoint e falha fechada. Consulta, eventos remotos e autorização ZIP não foram implementados.

## Fontes normativas

- Fonte local: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi` — 22/22 arquivos, SHA-256 agregado `0383F95D81140925C4EF91046C2D723CA9D94F477E138069F7202D79D9CCCAD8` no Plan e no Check.
- Schemas: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi\PL_NFeABI_1.00` — 20/20 arquivos, SHA-256 agregado `BA44B39981C4DF3860A6BE9AA7C0739904D664377EABE963AB52AC8C2E74AE40` no Plan e no Check.
- WSDL Status: HTTP 200, SHA-256 `33EEED91FD925E88C139E15C08B1CA135EA1A995C99FC1990ECD80F437EFCDAF`.
- WSDL Autorização: HTTP 200, SHA-256 `B7A2BF6692DA4A2A5C199332D86AF45DF67A28C57A5C8F920160B326DDD813C8`.

O A1 foi carregado diretamente do PFX autorizado pelo DEV apenas para obter os WSDLs. Não houve seleção de certificado cliente no repositório do Windows; senha, certificado e identidade não foram persistidos no repositório ou nas evidências.

## Validação

- Integridade das 27 UFs: PASS; conjunto completo, nenhum arquivo extra e SHA-256 `756B6744B329648FC4BED2861815A099A8D2FA1BD6043A2597EB5F9F5D854FB1` idêntico ao modelo NFGas em todos os arquivos.
- Build DLL normal: PASS, 0 erros e 4 avisos preexistentes.
- Build DLL INTEROP: PASS, 0 erros e 11 avisos preexistentes.
- Build de testes: PASS, 0 erros e 24 avisos preexistentes.
- `ServicosPublicadosTest`: PASS, 41/41, incluindo resolução individual das 27 UFs e igualdade do conteúdo embutido.
- Regressões ABI-003, ABI-004 e schemas: PASS, 3/3, 9/9 e 14/14.
- Revisão crítica independente: PASS após correção dos findings de reuso, chave de protocolo, preservação do XML bruto e `ConteudoXMLOriginal`.
- `git diff --check`, controles, segredos, escopo, linter e revisão integral: PASS.

## Cobertura entregue

Os testes confrontam a configuração diretamente com os WSDLs arquivados e cobrem as 27 UFs, conteúdo embutido idêntico ao modelo NFGas, endpoints/actions/wrapper/result, produção sem URL, retornos de sucesso e rejeição, retorno vazio, processado autorizado, protocolo de outra chave, reuso da instância, proxy, ausência de certificado, assinatura offline com certificado efêmero e preservação do XML bruto no construtor textual.

## Limitações e parada

Não foi transmitida NFeABI real. O acesso online foi restrito à leitura dos WSDLs autorizada pelo DEV. A mensagem pós-build preexistente sobre `*Undefined*Build.bat` não alterou o exit code. ABI-005 aguarda revisão do DEV; não está aprovada e ABI-006 permanece `PLANNED`, não iniciada.
