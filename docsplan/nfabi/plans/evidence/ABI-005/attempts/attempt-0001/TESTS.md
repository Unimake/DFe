# Unimake.DFe - NF-e ABI - Testes ABI-005 attempt-0001

| Fase | Comando/cenário | Resultado |
|---|---|---|
| Fonte | snapshots SHA-256 dos 22 arquivos e dos 20 XSDs | PASS: zero divergência |
| Catálogo | portal oficial de serviços | PASS: dois endpoints de homologação confirmados |
| WSDL Schannel | `curl.exe` para Status e Autorização com `?wsdl` | BLOCKED: `SEC_E_ILLEGAL_MESSAGE` |
| WSDL OpenSSL | cliente HTTPS Node.js para Status | BLOCKED: alerta TLS 40 |
| Protótipo temporário | build DLL normal | PASS: 0 erros, 4 avisos preexistentes |
| Protótipo temporário | build DLL INTEROP | PASS: 0 erros, 11 avisos preexistentes |
| Protótipo temporário | build de testes | PASS: 0 erros, avisos preexistentes |
| Protótipo temporário | `ServicosPublicadosTest` | PASS: 9/9, porém autorreferente para metadados wire |
| Regressão | `SerializacaoNFeABITest` | PASS: 3/3 |
| Regressão | `EventosNFeABITest` | PASS: 9/9 |
| Regressão | `SchemaFoundationTest` | PASS: 14/14 |
| Gate crítico | revisão independente | FAIL: finding P1, contrato SOAP não comprovado |
| Rollback | revisão do diff final | PASS: protótipo integralmente retirado |

Nenhum smoke fiscal, certificado real, endpoint transacional ou produto consumidor externo foi acessado.
