# Unimake.DFe - NF-e ABI - Evidência ABI-005

- Etapa: ABI-005
- Status: BLOCKED
- AttemptId: attempt-0001
- Estado-base: ABI-004 `APPROVED`; contratos XML principal, retornos e eventos presentes
- Início: 2026-09-14T17:29:17-03:00
- Término: 2026-09-14T17:46:09-03:00
- Próxima etapa iniciada: NÃO

## Resultado

A predecessora ABI-004 foi promovida para `APPROVED` por autorização explícita do DEV. A execução da ABI-005 foi bloqueada antes da entrega porque não foi possível comprovar o contrato wire dos serviços. O portal oficial publica somente Status e Autorização para homologação, enquanto o MOC confirma SOAP 1.2, os métodos `nfeStatusServico` e `nfeAutorizacao` e os XMLs fiscais. Nenhuma dessas fontes declara os valores exatos de `SOAPAction`, namespace/nome do wrapper SOAP ou elemento de retorno.

O acesso aos dois WSDLs oficiais falhou no handshake TLS com alerta 40 tanto via Schannel quanto OpenSSL, comportamento compatível com exigência de certificado cliente. Um protótipo com metadados inferidos chegou a compilar e passar testes offline, mas a revisão crítica independente o classificou como finding P1 por violar a proibição de adivinhar contratos. Todo o código, configuração, fixtures e testes do protótipo foi retirado; permanecem somente os registros de planejamento e evidência desta tentativa.

## Fonte normativa

- Fonte: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi`.
- Schemas: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi\PL_NFeABI_1.00`.
- Snapshot geral no Plan e no Check: `0383F95D81140925C4EF91046C2D723CA9D94F477E138069F7202D79D9CCCAD8` — PASS, 22/22 arquivos.
- Snapshot dos schemas no Plan e no Check: `BA44B39981C4DF3860A6BE9AA7C0739904D664377EABE963AB52AC8C2E74AE40` — PASS, 20/20 arquivos.

## Evidências executadas

- Catálogo oficial: os únicos endpoints publicados são `NFeABIStatusServico` 1.00 e `NFeABIAutorizacao` 1.00, ambos em homologação.
- Tentativas de obter ambos os WSDLs: falha TLS `SEC_E_ILLEGAL_MESSAGE` / `SSL alert handshake failure (40)`; nenhum WSDL foi arquivado.
- Protótipo temporário: build DLL normal PASS, build INTEROP PASS, build de testes PASS e 9/9 testes offline PASS.
- Regressões ABI-003/ABI-004/fundação: 3/3, 9/9 e 14/14 PASS.
- Revisão crítica independente: FAIL por finding P1 no contrato SOAP não comprovado.
- Rollback: PASS; nenhuma alteração de código do produto, configuração ou teste do protótipo permanece no diff.

## Condição de desbloqueio

Fornecer os WSDLs oficiais dos dois endpoints ou autorizar explicitamente o uso de um certificado cliente válido apenas para obtê-los. Na retomada, registrar um novo AttemptId, extrair valores wire exatos, implementar os serviços, substituir testes autorreferentes por comparações exatas contra o WSDL e repetir todos os gates.

## Parada

ABI-005 não foi entregue nem aprovada. ABI-006 permanece `PLANNED` e não foi iniciada.
