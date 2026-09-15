# Unimake.DFe - NF-e ABI - Revisão ABI-005 attempt-0001

## Resultado

FAIL. A revisão crítica independente encontrou um finding P1 e bloqueou a entrega.

## Finding material

O protótipo cadastrava `SOAPAction`, namespace/nome do wrapper e elemento de retorno inferidos dos padrões NFGas/NFCom. Os MOCs confirmam somente SOAP 1.2 e os métodos `nfeStatusServico`/`nfeAutorizacao`; o catálogo confirma somente nomes, versões e endpoints. Os testes repetiam as inferências e, portanto, não eram prova independente do contrato wire. Publicar esses valores violaria a exigência da ABI-005 de obter WSDL ou evidência oficial equivalente e de não adivinhar contratos.

## Disposição

O protótipo foi integralmente retirado. O finding não foi mascarado como limitação nem como PASS. A etapa permanece `BLOCKED` até que os WSDLs oficiais sejam fornecidos ou possam ser obtidos com autorização para usar certificado cliente. Esta revisão não aprova a ABI-005 e a ABI-006 não foi iniciada.
