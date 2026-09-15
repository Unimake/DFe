# Unimake.DFe - NF-e ABI - Entrega ABI-005

A ABI-005 entrega exclusivamente na DLL os dois serviços publicados da NFeABI: consulta de status e autorização síncrona, ambos em homologação e segundo o contrato wire dos WSDLs oficiais. A configuração segue o padrão NFGas com um XML para cada uma das 27 UFs, todos herdando de `SVRS.xml` e embutidos no assembly.

Produção permanece sem URL e falha fechada. Consulta de protocolo, transporte de eventos e autorização ZIP não foram implementados. Os testes offline cobrem contrato WSDL, configuração, assinatura, retornos, processado, rejeições, segurança e reuso; nenhuma operação fiscal real foi transmitida.

O estado final é `DELIVERED_FOR_REVIEW`. Somente o DEV pode aprovar a ABI-005; a ABI-006 não foi iniciada.
