# Unimake.DFe - NF-e ABI - Entrega ABI-005

A ABI-005 entrega exclusivamente na DLL os serviços de status e autorização síncrona da NFeABI, ambos em homologação e conforme os WSDLs oficiais. A configuração possui um XML para cada uma das 27 UFs, herdando de `SVRS.xml`.

A `attempt-0004` acrescenta testes no padrão BPe para os dois serviços: cria objetos tipados, usa a configuração de certificado e chama `Executar()`. Ambos passaram contra homologação. A divergência observada no retorno (`cUF=PR`) é compatibilizada apenas na cópia usada para desserialização, sem alterar o XML bruto.

Produção permanece sem URL e falha fechada. Consulta de protocolo, transporte de eventos e autorização ZIP não foram implementados. O estado final é `DELIVERED_FOR_REVIEW`; somente o DEV pode aprovar a ABI-005 e a ABI-006 não foi iniciada.
