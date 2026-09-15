# ABI-005 attempt-0002 - Contrato WSDL oficial

Os WSDLs foram obtidos em 2026-09-14 por HTTPS mTLS dos próprios endpoints oficiais de homologação. O certificado A1 foi carregado diretamente do PFX autorizado pelo DEV; não houve consulta ao repositório de certificados do Windows. Certificado, senha e dados identificadores não foram persistidos.

| Serviço | HTTP | SHA-256 | Wrapper de entrada | Elemento de retorno | SOAPAction | Endpoint |
|---|---:|---|---|---|---|---|
| Status | 200 | `33EEED91FD925E88C139E15C08B1CA135EA1A995C99FC1990ECD80F437EFCDAF` | `nfeabiDadosMsg` | `nfeabiStatusServicoResult` | `http://www.portalfiscal.inf.br/nfeabi/wsdl/NFeABIStatusServico/nfeabiStatusServico` | `https://homologacao.nfeabi.fazenda.pr.gov.br/nfeabi/NFeABIStatusServico` |
| Autorização | 200 | `B7A2BF6692DA4A2A5C199332D86AF45DF67A28C57A5C8F920160B326DDD813C8` | `nfeabiDadosMsg` | `nfeabiAutorizacaoResult` | `http://www.portalfiscal.inf.br/nfeabi/wsdl/NFeABIAutorizacao/nfeabiAutorizacao` | `https://homologacao.nfeabi.fazenda.pr.gov.br/nfeabi/NFeABIAutorizacao` |

Ambos usam SOAP 1.2 document/literal. O WSDL de autorização também publica `nfeabiAutorizacaoZip`; essa operação não foi implementada porque está fora do escopo de `AutorizacaoSinc` definido pela ABI-005.
