# Riscos

| ID | Risco | Prob. | Impacto | Indicador | Mitigação | Contingência | Owner | Deadline/etapa | Verificação | Estado |
|---|---|---|---|---|---|---|---|---|---|---|
| RISK-001 | documentação/XSD muda durante execução | média | alto | hash diverge | conferir pasta em Plan e Check | bloquear e criar replanejamento | executor | toda etapa 002+ | catálogo/hash | PRESENT |
| RISK-002 | WSDL/SOAPAction inacessível sem ambiente fiscal | média | alto | TLS handshake falha | obter em host/certificado preparado | bloquear ABI-005 sem adivinhar | DEV/equipe fiscal | antes de ABI-005 | evidence WSDL | PRESENT |
| RISK-003 | erro de ordem/precisão/choice no modelo extenso | alta | alto | round-trip/schema falha | fatiar core/eventos e usar fixtures amplas | corrigir antes do transporte | executor | ABI-003/004 | testes XPath/schema | MITIGATION_DEFINED |
| RISK-004 | quebra API/INTEROP/consumidores | média | alto | build/exemplo/UniNFe falha | copiar padrões públicos e teste integrado | reverter membro incompatível | executor | ABI-006 | build consumidor | MITIGATION_DEFINED |
