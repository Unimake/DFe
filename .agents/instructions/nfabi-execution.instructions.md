# Execução NF-e ABI

- Fonte normativa local obrigatória: $(System.Collections.Hashtable.SourceRoot); reler MOCs e XSDs no Plan e Check de cada etapa 002+.
- A pasta real de schemas é $(System.Collections.Hashtable.SchemaRoot); copiar somente na DLL e preservar nomes/conteúdo do pacote 1.00.
- Namespace http://www.portalfiscal.inf.br/nfeabi, modelo 77, UTF-8, XMLDSig e serviço síncrono.
- Endpoints publicados em 2026-09-10: somente NFeABIStatusServico 1.00 e NFeABIAutorizacao 1.00 em homologação.
- Produção, consulta de protocolo e recepção de eventos não ganham URL presumida. Mudança oficial exige replanejamento.
- Reuse padrões NFGas/NFCom sem copiar bugs, strings erradas ou comportamento não sustentado pelo XSD.
