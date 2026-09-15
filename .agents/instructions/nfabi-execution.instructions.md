# Execução NF-e ABI

- Fonte normativa local obrigatória: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi`; reler MOCs e XSDs no Plan e Check de cada etapa `ABI-002+`.
- A origem dos schemas é `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi\PL_NFeABI_1.00`; copiar somente na DLL e preservar nomes/conteúdo do pacote 1.00.
- Namespace http://www.portalfiscal.inf.br/nfeabi, modelo 77, UTF-8, XMLDSig e serviço síncrono.
- Endpoints publicados em 2026-09-10: somente NFeABIStatusServico 1.00 e NFeABIAutorizacao 1.00 em homologação.
- Produção, consulta de protocolo e recepção de eventos não ganham URL presumida. Mudança oficial exige replanejamento.
- Reuse padrões NFGas/NFCom sem copiar bugs, strings erradas ou comportamento não sustentado pelo XSD.
- Este plano altera e valida exclusivamente a DLL Unimake.DFe e seus testes no mesmo repositório; não acessar, modificar, compilar ou testar produtos consumidores externos.
