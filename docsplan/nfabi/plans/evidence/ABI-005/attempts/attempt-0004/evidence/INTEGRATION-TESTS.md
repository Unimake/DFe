# ABI-005 attempt-0004 - Integrações de homologação

| Serviço | Estrutura | Ambiente | Execução | Resultado |
|---|---|---|---|---|
| StatusServico | `ConsStatServNFeABI` tipado | homologação | `Executar()` com A1 configurado | PASS, 1/1 |
| AutorizacaoSinc | `NFeABI` tipada da massa mínima | homologação | `Executar()` com A1 configurado | PASS, 1/1 |

Os dois retornos reais trouxeram `cUF=PR`. O contrato interno continua fortemente tipado, e a compatibilidade converte a sigla somente em uma cópia dedicada à desserialização. Regressões offline comprovam o mesmo comportamento quando o namespace usa prefixo e asseguram que `RetornoWSString` e `RetornoWSXML` continuam brutos.

Não houve endpoint, teste ou tentativa de produção. O teste de autorização comprova o pipeline técnico e não afirma aprovação fiscal do documento.
