# Unimake.DFe - NF-e ABI - Revisão ABI-004

## Resultado

PASS. A revisão crítica foi realizada em contexto independente do executor e terminou sem finding material remanescente.

## Findings encontrados e resolvidos

1. A massa inicial de pagamento omitia `gRed`; os grupos foram completados conforme o tipo XSD compartilhado.
2. A apropriação validava apenas cardinalidade; agora valida `nAdquir` incremental de 1 a 99 e soma de `pParticip` em 100.0000%.
3. As escolhas CNPJ/CPF agora rejeitam ambos ou nenhum autor/adquirente, em vez de normalizar silenciosamente estado inconsistente.
4. Foi adicionado caso positivo para `cdEventoPag=02`, com schema e round-trip.
5. A regra ZZA15-20 agora exige `vBC = vAcrescParcela` para `cdEventoPag=02`, com fixture coerente e caso negativo.

## Verificações independentes

- estrutura, ordem, namespace, optionalidade e precisão contra os sete XSDs aplicáveis;
- eventos 110111, 112110 (modalidades 01 e 02) e 112120;
- retorno e processado, inclusive assinatura, atributos opcionais e porta zero;
- invariantes de Id, detalhe, sequência, escolhas de identificação e participações;
- build normal/INTEROP, regressões ABI-003 e fundação de schemas;
- escopo restrito à DLL/testes NFeABI e sincronização do plano; ABI-005 intocada.

## Gate humano

Esta revisão não aprova a ABI-004. A promoção para `APPROVED` pertence exclusivamente ao DEV e não autoriza automaticamente o início da ABI-005.
