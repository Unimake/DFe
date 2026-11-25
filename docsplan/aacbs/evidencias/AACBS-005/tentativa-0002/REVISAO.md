# Revisão independente da retomada AACBS-005

## Pagamentos e recolhimentos

Conferi os dois logs originais de abertura, as rotas da configuração embutida e os exemplos das páginas oficiais arquivadas. Os registros de 25/11/2026, às 00:58:43 UTC para pagamentos e 00:59:15 UTC para recolhimentos, confirmam HTTP 404, contrato incompatível na raiz e encerramento em `AberturaAmbigua`. As páginas documentam POST com CNPJ-base e `urlRetorno` vazio no cenário sem webhook. A [investigação](INVESTIGACAO-404.md) e o [pedido técnico preparado](SOLICITACAO-404.md) preservam os limites das evidências. O tíquete, a camada que respondeu, o estado de habilitação e o processamento no backend permanecem desconhecidos.

Recalculei os SHA-256 das oito fontes técnicas arquivadas. Os valores coincidem com os campos anterior e atual da [revalidação](REVALIDACAO-FONTES.csv) e com o inventário da tentativa anterior. Esta revisão usou as cópias locais e os registros do executor. Os comunicados públicos citados na investigação são fontes contextuais distintas dessas oito fontes técnicas.

## Proposta de DARF

O inventário privado contém a rotina `Inspecionar.ps1` (SHA-256 `4C69696EA023A00976AFD119B5D746A475CA37AFC1E9DEF9C882FC108262B3C4`), o visualizador `Visualizar-RAD01.ps1` (`CDC93EC250AD8474C2B37511E327102292BAE3CD515DC6802064799316E70C83`) e o arquivo cifrado da proposta. Recalculei o SHA-256 do arquivo cifrado. Ele coincide com o valor declarado no [preparo](PREPARO-DARF.md) e com o valor exigido pelo visualizador. O diretório está ignorado pelo Git.

No modo de preparo, a rotina verifica os hashes do JSON e do XML de créditos, a matriz e o tíquete coincidentes, os períodos, a sequência e os campos empregados na seleção. Ela filtra créditos positivos de origem normal, verifica modelo 55, comprimento da chave, dígito verificador, unicidade e raiz do emitente diferente da matriz. O XML usa a primeira candidata na ordem do original e contém apenas um item RAD, com `PAGAMENTO_TOTAL` e sem `valorDarfParcial`. A rotina valida o XSD e relê o XML protegido com DPAPI antes de informar o hash. O visualizador confere esse hash antes da leitura privada. O código de preparo e visualização não envia solicitações nem escreve no journal.

Os filtros demonstram consistência documental. A condição de adquirente, o débito pendente e o total que a Receita calcularia ainda exigem confirmação para a emissão real. A proposta RAD-01 permanece pendente de confirmação específica pelo DEV. PCONT aguarda contribuinte autorizado e período com saldo. O preparo não gerou XML PCONT, período ou valor presumido.

## Cobertura offline complementar

Conferi o SHA-256 `0521CF8D9EDE9A46B3306A2A15184E9570AEB195C5B02B24E8A8656CC76CC684` da [classe de regressão](../../../../../source/Unimake.DFe.Test/AACBS/Servicos/AACBSDarfContinuidadeTest.cs). Os cinco casos passam pela interface pública da DLL com transporte em memória. A sequência RAD/PCONT verifica token compartilhado, HTTP 401 com erro fiscal tipado, ausência de repetição automática e obtenção de outro token apenas após nova chamada explícita. O código sintético `DADOS_DE_ENTRADA_INVALIDOS` segue o exemplo para HTTP 401 no manual PCONT. Nos dois casos com resposta HTTP 200 interrompida, as asserções verificam descarte do corpo, limpeza do sucesso anterior, preservação do status e uma única tentativa de POST. Nas duas aberturas HTTP 404 simuladas, verificam método, rota, corpo sem webhook, ausência de tíquete e código fiscal, `SEM_RETORNO` e supressão do conteúdo remoto na exceção. Esses resultados tratam do comportamento local; as respostas recebidas pelo Beta em 24/11 seguem sem corpo preservado para comparação.

Os [testes Debug](testes-debug.log) e os [testes INTEROP](testes-interop.log) registram cinco casos aprovados em cada configuração, sem falhas ou casos pulados. Os builds terminaram com código zero e sem erros, com quatro avisos em Debug e quinze em INTEROP. O alcance desta prova é a execução dos testes gerenciados nas duas configurações. O registro COM não foi realizado nesta tentativa.

A revisão independente não executou as rotinas privadas nem abriu o conteúdo fiscal, não repetiu as aberturas e não realizou autenticação, emissão ou pagamento. O aceite da etapa cabe ao DEV.
