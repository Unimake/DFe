# AACBS-001 — Arquivos finais das consultas

Proposta de contrato XML para o JSON obtido por `BaixarArquivoRetorno` (AUX02), vinculada às quatro consultas assíncronas. A [matriz de campos](CONSULTAS-CAMPOS.csv) detalha propriedades, objetos, listas, itens, variantes e respectivas verificações. As seções 5 a 7 das páginas oficiais, revisão 1.3 de 13/11/2026, publicam os layouts, exemplos e respectivos JSON Schemas Draft 2020-12, versão 1.0.0. As referências F03–F06 e F14–F17 constam no [inventário](../FONTES-E-REQUISITOS.md).

| Família | Abertura | Classe de resultado e tipo complexo raiz propostos | Root XML | Fonte do arquivo final |
| --- | --- | --- | --- | --- |
| Débitos | API01 `ConsultarDebitos` | `RetArquivoDebitos` / `RetArquivoDebitosTipo` | `retArquivoDebitos` | [Página F03](https://docs.receitafederal.gov.br/apuracao-cbs/debito.html), [schema F14](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/debito-retorno.schema.json) |
| Créditos | API02 `ConsultarCreditos` | `RetArquivoCreditos` / `RetArquivoCreditosTipo` | `retArquivoCreditos` | [Página F04](https://docs.receitafederal.gov.br/apuracao-cbs/credito.html), [schema F15](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/credito-retorno.schema.json) |
| Pagamentos | API03 `ConsultarPagamentos` | `RetArquivoPagamentos` / `RetArquivoPagamentosTipo` | `retArquivoPagamentos` | [Página F05](https://docs.receitafederal.gov.br/apuracao-cbs/pagamentos.html), [schema F16](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/pagamento-retorno.schema.json) |
| Recolhimentos | API04 `ConsultarRecolhimentos` | `RetArquivoRecolhimentos` / `RetArquivoRecolhimentosTipo` | `retArquivoRecolhimentos` | [Página F06](https://docs.receitafederal.gov.br/apuracao-cbs/recolhimentos.html), [schema F17](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/recolhimento-retorno.schema.json) |

## Contrato XML

- As raízes e todos os elementos pertencem a `http://www.unimake.com.br/dfe/aacbs`. Cada raiz tem atributo obrigatório e não qualificado `versao="1.00"`, criado pela DLL. A resolução de schema usa a família e essa versão XML. As raízes são modelos próprios de `Unimake.Business.DFe.Xml.AACBS`, derivados de `XMLBase`.
- Os elementos preservam os nomes e a grafia do JSON. O XML tem sequência determinística na ordem da matriz para cada objeto, independentemente da ordem das propriedades na entrada JSON. O cabeçalho sempre contém `tiqueteSolicitacao`, `ni`, `niConsumidor`, `geradoEm` e `apuracao`.
- Cada lista JSON tem um elemento com o mesmo nome, contendo um filho `item` para cada entrada. O wrapper obrigatório `apuracao` aceita zero ou mais itens. `"apuracao":[]` produz `<apuracao />`. Dentro de uma apuração, `debitos`, `creditos` ou `pagamentos` devem conter pelo menos um `item`, assim como `composicao` em cada pagamento ou recolhimento. O CSV distingue a cardinalidade do wrapper da cardinalidade dos itens. Preservar a ordem e repetições, inclusive chaves repetidas em períodos diferentes.
- As coleções são `List<T>` tipadas. As propriedades C# usam PascalCase e o tipo do objeto dono aparece antes do ponto na matriz. Quando o mesmo caminho estrutural de pagamento muda de regra por tipo, os predicados do CSV indicam o ramo e são apenas notação de rastreabilidade. `[*]` representa cada item JSON.
- As identificações e chaves são `string`, incluindo `numeroDARF` com 17 dígitos, `ni` com 8 e `niConsumidor` com 11 ou 14. Os dígitos são ASCII e os zeros à esquerda são preservados. `niAdquirente`, `niContribuinte` e `niFornecedor`, quando aplicáveis, têm 14 dígitos. Validar presença, tamanho e formato sem acrescentar regra de dígito verificador. `chave` e `chaveDFE` admitem de 1 a 100 caracteres. `pa` é texto no formato `MM/AAAA`, com mês entre 01 e 12.
- **Exceção aprovada na AACBS-003:** `retArquivoDebitos/ni` aceita 8 ou 14 dígitos, preservados integralmente. O Beta devolveu a matriz completa de 14 dígitos enquanto HTML/schema oficiais continuam com oito. Conferir o valor de oito com a base enviada; em retorno de 14, conferir seus primeiros oito com essa base. Outra raiz/tamanho/tipo é inválido. Envios e demais famílias não são ampliados. Ver [constatação, aprovação e impacto](../evidencias/AACBS-003/tentativa-0001/PROPOSTA-NI-RETORNO.md).
- **Exceção adicional aprovada na AACBS-005, em 25/11/2026:** `retArquivoCreditos/ni` também aceita oito ou quatorze dígitos, preservando o valor integral e conferindo os oito primeiros com a base do pedido. Pagamentos/recolhimentos e os envios continuam com oito. A decisão decorre do arquivo real de créditos, sem alterar a fonte oficial, que ainda descreve oito. Ver [auditoria e validação](../evidencias/AACBS-005/tentativa-0001/AUDITORIA-NI-CREDITOS.md).
- `tiqueteSolicitacao` é texto opaco não vazio. Na conversão do download, conferir o tíquete conforme o contexto AUX02 e conferir as identificações quando disponíveis no contexto. Aplicar o esquema da família explícita e rejeitar conteúdo incompatível. O JSON vazio de apuração tem a mesma forma nas quatro famílias, por isso sua origem depende da correlação da solicitação/link e do contexto de download. O teste deve demonstrar esse limite.
- Os quatro schemas remotos fecham seus objetos para propriedades extras e excluem `null`. A projeção XML também é fechada e os elementos são não nillable. Proposta para a versão 1.00: rejeitar campo novo, campo duplicado, código desconhecido ou token de tipo incompatível, indicando a família e o caminho do erro sem incluir o conteúdo remoto. Campos opcionais ausentes ficam ausentes no XML. Tratar novas extensões como alteração de contrato.

### Valores e datas

| Tipo XSD proposto | Restrição e representação |
| --- | --- |
| `ValorMonetario18_2` | Restrição de `xs:decimal` com `totalDigits=18`, `fractionDigits=2` e faixa inclusiva de `-9999999999999999.99` a `9999999999999999.99`. As tabelas especificam 18,2 e os quatro JSON Schemas especificam múltiplo de 0,01 sem limite de magnitude ou sinal. A proposta aplica a precisão das tabelas e admite ambos os sinais. Ler o número JSON diretamente em base decimal, sem conversão por `double` nem arredondamento. Em XML usar ponto decimal, sem expoente. |
| `DataCalendario` | Restrição de `xs:date` a data válida com forma `yyyy-MM-dd`, sem fuso ou hora, mantida como `string` em C#. Usada em `dataArrecadacao`, em `vencimento` de recolhimentos e na forma aceita no PCONT de pagamentos. |
| `InstanteComFuso` | Tipo lexical `xs:string` com fuso obrigatório e forma ISO 8601/RFC 3339 do contrato remoto; calendário, fuso e projeção CLR são verificados pelo AACBSValidator. Essa composição evita arredondamento sub-tick pelo tipo xs:dateTime do CLR. O modelo disponibiliza `DateTimeOffset` e o membro `...Field` (`string`) preserva e serializa o texto recebido, incluindo fração, zeros finais, `Z` ou offset. O round-trip usa o texto original enquanto o instante interpretado permanecer sem edição. Ao alterar pelo membro tipado, a forma lexical anterior dá lugar à representação invariável do novo valor. |
| `VencimentoRad` | União restrita de `DataCalendario` e `InstanteComFuso`. `ComposicaoPagamentoArquivo.Vencimento` é o texto serializado. A projeção opcional `VencimentoInstante` é `DateTimeOffset?` e existe somente para o ramo RAD com instante. A restrição por ramo é detalhada abaixo. |

A leitura de instantes deve preservar a fração lexical também quando superar a resolução de ticks de `DateTimeOffset`. A projeção tipada usa a resolução do CLR, truncando apenas a parte sub-tick, sem alterar o XML ou o valor textual. Se fuso ou data/hora não admitirem projeção do CLR, a incompatibilidade deve ser explícita, sem substituição por meia-noite, UTC ou valor padrão. Datas civis permanecem datas de calendário em todos os pontos.

Valores fiscais obrigatórios devem preservar o zero explícito. As narrativas de débitos e créditos indicam que o produtor costuma omitir campos monetários opcionais iguais a zero. Os schemas permitem zeros explícitos nesses campos. Para preservar o valor recebido, usar `decimal?` nos opcionais e serializar zero quando estiver presente. Manter os grupos obrigatórios de crédito mesmo no caso de zero em todos os valores presentes. `principal`, `multa`, `juros` e `total` são campos remotos distintos e preservados como recebidos. [Débitos F03/F14](https://docs.receitafederal.gov.br/apuracao-cbs/debito.html), [créditos F04/F15](https://docs.receitafederal.gov.br/apuracao-cbs/credito.html), [pagamentos F05/F16](https://docs.receitafederal.gov.br/apuracao-cbs/pagamentos.html), [recolhimentos F06/F17](https://docs.receitafederal.gov.br/apuracao-cbs/recolhimentos.html)

## Hierarquias e variantes

### Débitos e créditos

`apuracao/item` agrupa por `pa` e contém `debitos/item` ou `creditos/item`. Cada registro exige `origem`, `documento`, `chave`, `emissao`, `registro`, `atualizacao` e `cbs`. `origem` é `int` e admite `0, 20, 21, 22, 30, 31, 32, 50, 51, 52, 53, 54, 55`, incluindo o zero obrigatório que representa origem normal. `documento` é `int` e admite `55, 57, 62, 63, 64, 65, 66, 67, 91, 92, 93, 94, 97`. A chave é obrigatória nos dois arquivos. [Schema F14](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/debito-retorno.schema.json), [schema F15](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/credito-retorno.schema.json)

| Família | Hierarquia dos valores CBS |
| --- | --- |
| Débitos | `cbs` obrigatório contém `apurado` e `saldoDevedor` obrigatórios. `excedente`, `inexigivel`, `suspenso`, `pgtoAntecipado` e `extinto` são opcionais. |
| Créditos | `cbs` obrigatório contém `apurado`, `apropriacao` e o opcional `excedente`. `apropriacao` exige `utilizacao` e admite `inapropriavel`, `suspenso`, `prescrito`, `aApropriar` e `apropriado`. `utilizacao` exige `naoUtilizado` e admite `inutilizavel`, `utilizado` e `restabelecido`. `naoUtilizado` exige `saldoCredor` e admite `pedidoRessarcimento`. |

### Pagamentos

`apuracao/item` agrupa por `dataArrecadacao`. Cada `pagamentos/item` tem `numeroDARF`, `tipo`, identificação e `composicao/item`. Em qualquer ramo, cada item de composição exige `sequencial` de 1 a 99999999, `pa`, `vencimento`, `principal`, `multa`, `juros` e `total`. Uma mesma lista pode conter itens RAD e PCONT. [Tabela e exemplo F05](https://docs.receitafederal.gov.br/apuracao-cbs/pagamentos.html), [schema F16](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/pagamento-retorno.schema.json)

| Ramo | Nível do pagamento | Nível da composição |
| --- | --- | --- |
| RAD, `tipo=1` | Exige `niAdquirente`. `niContribuinte` é vedado neste nível. | `niContribuinte` e `chaveDFE` opcionais. `vencimento` conforme proposta L10. |
| PCONT, `tipo=2` | Exige `niContribuinte`. `niAdquirente` é vedado neste nível. | `vencimento` é somente data civil. `niContribuinte` e `chaveDFE` vedados neste nível. |

A estrutura XSD 1.0 para `PagamentoArquivoTipo` propõe `xs:choice` entre `niAdquirente` e `niContribuinte`, ambos depois de `tipo`, e um tipo de composição que admite as propriedades dos dois ramos. A validação de conteúdo é obrigatória para vincular a identidade, a presença de campos da composição e o formato do vencimento ao valor de `tipo`. Aplicar a regra durante a conversão do JSON e na validação do XML recebido ou produzido pela biblioteca, pois a escolha estrutural do XSD 1.0 não vincula o valor de um elemento às demais restrições. A grafia `ni_contribuinte` pertence ao layout antigo e é uma propriedade desconhecida na versão atual.

**L10 — vencimento RAD.** A tabela e o exemplo de pagamentos usam `date`; o schema em `$defs/composicaoRAD/properties/vencimento` exige `date-time` e mantém `date` no PCONT. Proposta para revisão do DEV: aceitar a união restrita das duas formas documentadas somente para RAD de pagamentos. O XML e a classe preservam a forma que chegou. Se a variante recebida for uma data civil, registrar o desvio do schema F16 na evidência sanitizada pelo caminho/regra, sem valor fiscal. Qualquer terceira forma é inválida. [F05, §§5 e 7](https://docs.receitafederal.gov.br/apuracao-cbs/pagamentos.html), [F16, composição RAD](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/pagamento-retorno.schema.json#/$defs/composicaoRAD)

### Recolhimentos

`apuracao/item` agrupa por `dataArrecadacao`. Cada `pagamentos/item` exige `numeroDARF`, `tipo=1` e a coleção de `composicao`, sem identificação no nível do pagamento. Cada `composicao/item` exige `sequencial`, `pa`, `vencimento` como data civil, `niFornecedor` e os quatro valores. `chaveDFE` é opcional. Ausência de registros é representada por `apuracao` vazia. [Tabela e exemplo F06](https://docs.receitafederal.gov.br/apuracao-cbs/recolhimentos.html), [schema F17](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/recolhimento-retorno.schema.json)

## Exemplos sintéticos

Cabeçalho mínimo de débitos com apuração vazia. A mesma estrutura mínima se aplica às outras três famílias, com suas respectivas raízes.

```xml
<retArquivoDebitos xmlns="http://www.unimake.com.br/dfe/aacbs" versao="1.00">
  <tiqueteSolicitacao>TIQUETE-TESTE</tiqueteSolicitacao>
  <ni>00000001</ni>
  <niConsumidor>00000000000001</niConsumidor>
  <geradoEm>2026-08-31T10:11:12.123400-03:00</geradoEm>
  <apuracao />
</retArquivoDebitos>
```

Os seguintes fragmentos especificam conteúdo interno em uma resposta não vazia. Cada fragmento de CBS pertence ao `item` de um documento; cada fragmento de composição pertence à lista `composicao` do pagamento correspondente.

| Cenário | Fragmento de saída XML |
| --- | --- |
| Débito com saldo devedor zero | `<cbs><apurado>0</apurado><saldoDevedor>0</saldoDevedor></cbs>` |
| Crédito com saldo credor zero e grupos obrigatórios | `<cbs><apurado>0</apurado><apropriacao><utilizacao><naoUtilizado><saldoCredor>0</saldoCredor></naoUtilizado></utilizacao></apropriacao></cbs>` |
| RAD de pagamentos com data civil | `<item><sequencial>1</sequencial><pa>08/2026</pa><vencimento>2026-09-20</vencimento><principal>100</principal><multa>0</multa><juros>0</juros><total>100</total></item>` |
| PCONT de pagamentos | `<item><sequencial>1</sequencial><pa>08/2026</pa><vencimento>2026-09-20</vencimento><principal>80.50</principal><multa>0</multa><juros>0</juros><total>80.50</total></item>` |
| Recolhimento com fornecedor e sem chave | `<item><sequencial>1</sequencial><pa>08/2026</pa><vencimento>2026-09-20</vencimento><niFornecedor>00000000000002</niFornecedor><principal>100</principal><multa>0</multa><juros>0</juros><total>100</total></item>` |

## Casos de verificação

Os casos são especificações offline para as etapas correspondentes. Cada ajuste negativo deve partir de uma massa sintética válida e alterar uma regra por vez, observando o caminho que foi rejeitado.

| ID | Casos e critérios |
| --- | --- |
| ARQ-01 | Para cada família, converter massa com todos os campos permitidos e ao menos dois itens por coleção interna. Conferir raiz, namespace, versão, propriedade tipada, presença, ordem, valores e round-trip do XML. Exercitar os dois ramos de pagamentos e comparar cada caminho da matriz. Reordenar propriedades no JSON e manter a mesma sequência XML. Repetir uma chave de documento e manter as ocorrências. |
| ARQ-02 | Aceitar `apuracao: []` nas quatro famílias e preservar wrapper vazio. Rejeitar wrapper ausente, nulo ou de tipo diferente de array. |
| ARQ-03 | Para cada linha 1:1, remover a propriedade/grupo e rejeitar. Para cada lista interna 1:N, rejeitar lista vazia e item não objeto. Para cada propriedade, inclusive opcional, rejeitar `null`; rejeitar `xsi:nil` e propriedades duplicadas/desconhecidas em qualquer nível. |
| ARQ-04 | Preservar identificações e chaves com zeros à esquerda, chaves de 1 e 100 caracteres, `pa` de janeiro e dezembro e tíquete opaco. Rejeitar tamanho inválido, identificador numérico, dígito não ASCII, chave de 101 caracteres, chave ou tíquete vazio, `pa` `13/2026` e `8/2026`. |
| ARQ-05 | Preservar `Z`, offset `-03:00`, fração com zeros finais e fração além da resolução de ticks. Rejeitar instante sem fuso, data civil com hora/fuso e datas inexistentes. Verificar que a projeção C# de instante com fração sub-tick não substitui a forma lexical no round-trip. |
| ARQ-06 | Aceitar todos os códigos enumerados, inclusive `origem=0`, e o sequencial mínimo/máximo. Rejeitar `documento=99`, `origem=1`, `tipo=3`, `tipo=2` em recolhimentos, `sequencial=0`, `100000000`, valor fracionário ou string no lugar de código. |
| ARQ-07 | Verificar valores obrigatórios zero, positivos, negativos e limites inclusivos de 18,2, com precisão em centavos. Rejeitar parte fracionária menor que um centavo, excesso de magnitude, string numérica e vírgula decimal em XML. Preservar os quatro valores remotos de composição sem recalculá-los. |
| ARQ-08 | Exercitar cada valor monetário opcional presente, ausente e com zero explícito em débitos/créditos. Preservar `decimal?` e a presença do XML. Em créditos com zeros, preservar `apropriacao/utilizacao/naoUtilizado/saldoCredor`; rejeitar remoção de cada grupo obrigatório e o nome antigo `excedentes`. |
| ARQ-09 | Rejeitar raiz de família diferente no XML, versão ausente/incorreta, namespace incorreto ou ausente, ordem XML inválida, item fora do wrapper e elementos repetidos fora de lista. |
| ARQ-10 | Aceitar RAD e PCONT simultâneos em pagamentos, RAD com e sem identificador/chave opcionais na composição e recolhimento com e sem chave. Rejeitar identificação ausente, ambas as identificações presentes, identificação de outro ramo, `ni_contribuinte`, `niContribuinte` ou `chaveDFE` na composição PCONT e recolhimento sem `niFornecedor`. Executar pelo mapper e pela validação de conteúdo XML. |
| ARQ-11 | Selecionar o parser pelo contexto do download. Rejeitar arquivo preenchido de outra família e tíquete divergente. Exercitar a forma vazia comum com vínculo de origem válido e comprovar que apenas o conteúdo JSON idêntico não distingue as quatro famílias. Rejeitar JSON inválido ou raiz de tipo diferente de objeto. |
| ARQ-12 | Converter RAD com `2026-09-20` e com `2026-09-20T00:00:00.0100-03:00` preservando os dois textos. Rejeitar `20/09/2026`, `2026-09-20T00:00:00` e `2026-02-30`. Rejeitar o instante em PCONT e recolhimentos. Identificar a exceção de contrato aplicada à data civil RAD sem expor o valor retornado em evidência. |

Correção técnica da revisão do ticket ID #189855: o limite de 100 caracteres de chave/chaveDFE é contado por pontos de código Unicode no validador de conteúdo. O XSD usa guarda de até 200 unidades UTF-16 para não rejeitar prematuramente caracteres suplementares. A validação completa combina XSD e AACBSValidator; o limite efetivo continua sendo 100. As entradas/saídas XML AACBS preservam CR e CRLF usando escrita com entitização, inclusive nas entradas COM.
