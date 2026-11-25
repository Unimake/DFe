# Auditoria da compatibilidade NI de créditos

Decisão explícita do DEV em 25/11/2026: aprovar NI 8/14 em créditos. Entradas do plano aprovado: schema Xml/Schemas/AACBS/retArquivoCreditos_v1.00.xsd e classes Xml/AACBS, na biblioteca principal. Raiz de schemas: Xml/Schemas/AACBS; base Git 5e8e9b6aa. A divergência e o original protegido estão documentados na AACBS-004, sem nova consulta necessária.

## Mudança estrutural e grafo

Componente preexistente alterado: /retArquivoCreditos/ni, namespace http://www.unimake.com.br/dfe/aacbs, versão 1.00. Elemento obrigatório, posição 2 na sequence, sem mudança de nome, namespace, ordem ou cardinalidade. O tipo Identificacao8 foi substituído por união anônima exclusiva deste elemento entre Identificacao8 e Identificacao14. Nenhum tipo global foi alterado; não há inserção de tag, remoção, renomeação ou movimentação.

O [grafo local](GRAFO-XSD.csv) inventaria includes/imports/redefines do domínio. Fechamento: tiposComuns_v1.00.xsd → retArquivoCreditos_v1.00.xsd → retBaixarArquivoRetorno_v1.00.xsd, onde o ramo da choice usa RetArquivoCreditosTipo. Compilação XmlSchemaSet dos dois fechamentos antes/depois passou, com dependências locais resolvidas e sem busca de schema remoto.

## Classes, consumidores e risco

RetArquivoCreditos [XmlRoot/XmlType] → Ni [XmlElement("ni", Order=1)] é o mesmo caminho semântico. Continua string, sem mudança estrutural de classe, getter/setter, assinatura pública ou COM; somente summary esclarecido. Envelope RetBaixarArquivoRetorno.ArquivoCreditos conserva tipo e choice.

Mapper ArquivoCreditos preserva obj.Texto("ni") integralmente, com validação de oito ou quatorze dígitos ASCII e comparação dos primeiros oito com o CNPJ do envio. CabecalhoArquivo habilita o comprimento alternativo apenas quando chamado por créditos. Pagamentos e recolhimentos usam a forma padrão estrita de oito. O comportamento preexistente de débitos 8/14 não mudou. Identificacao8 global continua limitando as quatro aberturas e o pedido de download.

Busca estrutural em todo o checkout, inclusive a pasta oculta .NET Standard, localizou: modelo, envelope, mapper, ValidarConfig.xml e testes de serialização/validação/parsing. ValidarConfig.xml, nomes dos recursos, inclusões no projeto, resolvedores centrais, namespace e versão não precisam alteração. Não há consumidores UniNFe/PRO implementados neste escopo; nenhum pacote externo será restaurado ou consumidor alterado como parte deste delta. Não há enum novo ou conversão numérica do identificador.

Expectativas preexistentes ajustadas nos testes: a rejeição de NI14 em créditos, antes compartilhada com pagamentos/recolhimentos; as duas últimas permanecem negativas. Testes de oito dígitos, serialização, apuração vazia/com itens, round-trip e XSD do envelope são mantidos. Regressões novas cobrem preservação dos quatorze dígitos, rejeição de outra raiz e comprimentos/caracteres inválidos, sem ampliar envios ou outras famílias.

Os resultados efetivos dos runners, a leitura do original sem substituição e a revisão independente serão registrados na entrega da AACBS-005. A aprovação não elimina os critérios reais pendentes de pagamentos/recolhimentos ou DARFs. Publicação NuGet segue fora do escopo.

Validação efetiva do original: [VERIFICACAO-REAL-CREDITOS.json](VERIFICACAO-REAL-CREDITOS.json). O JSON integral de 8.969 bytes, sem substituição de NI, passou pelo serviço público com transporte em memória: 2 períodos, 27 créditos, NI14 preservado e vínculo de matriz/base/tíquete confirmado. Todos os campos e ordens comparados; XSD/conteúdo do filho e envelope e round-trips aprovados. XML operacional cifrado e relido; journal finalizado sob mutex, com os demais campos preservados. Zero HTTP real nesta prova. O runner Debug final passou em 499 casos; resultados INTEROP e pacote constam na entrega final.
