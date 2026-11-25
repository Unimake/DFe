# AACBS-001 — Contrato público e regras comuns

Especificação documental aprovada pelo DEV em 24/11/2026, incluindo as propostas de datas e retorno global do DARF RAD. A implementação desses contratos pertence à AACBS-002, liberada na mesma resposta; este documento preserva a especificação da AACBS-001.

## Artefatos e precedência

- [CONSULTAS.md](CONSULTAS.md) e [CONSULTAS-CAMPOS.csv](CONSULTAS-CAMPOS.csv): quatro arquivos finais e suas variantes.
- [DARF.md](DARF.md) e [DARF-CAMPOS.csv](DARF-CAMPOS.csv): dois envios e retornos síncronos.
- [ACESSO-E-FLUXO.md](ACESSO-E-FLUXO.md) e [ACESSO-CAMPOS.csv](ACESSO-CAMPOS.csv): autenticação, aberturas, acompanhamento e download.
- [CATALOGO-XML.csv](CATALOGO-XML.csv): raízes, classes, schemas e resolução dos 24 documentos XML.
- [Baseline de fontes](../fontes/2026-11-24/README.md) e [manifesto SHA-256](../fontes/2026-11-24/MANIFESTO-FONTES.csv): arquivos efetivamente obtidos.

As matrizes usam `;`, UTF-8 e as colunas `id`, `contrato`, `origemApi`, `caminhoApi`, `caminhoXml`, `propriedadeCSharp`, `tipoCSharp`, `tipoXsd`, `cardinalidade`, `regra`, `teste`, `fonte`. Uma linha representa campo, grupo, item de coleção, raiz ou metadado, com seu critério de teste proposto. Caminhos com `[*]` são notação de cada item; predicados RAD/PCONT indicam condições, não tags novas. Não são testes já executados.

Esta especificação consolida as decisões comuns. As matrizes detalham as regras dos campos. Divergência entre publicações oficiais recebe uma política explícita abaixo, sem afirmar que fontes contraditórias passaram a concordar. A aprovação do DEV congela a proposta para a AACBS-002; alteração material posterior exige análise de impacto e nova evidência.

**Aditivo aprovado na AACBS-003, 24/11/2026:** `retArquivoDebitos/ni` aceita 8 ou 14 dígitos ASCII, preservando o valor integral e conferindo a base com o pedido. O retorno real Beta veio com a matriz completa; o HTML e o JSON Schema oficiais continuam limitando o campo a oito. A [decisão de compatibilidade e auditoria de impacto](../evidencias/AACBS-003/tentativa-0001/PROPOSTA-NI-RETORNO.md) limita a exceção aos débitos. Envios, demais famílias e versão XML 1.00 permanecem conforme o contrato anterior.

## Identidade e compatibilidade

| Item | Definição para a implementação |
| --- | --- |
| Domínio | `AACBS`, somente CBS; sem seleção de provedores. |
| Namespace C# XML | `Unimake.Business.DFe.Xml.AACBS`. |
| Namespace C# serviços | `Unimake.Business.DFe.Servicos.AACBS`. |
| Namespace XML Unimake | `http://www.unimake.com.br/dfe/aacbs`; identificador próprio, não apresentado como publicação ou endpoint da Receita. |
| Versão XML/XSD | `1.00`, independente do `/v2` HTTP, versão de JSON Schema ou NuGet. |
| Raiz/atributo | Nome do catálogo; `versao="1.00"` obrigatório, não qualificado e fixo. Elementos qualificados no namespace AACBS. |
| Ordem | Sequência do objeto conforme a matriz. A ordem de propriedades JSON não controla a ordem XML. |
| Linguagem/target | C# 7.3, netstandard2.0, namespaces em bloco, documentação pública e INTEROP preservados. |

Proposta de identidade numérica, conferida no código-base atual: `TipoDFe.AACBS = 24`; serviços `AACBSAutenticar=140`, `AACBSConsultarDebitos=141`, `AACBSConsultarCreditos=142`, `AACBSConsultarPagamentos=143`, `AACBSConsultarRecolhimentos=144`, `AACBSConsultarSituacaoSolicitacao=145`, `AACBSConsultarLinkDownload=146`, `AACBSBaixarArquivoRetorno=147`, `AACBSGerarDarfAdquirente=148`, `AACBSGerarDarfContribuinte=149`. Os últimos valores atuais são TipoDFe 23 e Servico 139, além do sentinela 9999. Nada foi adicionado ao código nesta etapa. Revalidar conflitos antes da inserção; não renumerar APIs preexistentes.

## Forma pública dos serviços e modelos

Os dez serviços seguem o padrão CIOT: construtor vazio; construtores com modelo XML ou string XML e `Configuracao`; `Envio` tipado; `Executar()`; `Result` tipado; `RetornoWSXML`, `RetornoWSString` e `HttpStatusCode` herdados; gravação pelo padrão `GravarXmlDistribuicao`. INTEROP recebe a entrada pública equivalente. Para uma coleção pública `Nome`, fornecer `AddNome(T item)`, `GetNome(int index)` e `GetNomeCount()`; ocultar de COM os membros genéricos inadequados e manter os métodos tipados. Não criar um transporte ou um coordenador de negócio paralelo.

Cada raiz do catálogo tem classe pública própria, herdando de `XMLBase`, diretamente ou por base do domínio. Grupos internos são tipos públicos documentados. Os atributos `XmlRoot`, `XmlType`, `XmlElement`, `XmlArray`, `XmlArrayItem`, `XmlAttribute` e `XmlIgnore` definem explicitamente o contrato. Atributos COM seguem o padrão do repositório; membros que não podem ser expostos em COM recebem alternativa compatível ou `ComVisible(false)`.

O catálogo lista 24 XSDs de raiz em `Xml/Schemas/AACBS`. Acrescentar `tiposComuns_v1.00.xsd` para versão, identificações, decimal, datas, erro local e tipos estritamente equivalentes, com includes no mesmo targetNamespace. Tipos complexos específicos de cada família permanecem vinculados ao seu contrato. Embutir o conjunto completo e registrar todas as raízes no catálogo central, com resolução por namespace/raiz/versão; isso é necessário sobretudo para nomes genéricos como `autenticar`. Nenhum XSD dessa lista foi criado na AACBS-001.

`Configuracao.ClientID` e `Configuracao.ClientSecret` já existem. A grafia do campo de preparação “ClientId” é apenas orientação ao usuário; a integração preenche o membro real `ClientID`. Não criar outro par de credenciais com grafia quase igual. Credenciais não entram no XML/JSON fiscal; a DLL prepara o OAuth internamente. `SchemaVersao="1.00"`, `TipoDFe.AACBS`, `Servico` específico e `(int)UFBrasil.AN` identificam a configuração nacional.

### Resultado do download e resultado sem resposta

O `Result` de `BaixarArquivoRetorno` tem tipo concreto `RetBaixarArquivoRetorno`. Seu XML é um envelope Unimake:

```xml
<retBaixarArquivoRetorno xmlns="http://www.unimake.com.br/dfe/aacbs" versao="1.00">
  <retArquivoDebitos versao="1.00">
    <tiqueteSolicitacao>TIQUETE-SINTETICO</tiqueteSolicitacao>
    <ni>00000001</ni>
    <niConsumidor>00000000000001</niConsumidor>
    <geradoEm>2026-11-24T12:00:00Z</geradoEm>
    <apuracao />
  </retArquivoDebitos>
</retBaixarArquivoRetorno>
```

Escolha exclusiva entre `retArquivoDebitos`, `retArquivoCreditos`, `retArquivoPagamentos` e `retArquivoRecolhimentos`, expostas como propriedades `ArquivoDebitos`, `ArquivoCreditos`, `ArquivoPagamentos` e `ArquivoRecolhimentos`. O modelo interno pode ser serializado isoladamente com sua raiz própria. Assim .NET e COM recebem resultado concreto sem tentar adivinhar o tipo ou usar um retorno genérico de JSON. A API não recebe esse envelope; o mapper projeta o arquivo no filho da família informada.

Para cumprir o comportamento amigável de `Result` sem fabricar erro remoto, somente as dez raízes de resultado dos serviços herdam de `RetornoAACBSBase` e admitem uma alternativa local exclusiva:

```xml
<erroLocal><codigo>SEM_RETORNO</codigo><mensagem>Não há retorno disponível para este serviço.</mensagem></erroLocal>
```

Esse grupo tem dois textos obrigatórios não vazios, é metadado Unimake e nunca integra JSON. O retorno escolhe payload remoto **ou** `erroLocal`; o grupo não coexiste com sucesso/erro remoto e não substitui exceção de `Executar()`. Os quatro `RetArquivo*` herdam diretamente de `XMLBase`, exigem cabeçalho/apuração completos e não admitem `erroLocal`, isolados ou dentro do envelope. Quando não houve resposta de negócio, não inventar `codigoErro`, `cdRetorno`, `situacao` ou cStat. HTTP permanece em `HttpStatusCode`. O acesso a `Result` não dispara transporte, autenticação ou gravação.

## Regras de campos e tipos

1. Tags derivadas de propriedades remotas preservam nome e caixa, incluindo `niContribuinte`, `chaveDFE`, `numeroDARF`, `tEASegundos` e os nomes OAuth com `_`. C# usa PascalCase com atributos explícitos. Parâmetros de path e metadados locais têm projeção indicada nas matrizes e não vazam ao body.
2. Cada array JSON vira wrapper com o mesmo nome e filhos `item`; uma lista vazia válida mantém wrapper vazio. No RAD, a raiz XML usa `solicitacoes/item` para o array que é o próprio body JSON. Não colapsar lista unitária, remover duplicatas, reordenar itens ou produzir objeto em lugar de array.
3. Identificadores são texto. Consultas e contexto do download exigem CNPJ base com oito dígitos ASCII; DARFs exigem CNPJ completo com 14, no elemento `cnpj`. A configuração local guarda a matriz completa; o suporte de testes deriva a base quando montar consultas. O XML público não aceita máscara nem corrige tamanho por truncamento silencioso.
4. Montantes são `decimal`, sem passagem por `double`, arredondamento ou recálculo de totais. Opcionais usam `decimal?` em .NET; ausência e zero explícito são distintos. Em COM, usar valor e indicador explícito de presença ou a propriedade Field conforme padrão existente, sem sentinela numérica que colida com valor fiscal. Usar `ValorMonetario18_2`, conforme a matriz; não inventar restrição de sinal.
5. Datas civis são texto `yyyy-MM-dd`, validado como data real, sem hora/fuso. Instantes preservam o texto original e fuso em membro Field; a projeção .NET `DateTimeOffset` e alternativa COM não podem modificar o texto no round-trip. Definir e testar a projeção de fração sub-tick como documentado em CONSULTAS.md. A propriedade Field continua sendo o valor integral do contrato.
6. Objetos fiscais são fechados aos campos da baseline e às variantes explicitamente aceitas. Rejeitar `null` não documentado, nomes duplicados, tipos incompatíveis e campos desconhecidos com erro sanitizado de contrato, sem valor remoto. Códigos com domínio fechado no schema, como `origem`, `documento`, `tipo`, situação e `cdRetorno` DARF, obedecem ao catálogo. `codigoErro` das consultas/link e códigos OAuth são textos: preservar novo valor como erro, sem supor significado ou sucesso. Exceção OAuth: seguir sua regra normativa de ignorar nomes de resposta não reconhecidos, sem registrar seus valores e sem invalidar os campos obrigatórios conhecidos.
7. XSD 1.0 valida estrutura, ordem, tipos, cardinalidades e escolhas. Regras condicionais que dependem de `tipo`, `cdRetorno` ou `opcaoPagamento` ficam também no validator do domínio e no parser. Não enfraquecer XSD global para contornar uma condição local.

## Tratamento das divergências e restrições propostas

| Questão | Decisão para revisão do DEV | Limite explícito |
| --- | --- | --- |
| OAuth body/Basic — L01 | POST do mesmo endpoint Receita Integra conforme manual oficial DeRE 1.0.2: Basic, formulário e grant fixo. Não transferir payload/assinatura DeRE para CBS. | Escopo CBS literal não publicado: não inventar `scope`. Autenticação real ainda não executada. |
| Produção restrita — L02 | Ausência de URI REST qualificada impede configurar/habilitar esse ambiente; produção permanece configuração separada. | Bloqueia os testes reais PRR da 003, não a implementação offline dos contratos na 002. Não sondar prefixos candidatos nem usar produção como fallback. |
| Datas DARF — L09 | Aceitar exatamente uma entre `dataMaximaPagamento` do HTML e `dataVencimento` do schema, preservando a propriedade recebida em XML. | A primeira variante diverge do schema JSON literal. Não alegar conformidade simultânea, nem aceitar ambas/nenhuma. |
| Vencimento RAD — L10 | União restrita de data civil e instante com fuso apenas na composição RAD de pagamentos. PCONT/recolhimentos continuam data. | Preservar a variante e registrar a divergência por caminho/regra, sem dados fiscais. |
| RAD lote/objeto — L11 | Um POST com array 1:N e um resultado global objeto, exatamente como publicado; sem correlação por DF-e fabricada ou divisão do POST. | `DARF_GERADO` não comprova cada item. Teste real de lote aguarda esclarecimento; caso real inicial unitário exige habilitação própria. |
| CNPJ DARF — L12 | Quatorze dígitos conforme tabela e instruções; o exemplo de oito é tratado como inconsistente. | Sem tentativas reais alternando tamanhos. Códigos PCONT preservados mesmo quando sua descrição parece referir-se ao RAD. |
| Limites/PDF — L13 | Preservar link no XML. Nenhum GET de PDF ou pagamento. Orçamento técnico local é distinto de regra oficial. | A cota de quatro consultas não é aplicada por analogia aos DARFs; limites e idempotência de emissão seguem pendentes. |
| Formato monetário | Aplicar 18,2 das tabelas, mais a granularidade em centavos dos schemas; zero opcional explícito é preservado. | Não inferir proibição de valores negativos; resposta além da magnitude suportada é incompatibilidade, sem arredondar. |

As propostas de L09/L10 são compatibilidade deliberada com duas formas publicadas, não afirmação de correção da documentação. A validação dos fixtures contra JSON Schema oficial deve identificar esses desvios esperados, em vez de alterar silenciosamente os arquivos oficiais ou marcar falsamente conformidade.

## Transporte, erros e retomada

Endpoint/método vêm da configuração embutida da operação e ambiente. GET situação/link não tem body. O GET externo usa somente a URL assinada e a política explícita de proxy, sem Bearer, credenciais do servidor ou certificado de cliente da API. HTTPS e validação TLS permanecem ativos. Parâmetros de path opacos são codificados, sem mudar seu conteúdo.

Preservar status HTTP e corpo de negócio reconhecido até XML/`Result`, inclusive erro documentado em status não 2xx. Se o corpo não for reconhecível, registrar status conhecido e lançar falha sanitizada, sem produzir sucesso vazio ou XML fiscal inventado. A implementação adapta o caminho comum apenas onde necessário: seus traces e exceções de parsing devem passar em testes sentinela antes de dados reais.

No começo de cada `Executar()`, antes de preparação/autenticação, limpar resultado tipado em cache, `RetornoWSXML`, `RetornoWSString`, `RetornoWSRawString`, `RetornoWSStream` e status anterior. `HttpStatusCode=(HttpStatusCode)0` representa ausência de resposta HTTP da operação atual, sem inventar status de sucesso. Disponibilizar o status efetivamente recebido mesmo quando o parsing falhar depois; isso exige ajuste delimitado no ponto de propagação do transporte, com regressão. Não herdar o status da chamada anterior. Token servido do cache não tem HTTP atual; HTTP de uma autenticação auxiliar não se torna HTTP de negócio que não ocorreu.

Cada execução faz no máximo um request de negócio, além da autenticação estritamente necessária. Não repetir POST após 401/429/500, timeout ou resposta perdida. Uma próxima chamada explícita pode ocorrer depois de o consumidor resolver o contexto; não existe retry implícito de negócio. Nenhum status de solicitação é sonda oficial de disponibilidade.

O download valida a correspondência com CNPJ base e tíquete esperados; a família é fornecida pelo consumidor. Com `apuracao:[]`, os quatro arquivos são estruturalmente indistinguíveis. A DLL não consegue provar a família de origem somente pelo JSON vazio; a aplicação guarda a associação tíquete/família e o harness mantém esse vínculo local. Não mascarar esse limite com heurística de desserialização.

Cache de token usa identidade derivada irreversível da credencial, ambiente e endpoint de autenticação; mudança de segredo não reutiliza cache antigo. A cota é de autenticação, não se reinicia por instância/contribuinte/teste. A identidade de execução inclui ainda operação, empresa e entrada. Cache persistente cifrado e journal pertencem ao suporte de testes; a DLL não procura arquivos particulares ou cria polling/persistência de negócio.

## Verificações comuns exigidas na implementação

| ID | Critério a demonstrar |
| --- | --- |
| COM-01 | Todas as 24 raízes e includes do catálogo resolvem domínio, serviço, versão e namespace; XML de outra raiz não recebe schema por coincidência. |
| COM-02 | Cada linha das três matrizes tem asserção adequada de serialização/mapper/validação; campos opcionais têm ausência e presença, inclusive zero. Casos referenciados existem. |
| COM-03 | Arquivos finais independentes e envelope de download validam; escolha de exatamente uma família, erro local somente no envelope e leitura .NET/COM concreta. Rejeitar `erroLocal` aninhado no filho de arquivo. |
| COM-04 | `Result` antes de executar gera objeto com `erroLocal` amigável, sem request e sem código remoto falso. Reuso após sucesso seguido de timeout não expõe XML/arquivo/HTTP anterior; HTTP recebido antes de erro de parser é preservado. Token do cache não repete o HTTP da emissão. Leitura repetida de Result não altera XML nem gera tráfego. |
| COM-05 | XML inválido ou configuração de ambiente sem endpoint impede o request; fonte/credencial/localidade não são trocadas para obter sucesso. |
| COM-06 | Status, códigos/mensagens/listas reconhecidos e falhas de transporte permanecem distintos. Nenhum dado sensível em trace, exceção, asserção ou evidência. |
| COM-07 | Contar requests; GETs sem corpo, storage sem credenciais/certificado e proxy explícito em todos os caminhos; nenhum retry ou sonda oculta. |
| COM-08 | INTEROP_Release e build principal; coleções com métodos COM, opcionais sem perda de presença e datas sem perda da representação XML. |
| COM-09 | Roteiro real lê acesso DPAPI sem valores em comandos/logs, usa cache/cotas e retoma tíquete sem reabrir; PRR e DARF obedecem às restrições aprovadas. |

A AACBS-001 verificou documentalmente completude e consistência, hashes e matrizes; os resultados estão em seu dossiê. Estes casos de implementação permanecem planejados para 002–005. As propostas e limitações acima acompanham a entrega ao DEV; não iniciar a AACBS-002 antes da aprovação.

Em 25/11/2026 o DEV aprovou também NI de 8/14 dígitos no retorno de créditos, preservado integralmente e correlacionado pela raiz do pedido. A alteração permanece local a retArquivoCreditos/ni e ao ramo correspondente do envelope. Envios, pagamentos e recolhimentos não foram ampliados. A AACBS-005 registra a auditoria, as regressões e a validação do original cifrado.
