# AACBS — Matriz de APIs e contratos XML

Referência inicial: **12/11/2026**; revisão e qualificação da **AACBS-001 em 24/11/2026**, posteriormente aprovada pelo DEV. Escopo: oito endpoints de negócio e dois auxiliares, dez serviços. Nomes e contratos definidos em [ESPECIFICACAO.md](contratos/ESPECIFICACAO.md); os modelos XML/XSD foram entregues na AACBS-002, enquanto os serviços HTTP pertencem às etapas seguintes.

Fontes, revisões, limites, acesso e lacunas estão em [FONTES-E-REQUISITOS.md](FONTES-E-REQUISITOS.md). Não há implementação ou consumo real nesta revisão; as etapas liberadas e os limites de execução são registrados no PDCA e no roteiro local.

## Unidades de serviço

Base publicada de produção para negócio: `https://api.receitafederal.gov.br/apuracao-cbs/v2`.

| ID | Serviço proposto | Método / rota | XML de entrada → XML de retorno propostos | Entrega funcional planejada |
| --- | --- | --- | --- | --- |
| API01 | `ConsultarDebitos` | `POST /debitos/{cnpj}` | `consultarDebitos` → `retConsultarDebitos` | 003; [contrato RFB](https://docs.receitafederal.gov.br/apuracao-cbs/debito.html) |
| API02 | `ConsultarCreditos` | `POST /creditos/{cnpj}` | `consultarCreditos` → `retConsultarCreditos` | 004; [contrato RFB](https://docs.receitafederal.gov.br/apuracao-cbs/credito.html) |
| API03 | `ConsultarPagamentos` | `POST /pagamentos/{cnpj}` | `consultarPagamentos` → `retConsultarPagamentos` | 004; [contrato RFB](https://docs.receitafederal.gov.br/apuracao-cbs/pagamentos.html) |
| API04 | `ConsultarRecolhimentos` | `POST /recolhimentos/{cnpj}` | `consultarRecolhimentos` → `retConsultarRecolhimentos` | 004; [contrato RFB](https://docs.receitafederal.gov.br/apuracao-cbs/recolhimentos.html) |
| API05 | `ConsultarSituacaoSolicitacao` | `GET /situacao/{tiqueteSolicitacao}` | `consultarSituacaoSolicitacao` → `retConsultarSituacaoSolicitacao` | 003, comum aos quatro fluxos; [contrato RFB](https://docs.receitafederal.gov.br/apuracao-cbs/situacao-solicitacao.html) |
| API06 | `ConsultarLinkDownload` | `GET /download/{tiqueteSolicitacao}` | `consultarLinkDownload` → `retConsultarLinkDownload` | 003, comum aos quatro fluxos; [contrato RFB](https://docs.receitafederal.gov.br/apuracao-cbs/download.html) |
| API07 | `GerarDarfAdquirente` | `POST /rad/{cnpj}` | `gerarDarfAdquirente` → `retGerarDarfAdquirente` | 004; síncrono, documento 1.0 de 13/11; [contrato RFB](https://docs.receitafederal.gov.br/apuracao-cbs/gerar-darf-ad.html) |
| API08 | `GerarDarfContribuinte` | `POST /pcont/{cnpj}` | `gerarDarfContribuinte` → `retGerarDarfContribuinte` | 004; síncrono, documento 1.0 de 13/11; [contrato RFB](https://docs.receitafederal.gov.br/apuracao-cbs/gerar-darf-contrib.html) |
| AUX01 | `Autenticar` | `POST https://api.receitafederal.gov.br/token`, OAuth Client Credentials | `autenticar` → `retAutenticar`; Basic e formulário qualificados nas fontes do emissor | 003; [contrato de acesso](contratos/ACESSO-E-FLUXO.md) |
| AUX02 | `BaixarArquivoRetorno` | `GET` da `urlAssinada` devolvida por API06 | `baixarArquivoRetorno` → `retBaixarArquivoRetorno`, envelope tipado com exatamente uma família final | Débitos em 003; demais famílias em 004; [contrato comum](contratos/ESPECIFICACAO.md) |

O serviço auxiliar de autenticação também atende a execução automática dos serviços de negócio: o DEV fornece credenciais por configuração e não precisa obter/copiar token. O manual 1.4 exige cache, informa validade de uma hora e dois tokens/hora. O [contrato de acesso](contratos/ACESSO-E-FLUXO.md) qualifica Basic/formulário e define cache/renovação coordenada, proteção e resposta, sem gravação automática de XML com credenciais/token. O roteiro manterá cache operacional cifrado entre execuções.

Na revisão de 24/11, todas as páginas de endpoint lidas publicam somente produção. A URL PRR de download registrada em 12/11 foi removida da fonte atual. PRR depende de URL oficial; não preencher endpoints por analogia nem usar produção como fallback. Configuração embutida deve representar ausência de suporte ao ambiente e impedir chamada acidental a outro ambiente.

## Contratos de abertura, acompanhamento e download

| Grupo | Entrada pública e transformação | Resposta pública e validação |
| --- | --- | --- |
| Aberturas API01–04 | XML contém CNPJ base e opção de webhook; mapper leva CNPJ ao path e produz corpo JSON com `urlRetorno`, incluindo string vazia quando não usado. Autorização vem da configuração/autenticação da DLL. | Retorno imediato preserva `tiqueteSolicitacao`, `tEASegundos` (inclusive -1) ou `codigoErro`/`mensagemErro`. HTTP 201 significa solicitação aceita, não arquivo final recebido. |
| Situação API05 | XML fornece tíquete; mapper usa path, sem criar corpo GET. | Preservar estado publicado e erros, inclusive situação expirada em HTTP 200. XSD distingue formas válidas de resposta. |
| Link API06 | XML fornece tíquete; autenticação Bearer da API. | Preservar URL assinada, expiração opcional e erros. O XML deste retorno contém segredo temporário e não deve ir para evidência/log. |
| Arquivo AUX02 | XML fornece contexto explícito da família, tíquete e link correspondente; formato exato definido em 001. GET externo sem Bearer nem credenciais da API, respeitando expiração e política de proxy. | Parser transforma JSON UTF-8 em XML tipado da família correta e valida XSD de retorno. Não inferir a família unicamente do conteúdo remoto. |
| DARF RAD API07 | CNPJ com 14 posições conforme instruções, pendente de resolver exemplo divergente. Mapper produz **array JSON** com uma ou mais solicitações por DF-e: `chaveDFE`, opção total/parcial e valor parcial condicional. | Resposta síncrona com `cdRetorno`, dados do DARF e valores ou erro. Cardinalidade/correlação do retorno do lote e nome da data exigem resolução na 001. |
| DARF PCONT API08 | CNPJ com 14 posições conforme instruções; mapper produz **objeto JSON** com período `pa`, opção total/parcial e valor parcial condicional. | Retorno síncrono próprio, incluindo principal, multa, juros e total. Não participa da sequência tíquete/situação/link. |
| Autenticação AUX01 | `Configuracao.ClientID/ClientSecret`; Basic normativo, body form-urlencoded `grant_type=client_credentials`, sem inventar scope CBS. | Resposta/erro conforme contrato de acesso; cache por credencial/emissor/ambiente, renovação coordenada e cota própria de dois tokens/hora. |

Todos os XMLs possuem classes e XSDs próprios ou tipos comuns compartilhados na entrega da AACBS-002. O [catálogo](contratos/CATALOGO-XML.csv) define 24 raízes, classes e schemas; a [especificação](contratos/ESPECIFICACAO.md) fixa o namespace Unimake e a versão XML 1.00 aprovados pelo DEV. Os seis JSON Schemas oficiais são baseline documental; não são XSDs da DLL.

## Famílias do arquivo final

| Família / root proposto | Conteúdo a preservar | Particularidades obrigatórias na especificação e testes |
| --- | --- | --- |
| `retArquivoDebitos` | Identificação, geração, períodos de apuração, débitos/documentos e valores CBS. | Valores 18,2, `excedente` singular, opcionais ausentes, saldo zero obrigatório, múltiplos períodos/documentos e apuração vazia. |
| `retArquivoCreditos` | Identificação, períodos, créditos/documentos e hierarquia de apropriação/utilização/saldo. | Baseline atual usa `excedente` singular e `chave` obrigatória; hierarquia intermediária mesmo com valores zero e ausência de opcionais. |
| `retArquivoPagamentos` | Identificação, arrecadações, pagamentos e composições RAD/PCONT. | `niContribuinte` substitui o antigo `ni_contribuinte`; identificação por tipo, datas e divergência residual do vencimento RAD devem seguir a resolução da 001. |
| `retArquivoRecolhimentos` | Identificação, arrecadações, pagamentos e composições com fornecedor. | Baseline atual restringe `tipo=1`; `niFornecedor`, datas sem horário e chave opcional distinguem este arquivo. |

As quatro famílias usam raízes e tipos de resultado distintos. Tipos comuns só serão compartilhados quando houver equivalência documental, para evitar aceitar arquivo de outro serviço por semelhança. A especificação deve registrar números, datas versus datas/horas com fração/offset, identificadores como texto, ausência, vazio e campos desconhecidos. Os schemas atuais tipam os campos sem `null` e usam `additionalProperties:false`; conciliar a validação com a política de evolução, sem perda silenciosa.

## Retornos síncronos DARF e fontes de contrato

API07/API08 possuem raízes de retorno próprias, distintas das quatro famílias baixadas. Preservar `cdRetorno`, `darf` (PIX, código de barras e link PDF), `informacoesPagamento`, mensagem e campos inválidos conforme a resposta recebida. Os dois contratos diferem em valores de multa/juros; não compartilhar tipos apenas porque os exemplos parecem semelhantes. Gerar DARF não executa pagamento, e nenhuma chamada PIX está no escopo.

As páginas F03–F06/F12–F13 anunciam **Draft 2020-12, versão 1.0.0** dos seguintes schemas, acrescentados ao levantamento em 24/11:

| Operação | Schema JSON oficial de retorno |
| --- | --- |
| Débitos | [debito-retorno.schema.json](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/debito-retorno.schema.json) |
| Créditos | [credito-retorno.schema.json](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/credito-retorno.schema.json) |
| Pagamentos | [pagamento-retorno.schema.json](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/pagamento-retorno.schema.json) |
| Recolhimentos | [recolhimento-retorno.schema.json](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/recolhimento-retorno.schema.json) |
| DARF RAD | [gerar-darf-ad-retorno.schema.json](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/gerar-darf-ad-retorno.schema.json) |
| DARF PCONT | [gerar-darf-contrib-retorno.schema.json](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/gerar-darf-contrib-retorno.schema.json) |

Esses schemas não eliminam a qualificação da etapa 001: DARF usa `dataMaximaPagamento` no HTML e `dataVencimento` no schema; pagamento RAD usa vencimento data no exemplo/tabela e `date-time` no schema. RAD recebe array, mas o retorno é descrito como objeto único sem correlação por DF-e. Os detalhes e as resoluções ficam na 001, conforme [lacunas L09–L13](FONTES-E-REQUISITOS.md#lacunas-e-divergências-a-resolver).

## Responsabilidades e cobertura

A aplicação guarda tíquetes, agenda consultas e decide a retomada. Cada serviço da DLL executa sua operação e devolve XML/`Result`; não existe polling automático, espera de conclusão nem reabertura automática de POST com resposta incerta. A gravação dos XMLs usa a API de persistência existente, sob decisão do consumidor.

A etapa 002 entrega e testa todos os modelos/XSDs; 003 demonstra o ciclo completo de débitos, incluindo autenticação, situação, link e arquivo; 004 entrega os outros três ciclos, os dois DARFs e a cobertura conjunta. Cada célula da matriz deve ter teste de serialização, validação ou serviço aplicável. Testes de serviço passam por `Executar()` com transporte controlado, validando XML público → request HTTP/JSON → resposta/erro → XML público/`Result`.

Incluir estados pendente/processando/concluído/erro/expirado, 400/401/403/404/429/500, corpo inválido, timeout, retorno vazio, token expirado, link expirado, download interrompido e troca indevida de família. Erros de transporte preservam o padrão de exceções da DLL; não fabricar sucesso ou resposta fiscal quando não houve resposta de negócio.

Para DARF, incluir valor parcial condicional, pagamento total, array RAD versus objeto PCONT, dados de erro, diferenças entre os retornos, cardinalidade resolvida e preservação das datas/campos publicados. Não aplicar automaticamente a cota das quatro consultas aos DARFs.

Validação real incremental: 003 autenticação/débitos; 004 demais consultas e geração DARF somente em cenário explicitamente habilitado; 005 consolida resultados e lacunas sem repetir casos válidos. A configuração privada usa credencial DPAPI e metadados locais, conforme [ACESSO-LOCAL.md](ACESSO-LOCAL.md); o modo local de verificação passou sem obter token. Isso não comprova habilitação remota ou libera outra etapa.

Webhook não cria outro serviço HTTP cliente: é uma entrada da aplicação, fora do escopo de hospedagem da DLL. O campo de envio continua suportado; eventual adaptação do payload recebido para XML será especificada em 001, sem exigir infraestrutura de webhook para usar os dez serviços.
