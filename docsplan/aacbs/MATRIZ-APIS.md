# AACBS — Matriz de APIs e contratos XML

Referência documental: **12/11/2026**. Escopo: CBS; **seis endpoints de negócio e duas unidades auxiliares**, totalizando oito serviços planejados. Nomes abaixo são propostas para a especificação da etapa **AACBS-001**; não constituem API pública já criada ou aprovada.

Fontes, revisões, limites, acesso e lacunas estão em [FONTES-E-REQUISITOS.md](FONTES-E-REQUISITOS.md). Não há implementação nem autorização de consumo real nesta matriz.

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
| AUX01 | `Autenticar` | OAuth Client Credentials; token de produção publicado em `https://api.receitafederal.gov.br/token` | `autenticar` → `retAutenticar`; detalhes dependem do manual técnico vigente | 003; [manual oficial, p.13](https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/manuais/reforma-tributaria-do-consumo/manual-plataforma-cbs-21-maio-2026-07h40.pdf) |
| AUX02 | `BaixarArquivoRetorno` | `GET` da `urlAssinada` devolvida por API06 | `baixarArquivoRetorno` → uma das quatro famílias finais abaixo | Débitos em 003; outras famílias em 004; [regras comuns](https://docs.receitafederal.gov.br/apuracao-cbs/guiaapiassincrona.html) |

O serviço auxiliar de autenticação também atende a execução automática dos serviços de negócio: o DEV fornece credenciais por configuração e não precisa obter/copiar token. A etapa 001 definirá os envelopes de invocação, proteção de segredos e a exposição do resultado auxiliar sem persistência automática de credenciais/token.

A única rota de produção restrita comprovada neste levantamento é `https://api.receitafederal.gov.br/apuracao-cbs-prr/v2/download/{tiqueteSolicitacao}`. Não preencher outros endpoints por analogia. Configuração embutida deve representar ausência de suporte ao ambiente e impedir chamada acidental a outro ambiente.

## Contratos de abertura, acompanhamento e download

| Grupo | Entrada pública e transformação | Resposta pública e validação |
| --- | --- | --- |
| Aberturas API01–04 | XML contém CNPJ base e opção de webhook; mapper leva CNPJ ao path e produz corpo JSON com `urlRetorno`, incluindo string vazia quando não usado. Autorização vem da configuração/autenticação da DLL. | Retorno imediato preserva `tiqueteSolicitacao`, `tEASegundos` (inclusive -1) ou `codigoErro`/`mensagemErro`. HTTP 201 significa solicitação aceita, não arquivo final recebido. |
| Situação API05 | XML fornece tíquete; mapper usa path, sem criar corpo GET. | Preservar estado publicado e erros, inclusive situação expirada em HTTP 200. XSD distingue formas válidas de resposta. |
| Link API06 | XML fornece tíquete; autenticação Bearer da API. | Preservar URL assinada, expiração opcional e erros. O XML deste retorno contém segredo temporário e não deve ir para evidência/log. |
| Arquivo AUX02 | XML fornece contexto explícito da família, tíquete e link correspondente; formato exato definido em 001. GET externo sem Bearer nem credenciais da API, respeitando expiração e política de proxy. | Parser transforma JSON UTF-8 em XML tipado da família correta e valida XSD de retorno. Não inferir a família unicamente do conteúdo remoto. |
| Autenticação AUX01 | Configuração protegida fornece credenciais; mapping exato do POST OAuth depende da qualificação de F11 em 001. | Converter resposta/erro conforme contrato confirmado, mantendo dados sensíveis protegidos; cache isolado por credencial/ambiente quando adotado. |

Todos os XMLs de entrada, retorno imediato, erros e arquivos finais terão classes serializáveis e XSDs próprios ou tipos comuns explicitamente compartilhados. Roots, namespace pertencente à Unimake, atributos de versão, nomes de arquivos e relação com `SchemaVersao` serão definidos na etapa 001. XSDs da DLL não serão apresentados como schemas oficiais da Receita.

## Famílias do arquivo final

| Família / root proposto | Conteúdo a preservar | Particularidades obrigatórias na especificação e testes |
| --- | --- | --- |
| `retArquivoDebitos` | Identificação, geração, períodos de apuração, débitos/documentos e valores CBS. | Valores 18,2, `excedente` singular, opcionais ausentes, saldo zero obrigatório, múltiplos períodos/documentos e apuração vazia. |
| `retArquivoCreditos` | Identificação, períodos, créditos/documentos e hierarquia de apropriação/utilização/saldo. | `excedentes` plural, hierarquia intermediária mesmo com valores zero e ausência de opcionais; não reaproveitar a estrutura de débitos. |
| `retArquivoPagamentos` | Identificação, arrecadações, pagamentos, adquirente e composições. | Preservar `ni_contribuinte` e `chaveDFE` conforme contrato resolvido; RAD/PCONT, valores e campos condicionais. |
| `retArquivoRecolhimentos` | Identificação, arrecadações, pagamentos e composições com fornecedor. | `niFornecedor` distingue este arquivo de pagamentos; divergência de `tipo` precisa de resolução antes de enum/XSD fechado. |

As quatro famílias usam raízes e tipos de resultado distintos. Tipos comuns só serão compartilhados quando houver equivalência documental, para evitar aceitar arquivo de outro serviço por semelhança. A especificação deve registrar o tratamento de números, datas com fração/offset, identificadores como texto, ausência, vazio, `null` não documentado e campos desconhecidos.

## Responsabilidades e cobertura

A aplicação guarda tíquetes, agenda consultas e decide a retomada. Cada serviço da DLL executa sua operação e devolve XML/`Result`; não existe polling automático, espera de conclusão nem reabertura automática de POST com resposta incerta. A gravação dos XMLs usa a API de persistência existente, sob decisão do consumidor.

A etapa 002 entrega e testa todos os modelos/XSDs; 003 demonstra o ciclo completo de débitos, incluindo autenticação, situação, link e arquivo; 004 entrega os outros três ciclos e a cobertura conjunta. Cada célula da matriz deve ter teste de serialização, validação ou serviço aplicável. Testes de serviço passam por `Executar()` com transporte controlado, validando XML público → request HTTP/JSON → resposta/erro → XML público/`Result`.

Incluir estados pendente/processando/concluído/erro/expirado, 400/401/403/404/429/500, corpo inválido, timeout, retorno vazio, token expirado, link expirado, download interrompido e troca indevida de família. Erros de transporte preservam o padrão de exceções da DLL; não fabricar sucesso ou resposta fiscal quando não houve resposta de negócio.

Webhook não cria um nono serviço HTTP cliente: é uma entrada da aplicação, fora do escopo de hospedagem da DLL. O campo de envio continua suportado; eventual adaptação do payload recebido para XML será especificada em 001, sem exigir infraestrutura de webhook para usar os oito serviços.
