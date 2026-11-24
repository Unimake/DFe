# AACBS — Fontes e requisitos de acesso

Levantamento inicial em **12/11/2026**, revisado em **24/11/2026** na AACBS-000 e qualificado documentalmente na **AACBS-001, tentativa-0001**, após liberação expressa do DEV. Sem chamadas às APIs de negócio. Domínio: **AACBS — Apuração Assistida da CBS**; IBS depende de documentação e planejamento futuros.

A AACBS-000 foi aprovada e a especificação da AACBS-001 está em [contratos/ESPECIFICACAO.md](contratos/ESPECIFICACAO.md). A preparação local passou no modo `-Verificar`, sem revelar chaves ou obter token; na qualificação documental ainda não havia prova remota. O ambiente inicialmente informado pelo DEV foi Produção Restrita. Os resultados reais posteriores pertencem ao [dossiê AACBS-003](evidencias/AACBS-003/tentativa-0001/ENTREGA.md).

Após a pesquisa dos ambientes na AACBS-003, o DEV autorizou explicitamente os testes em Produção Beta. Os metadados locais foram atualizados, preservando as chaves e os limites do roteiro. Essa autorização não resolve a ausência de rotas oficiais PRR nem libera etapas seguintes sem aprovação.

## Fontes de referência

| ID | Fonte oficial | Versão identificada e uso |
| --- | --- | --- |
| F01 | [Página inicial da apuração CBS](https://docs.receitafederal.gov.br/apuracao-cbs/) | Reconsulta em 24/11/2026; conjunto CBS agora inclui geração de DARF RAD e PCONT. |
| F02 | [Regras comuns das APIs assíncronas](https://docs.receitafederal.gov.br/apuracao-cbs/guiaapiassincrona.html) | Documento 1.1, de 14/10/2026; fluxo comum, autenticação, webhook e download. |
| F03 | [Débitos](https://docs.receitafederal.gov.br/apuracao-cbs/debito.html) | Documento 1.3, de 13/11/2026; API v2, layout e schema JSON do arquivo final. |
| F04 | [Créditos](https://docs.receitafederal.gov.br/apuracao-cbs/credito.html) | Documento 1.3, de 13/11/2026; API v2, layout e schema JSON do arquivo final. |
| F05 | [Pagamentos](https://docs.receitafederal.gov.br/apuracao-cbs/pagamentos.html) | Documento 1.3, de 13/11/2026; API v2, layout e schema JSON do arquivo final. |
| F06 | [Recolhimentos](https://docs.receitafederal.gov.br/apuracao-cbs/recolhimentos.html) | Documento 1.3, de 13/11/2026; API v2, layout e schema JSON do arquivo final. |
| F07 | [Situação da solicitação](https://docs.receitafederal.gov.br/apuracao-cbs/situacao-solicitacao.html) | Documento 1.2, de 14/10/2026; estados e erros. |
| F08 | [Link de download](https://docs.receitafederal.gov.br/apuracao-cbs/download.html) | Mantém rótulo 1.0, de 13/10/2026, mas o conteúdo atual publica somente produção. |
| F09 | [Dúvidas frequentes](https://docs.receitafederal.gov.br/apuracao-cbs/faq.html) | Sem versão explícita; limites, janela incremental e características do webhook. |
| F10 | [Manual Plataforma CBS — maio/2026](https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/manuais/reforma-tributaria-do-consumo/manual-plataforma-cbs-21-maio-2026-07h40.pdf) | Páginas 6–9 e 13: representação, geração de credenciais e token. Contratos de negócio v1 estão superados pelas páginas atuais v2. |
| F11 | [Repositório oficial — Receita Integra](https://arquivos.receitafederal.gov.br/index.php/s/gn672Ad4CF8N6TK?dir=/Documentos/T%C3%A9cnicos/Receita+Integra) | Destino indicado por F02; a revisão atual obteve o manual técnico em fonte primária alternativa, F20. |
| F12 | [Gerar DARF como adquirente — RAD](https://docs.receitafederal.gov.br/apuracao-cbs/gerar-darf-ad.html) | Documento 1.0, de 13/11/2026; novo endpoint síncrono e schema JSON de retorno. |
| F13 | [Gerar DARF como contribuinte — PCONT](https://docs.receitafederal.gov.br/apuracao-cbs/gerar-darf-contrib.html) | Documento 1.0, de 13/11/2026; novo endpoint síncrono e schema JSON de retorno. |
| F20 | [Manual de Integração Técnica — Receita Integra](https://cgibs.gov.br/upload/arquivos/202609/05164748-08-manual-de-integracao-tecnica-receita-integra-v-1-4.pdf) | Documento 1.4, de 17/07/2026; fonte primária obtida na revisão de 24/11. |
| F21 | [Manual do Desenvolvedor DeRE](https://cgibs.gov.br/upload/arquivos/202608/18211952-07-manual-do-desenvolvedor-v-1-0-2.pdf) | 1.0.2, §3.1.3, pp.17–18; exemplo oficial do mesmo Authorization Server Receita Integra: Basic, formulário e resposta. Não transfere payload/assinatura DeRE para CBS. |
| F22 | [Liberação de APIs CBS v2](https://www.gov.br/receitafederal/pt-br/assuntos/noticias/2026/novembro/receita-federal-libera-acesso-as-novas-apis-de-apuracao-da-cbs/) | Comunicado de 09/11; disponibilidade e geração de credenciais, sem URI REST PRR. |
| F23 | [Acesso ao Piloto CBS](https://www.gov.br/receitafederal/pt-br/assuntos/noticias/2026/novembro/receita-federal-amplia-acesso-ao-ambiente-de-producao-restrita-do-piloto-da-cbs) | Comunicado de 20/11; acesso humano e poderes de representação, sem URI REST de negócio. |
| F24 | [RFC 6749](https://www.rfc-editor.org/rfc/rfc6749.html) e [RFC 6750](https://www.rfc-editor.org/rfc/rfc6750.html) | Protocolo OAuth/Bearer: codificação de Basic, respostas, extensões e transporte do token; complementam F20/F21. |
| F25 | [Atualização das credenciais, 14/08/2026; revisão 16/10/2026](https://www.gov.br/receitafederal/pt-br/assuntos/noticias/2026/agosto/credenciais-de-acesso-as-api-da-reforma-tributaria-serao-atualizadas) | Credenciais compartilhadas entre APIs conforme autorizações; lista débitos e débitos PRR separadamente. Não qualifica uma rota única ou seletor de ambiente. |

Na AACBS-003, a [pesquisa específica de ambientes](evidencias/AACBS-003/tentativa-0001/PESQUISA-AMBIENTES.md) também conferiu diretamente o repositório público F11: o texto integral do PDF Receita Integra 1.4 é igual ao de F20. F23 confirma que Piloto/PRR usa documentos de homologação e Produção Beta usa documentos efetivos. As mesmas chaves não selecionam por si só um ambiente; a rota PRR continua sem confirmação nas fontes primárias atuais.

Schemas oficiais de retorno, publicados em F03–F06/F12–F13 como **Draft 2020-12, versão 1.0.0**, conferidos em 24/11/2026:

| ID | Arquivo oficial |
| --- | --- |
| F14 | [debito-retorno.schema.json](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/debito-retorno.schema.json) |
| F15 | [credito-retorno.schema.json](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/credito-retorno.schema.json) |
| F16 | [pagamento-retorno.schema.json](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/pagamento-retorno.schema.json) |
| F17 | [recolhimento-retorno.schema.json](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/recolhimento-retorno.schema.json) |
| F18 | [gerar-darf-ad-retorno.schema.json](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/gerar-darf-ad-retorno.schema.json) |
| F19 | [gerar-darf-contrib-retorno.schema.json](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/gerar-darf-contrib-retorno.schema.json) |

A [baseline da AACBS-001](fontes/2026-11-24/README.md) preserva 23 fontes públicas, com URL, bytes e SHA-256 em [MANIFESTO-FONTES.csv](fontes/2026-11-24/MANIFESTO-FONTES.csv). O material histórico F10 e o repositório JS F11 são referências anteriores; os contratos correntes usam os documentos capturados. Revalidar conteúdo antes da etapa dependente: F08 mudou sem novo rótulo. Versões documental, HTTP, JSON Schema e XML/XSD são independentes.

## Requisitos comprovados e preparativos do DEV

| Item | O que está comprovado | Preparativo / responsável |
| --- | --- | --- |
| Empresa | F02 exige credenciais válidas do estabelecimento matriz; as quatro consultas usam CNPJ base de 8 posições. F12/F13 orientam CNPJ de 14 posições para DARF, com divergência em exemplo RAD. | DEV indica empresa/cenário autorizado e confirma a identificação; dados reais ficam fora das massas versionadas. |
| Representação | F10 permite acesso gov.br e representação da pessoa jurídica pelo responsável ou procurador autorizado. | DEV providencia acesso e autorizações para gerar a credencial da empresa. |
| Credenciais | F20 confirma o mecanismo; preparo e verificação locais passaram em 24/11, e a AACBS-003 obteve resposta real 200 de autenticação em Beta. | Credencial DPAPI e metadados continuam em `.local/aacbs`, conforme ACESSO-LOCAL.md. Nenhum valor foi exibido ou transferido ao plano; o uso real ocorreu somente após autorização. Não presumir ACL restrita ou permissões para outras operações. |
| Token | F20 confirma POST HTTPS no emissor de produção, uma hora, dois tokens/hora e cache. F21/F24 qualificam Basic com form-encoding UTF-8 dos componentes e formulário `grant_type=client_credentials`; resposta `access_token`, `token_type`, `expires_in`. | Implementar conforme ACESSO-E-FLUXO.md na 003. Nenhum token solicitado na 001; não inventar scope CBS ou emissor PRR. |
| Certificado | As fontes lidas não comprovam A1, assinatura XML ou mTLS obrigatório para consumir CBS. Os cinco serviços do ciclo real da AACBS-003 funcionaram sem certificado de cliente e sem seleção automática de certificado. | Essa prova é limitada ao fluxo de débitos Beta consumido. Acesso ao portal para habilitação e transporte da API são assuntos distintos; não ampliar requisitos por suposição. |
| Ambiente real | Todas as páginas de endpoint atuais lidas publicam somente produção. A URL PRR de download registrada em 12/11 foi removida da fonte atual. | DEV e agente de contratos confirmam ambiente habilitado. PRR exige URL oficial; não derivar endpoint nem usar produção como fallback. |
| Conectividade | APIs usam HTTPS; arquivo final vem de URL assinada externa. | DEV prepara acesso HTTPS aos hosts efetivamente publicados e política de proxy; confirmar eventuais liberações no manual vigente. |
| Dados úteis | Retorno vazio é válido e não comprova o mapeamento de todos os campos. | DEV identifica cenário autorizado com dados CBS representativos; massas versionadas serão sintéticas ou anonimizadas. |
| Webhook | Opcional, público e HTTPS; validação inicial por HEAD e notificação POST JSON, sem autenticação ou assinatura adicional, segundo F02/F09. | Não é requisito do fluxo inicial por consulta explícita. Caso usado, infraestrutura receptora pertence à aplicação consumidora. |

## Operação e limites

- Cada abertura aceita devolve tíquete e estimativa; processamento pode durar até 240 minutos. A aplicação conserva tíquetes e decide quando consultar novamente. A DLL não mantém tarefa em segundo plano nem espera quatro horas em `Executar()`. [F02](https://docs.receitafederal.gov.br/apuracao-cbs/guiaapiassincrona.html)
- F09 explicita quatro aberturas por dia, por credencial e por API, para **débitos, créditos, pagamentos e recolhimentos**, reiniciadas à meia-noite; fuso e contabilização de tentativas inválidas não estão definidos. F02 agora também qualifica a credencial. Não aplicar essa cota aos DARFs por analogia nem esgotá-la em testes. [F09](https://docs.receitafederal.gov.br/apuracao-cbs/faq.html)
- A autenticação possui limite próprio: dois tokens por hora e validade de uma hora. O manual exige cache; confirmar detalhes de escopo da cota na 001 e coordenar testes/agentes para não renovar por instância nem disputar o limite real. [F20](https://cgibs.gov.br/upload/arquivos/202609/05164748-08-manual-de-integracao-tecnica-receita-integra-v-1-4.pdf)
- As quatro consultas são incrementais: quando há conclusão nos últimos três meses, o recorte se inicia na última atualização concluída, limitado a oito dias; sem esse histórico, começa no primeiro dia do mês corrente. Essas consultas não aceitam período arbitrário; PCONT possui `pa` próprio para geração de DARF. [F03](https://docs.receitafederal.gov.br/apuracao-cbs/debito.html), [F13](https://docs.receitafederal.gov.br/apuracao-cbs/gerar-darf-contrib.html)
- Sem webhook, enviar `urlRetorno` como string vazia. Ao concluir, obter o link e fazer GET direto para baixar JSON UTF-8, sem Bearer nem cabeçalhos adicionais de autenticação. Credenciais da API não podem seguir para o armazenamento externo. [F02](https://docs.receitafederal.gov.br/apuracao-cbs/guiaapiassincrona.html)
- O arquivo fica disponível por até 48 horas; a expiração específica do link deve ser observada. URL assinada é segredo temporário: não registrar seu conteúdo em logs/evidências. [F09](https://docs.receitafederal.gov.br/apuracao-cbs/faq.html)
- Persistência de XMLs de negócio e retomada pertencem ao consumidor, usando a API de gravação da DLL. Tokens, credenciais e URLs assinadas exigem tratamento protegido; XML não torna esses dados públicos.
- DARF RAD e PCONT são síncronos e geram documentos de arrecadação: exigem cenário explicitamente habilitado para teste. A integração planejada não efetua pagamento nem aciona PIX. Não repetir POST com resultado incerto ou gerar documentos apenas para exercitar erros/limites. [F12](https://docs.receitafederal.gov.br/apuracao-cbs/gerar-darf-ad.html), [F13](https://docs.receitafederal.gov.br/apuracao-cbs/gerar-darf-contrib.html)

## Mudanças e questões históricas de 12/11

- F03–F06 passaram de 1.2 para 1.3, com schemas JSON oficiais e revisão das tabelas/exemplos; F12/F13 foram acrescentadas ao catálogo. Inventário atual: oito endpoints de negócio e dois auxiliares.
- A antiga divergência de `niConsumidor` foi corrigida para 11/14 posições nas tabelas/schemas e exemplos atuais. Créditos agora usa `excedente` singular e `chave` obrigatória; seu exemplo deixou de usar `documento=1`. Pagamentos substituiu `ni_contribuinte` por `niContribuinte` e separou RAD/PCONT. Recolhimentos usa somente `tipo=1`, sem os antigos exemplos 2/3. Esses problemas não permanecem abertos como antes.
- Pagamentos/recolhimentos passaram a datas sem horário para arrecadação/vencimento nas tabelas/exemplos; há uma divergência residual no schema RAD de pagamentos, registrada abaixo. Exemplos de créditos também demonstram offset `-03:00`, além das frações de segundo.
- F08 removeu PRR, a orientação explícita de regeneração após expiração e a menção a download anterior como causa de 404. O registro de 12/11 é histórico; não prova suporte vigente nem repetibilidade do download.

## Lacunas e divergências a resolver

| ID | Evidência / dúvida | Responsável e gatilho |
| --- | --- | --- |
| L01 | Qualificada documentalmente por F20/F21/F24: POST do mesmo emissor, Basic com codificação normativa, form-urlencoded, grant fixo e campos da resposta. Scope CBS literal não publicado e não é adicionado por inferência. | Especificação em ACESSO-E-FLUXO.md; prova real na 003, depois de seus pré-requisitos. Nenhuma autenticação especulativa na 001. |
| L02 | Não há URL PRR vigente nas páginas de endpoint atuais. ACL de IP, credencial por ambiente e permissão CBS precisam de confirmação. | Agente de contratos na etapa 001; DEV define inicialmente empresa, ambiente e cenários. O executor verifica os pré-requisitos antes de cada teste, sem nova confirmação quando o contexto autorizado permanece igual. Ambiente ausente gera erro local, sem fallback para produção. |
| L03 | Questão antiga resolvida documentalmente: F06/F17 agora restringem recolhimentos a `tipo=1`; F04/F15 corrigiram o exemplo de documento. | Agente de contratos usa baseline atual na etapa 001 e registra enums/cardinalidades; manter esta linha como histórico. |
| L04 | Questão antiga de identificação/RAD-PCONT corrigida nas fontes atuais; F16 exige identificação específica por tipo e proíbe campos de outro ramo. | Agente de contratos formaliza a regra atual na etapa 001; testes cruzados nas etapas 002/004. |
| L05 | `excedente` singular agora em débitos/créditos; preservação de datas/offset, ausência, zeros obrigatórios e grupos continua requisito de contrato. | Agente de contratos especifica na etapa 001; implementação/testes nas etapas 002–004. |
| L06 | Reutilização de link, renovação e recuperação de download interrompido continuam sem contrato completo; F08 removeu as duas afirmações que motivaram o conflito anterior. | Agente de contratos na etapa 001; verificar no teste incremental autorizado, sem prometer repetibilidade nem consumir cota sem necessidade. |
| L07 | F10 usa endpoints v1 e limites de 2 aberturas/24h de disponibilidade, diferentes de F02/F09 atuais. | Agente de contratos mantém F10 apenas para habilitação/token não contraditos; adota v2 atual para negócio e registra qualquer novo conflito. |
| L08 | Faltam limites oficiais de tamanho/rate limit situação/link e idempotência. Os schemas fecham objetos e excluem null; OAuth tem exceção normativa para extensões. | Política explícita na especificação: rejeitar incompatibilidade fiscal sem perder campo silenciosamente; limites técnicos não são regra da Receita; POST ambíguo não é repetido. |
| L09 | HTML usa `dataMaximaPagamento`; schemas usam `dataVencimento`. | Proposta para aprovação: escolha exclusiva entre os nomes publicados, preservando o que chegou. Não afirmar conformidade simultânea das fontes. |
| L10 | Vencimento RAD no HTML é date e no schema date-time; PCONT continua date. | Proposta para aprovação: união restrita somente nesse campo RAD, preservando representação e registrando o desvio; sem conversão silenciosa. |
| L11 | Request RAD array1:N, retorno global objeto sem correlação por DF-e; semântica de agregação/atomicidade não esclarecida. | Preservar as duas formas; não inferir sucesso por item nem fragmentar POST. Teste real inicial unitário; lote real aguarda esclarecimento e cenário próprio. |
| L12 | Tabela/instruções CNPJ14 versus exemplo RAD8; descrições de códigos PCONT aludem a RAD. | Proposta para aprovação: CNPJ14 no XML/path DARF; códigos e mensagens preservados no contexto real da operação. Não testar tamanhos candidatos. |
| L13 | Limites/idempotência DARF e contrato de PDF não publicados. | Sem serviço/GET PDF ou pagamento; orçamento real explícito e sem analogia com cota das consultas. Restrição da implementação e dos testes posteriores. |
| L14 | Retorno real Beta de débitos trouxe ni com 14 dígitos da matriz, enquanto HTML F03 e schema F14 continuam exigindo oito; tíquete e raiz do CNPJ foram conferidos sem expor valores. | DEV aprovou aceitar 8/14 apenas em retArquivoDebitos/ni, preservando o valor integral e comparando a base com o pedido. Envios e demais famílias continuam com oito. [Evidência e impacto](evidencias/AACBS-003/tentativa-0001/PROPOSTA-NI-RETORNO.md). |

## Condição para testes reais e evidências

Testes reais serão incrementais, dentro da etapa liberada pelo DEV: **003** autenticação e ciclo de débitos; **004** demais consultas e DARF somente em cenário explicitamente habilitado; **005** consolidação da evidência e das lacunas, sem repetir casos válidos apenas para gastar cota. Configurar o arquivo privado conforme [ACESSO-LOCAL.md](ACESSO-LOCAL.md), confirmar empresa/ambiente/cenário, conectividade e saldo de cota antes do consumo. A autonomia de execução permanece limitada à etapa liberada e aos casos habilitados; credencial disponível não aprova a etapa seguinte.

A ausência de pré-requisito deve constar como pendência externa; teste simulado não pode ser descrito como homologação. Resultados anteriores só valem para a versão/contrato efetivamente testados; mudança material exige revalidação proporcional.

Registrar somente operação, ambiente, horário, resultado HTTP/situação, versão testada e identificador de correlação sanitizado, com comparação das propriedades esperadas. Não anexar resposta fiscal bruta, token, ClientSecret, URL assinada, CPF/CNPJ real ou certificado. Eventual massa útil será anonimizada antes de entrar no repositório.
