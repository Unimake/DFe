# AACBS — Fontes e requisitos de acesso

Levantamento documental em **12/11/2026**, sem chamadas às APIs de negócio. Sigla técnica proposta: **AACBS — Apuração Assistida da CBS**. O escopo inicial compreende somente o conjunto CBS indicado pelo DEV; IBS depende de documentação e planejamento futuros.

Este documento registra fatos e pendências para a etapa **AACBS-001**. Não aprova etapas nem comprova habilitação, autenticação ou consumo real.

## Fontes de referência

| ID | Fonte oficial | Versão identificada e uso |
| --- | --- | --- |
| F01 | [Página inicial da apuração CBS](https://docs.receitafederal.gov.br/apuracao-cbs/) | Catálogo consultado em 12/11/2026; identifica este conjunto como CBS. |
| F02 | [Regras comuns das APIs assíncronas](https://docs.receitafederal.gov.br/apuracao-cbs/guiaapiassincrona.html) | Documento 1.1, de 14/10/2026; fluxo comum, autenticação, webhook e download. |
| F03 | [Débitos](https://docs.receitafederal.gov.br/apuracao-cbs/debito.html) | Documento 1.2, de 14/10/2026; endpoint API v2 e arquivo final. |
| F04 | [Créditos](https://docs.receitafederal.gov.br/apuracao-cbs/credito.html) | Documento 1.2, de 14/10/2026; endpoint API v2 e arquivo final. |
| F05 | [Pagamentos](https://docs.receitafederal.gov.br/apuracao-cbs/pagamentos.html) | Documento 1.2, de 14/10/2026; endpoint API v2 e arquivo final. |
| F06 | [Recolhimentos](https://docs.receitafederal.gov.br/apuracao-cbs/recolhimentos.html) | Documento 1.2, de 14/10/2026; endpoint API v2 e arquivo final. |
| F07 | [Situação da solicitação](https://docs.receitafederal.gov.br/apuracao-cbs/situacao-solicitacao.html) | Documento 1.2, de 14/10/2026; estados e erros. |
| F08 | [Link de download](https://docs.receitafederal.gov.br/apuracao-cbs/download.html) | Documento 1.0, de 13/10/2026; links de produção e produção restrita. |
| F09 | [Dúvidas frequentes](https://docs.receitafederal.gov.br/apuracao-cbs/faq.html) | Sem versão explícita; limites, janela incremental e características do webhook. |
| F10 | [Manual Plataforma CBS — maio/2026](https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/manuais/reforma-tributaria-do-consumo/manual-plataforma-cbs-21-maio-2026-07h40.pdf) | Páginas 6–9 e 13: representação, geração de credenciais e token. Contratos de negócio v1 estão superados pelas páginas atuais v2. |
| F11 | [Repositório oficial — Receita Integra](https://arquivos.receitafederal.gov.br/index.php/s/gn672Ad4CF8N6TK?dir=/Documentos/T%C3%A9cnicos/Receita+Integra) | Destino indicado por F02 para o manual técnico de autenticação; conteúdo dos arquivos ainda não conferido. |

Na etapa 001, registrar a revisão efetivamente lida, data, links e hashes dos documentos técnicos obtidos. Revalidar as fontes antes de implementar uma etapa dependente; mudança de contrato exige análise de impacto e eventual revisão do DEV. A versão documental, a versão da API na URL e a versão do XML/XSD da DLL são identificadores distintos.

## Requisitos comprovados e preparativos do DEV

| Item | O que está comprovado | Preparativo / responsável |
| --- | --- | --- |
| Empresa | F02 exige credenciais válidas do estabelecimento matriz; aberturas usam CNPJ base de 8 posições. | DEV indica empresa autorizada para testes e seu CNPJ matriz; dados reais ficam fora das massas versionadas. |
| Representação | F10 permite acesso gov.br e representação da pessoa jurídica pelo responsável ou procurador autorizado. | DEV providencia acesso e autorizações para gerar a credencial da empresa. |
| Credenciais | F10 orienta gerar ClientId/ClientSecret pelo serviço de geração de credencial no [portal](https://consumo.tributos.gov.br/). | DEV gera credenciais com acesso CBS e as fornece por configuração local protegida. Não enviar segredos em chat, plano ou evidências. |
| Token | F02 publica OAuth2 Client Credentials; F10 publica `https://api.receitafederal.gov.br/token` e validade de uma hora. | Agente de contratos confirma o manual técnico vigente; a DLL fará autenticação automaticamente nos serviços. |
| Certificado | As fontes lidas não comprovam A1, assinatura XML ou mTLS obrigatório para consumir CBS. | Não exigir certificado da empresa como pré-requisito de transporte sem confirmação oficial; acesso ao portal e autenticação da API são assuntos distintos. |
| Ambiente real | F03–F07 publicam produção; F08 também publica produção restrita para download. | DEV e agente de contratos confirmam ambiente habilitado para a empresa. Não derivar endpoints de homologação por substituição de texto. |
| Conectividade | APIs usam HTTPS; arquivo final vem de URL assinada externa. | DEV prepara acesso HTTPS aos hosts efetivamente publicados e política de proxy; confirmar eventuais liberações no manual vigente. |
| Dados úteis | Retorno vazio é válido e não comprova o mapeamento de todos os campos. | DEV identifica cenário autorizado com dados CBS representativos; massas versionadas serão sintéticas ou anonimizadas. |
| Webhook | Opcional, público e HTTPS; validação inicial por HEAD e notificação POST JSON, sem autenticação ou assinatura adicional, segundo F02/F09. | Não é requisito do fluxo inicial por consulta explícita. Caso usado, infraestrutura receptora pertence à aplicação consumidora. |

## Operação e limites

- Cada abertura aceita devolve tíquete e estimativa; processamento pode durar até 240 minutos. A aplicação conserva tíquetes e decide quando consultar novamente. A DLL não mantém tarefa em segundo plano nem espera quatro horas em `Executar()`. [F02](https://docs.receitafederal.gov.br/apuracao-cbs/guiaapiassincrona.html)
- F09 explicita quatro aberturas por dia por credencial e por API, reiniciadas à meia-noite; o fuso e a contabilização de tentativas inválidas não estão definidos. F02 resume o limite sem qualificar a credencial. Não testar limites esgotando a cota real. [F09](https://docs.receitafederal.gov.br/apuracao-cbs/faq.html)
- As consultas são incrementais: quando há conclusão nos últimos três meses, o recorte se inicia na última atualização concluída, limitado a oito dias; sem esse histórico, começa no primeiro dia do mês corrente. Não existe parâmetro documentado para escolher período arbitrário. [F03](https://docs.receitafederal.gov.br/apuracao-cbs/debito.html)
- Sem webhook, enviar `urlRetorno` como string vazia. Ao concluir, obter o link e fazer GET direto para baixar JSON UTF-8, sem Bearer nem cabeçalhos adicionais de autenticação. Credenciais da API não podem seguir para o armazenamento externo. [F02](https://docs.receitafederal.gov.br/apuracao-cbs/guiaapiassincrona.html)
- O arquivo fica disponível por até 48 horas; a expiração específica do link deve ser observada. URL assinada é segredo temporário: não registrar seu conteúdo em logs/evidências. [F09](https://docs.receitafederal.gov.br/apuracao-cbs/faq.html)
- Persistência de XMLs de negócio e retomada pertencem ao consumidor, usando a API de gravação da DLL. Tokens, credenciais e URLs assinadas exigem tratamento protegido; XML não torna esses dados públicos.

## Lacunas e divergências a resolver

| ID | Evidência / dúvida | Responsável e gatilho |
| --- | --- | --- |
| L01 | F11 ainda não conferido. F02/F10 não especificam suficientemente body/content type do POST token, Basic versus credenciais no corpo, escopos e resposta de expiração. Não inferir esses detalhes. | Agente de contratos na etapa 001; DEV obtém documento/habilitação caso o acesso público continue insuficiente. Bloqueia congelar autenticação e iniciar seu código. |
| L02 | Não há conjunto completo publicado de URLs de produção restrita nas fontes lidas. ACL de IP, credencial por ambiente e permissão CBS precisam de confirmação. | Agente de contratos na etapa 001; DEV confirma acesso antes da etapa 005. Ambiente ausente deve gerar erro local explícito, sem fallback para produção. |
| L03 | F06 tabela permite `tipo` 1/2, mas exemplo contém 3. F04 exemplo contém `documento=1`, fora da tabela. | Agente de contratos registra resolução documental na etapa 001; domínio controverso não vira enum/XSD fechado sem justificativa revisável. |
| L04 | `niConsumidor` é descrito como 14 posições e CPF/CNPJ, enquanto exemplos usam 8. F05 mostra `niAdquirente` vazio e campos condicionais RAD presentes também em PCONT. | Agente de contratos na etapa 001 define obrigatoriedade e tipos com fonte/resolução; DEV decide premissa residual de impacto público. |
| L05 | F04 usa `cbs.excedentes`; F03 usa `cbs.excedente`. Datas exemplificam microssegundos e UTC. Valores opcionais zero podem ser omitidos; listas vazias são válidas. | Agente de contratos especifica preservação na etapa 001; implementação/testes nas etapas 002–004. Não uniformizar nomes nem perder precisão, ausência ou grupos. |
| L06 | F08 admite renovar link expirado, mas cita download já realizado como causa de arquivo indisponível. Reutilização, retomada e recuperação de download interrompido não estão completamente definidos. | Agente de contratos na etapa 001; experimento controlado na etapa 005 somente com cenário autorizado. Não prometer repetibilidade. |
| L07 | F10 usa endpoints v1 e limites de 2 aberturas/24h de disponibilidade, diferentes de F02/F09 atuais. | Agente de contratos mantém F10 apenas para habilitação/token não contraditos; adota v2 atual para negócio e registra qualquer novo conflito. |
| L08 | Não foram publicados nas páginas lidas: limites de tamanho do arquivo, política completa de `null`, extensões futuras, rate limit de situação/link e idempotência do POST. | Agente de contratos na etapa 001 define limites técnicos explícitos e testes sintéticos, sem apresentá-los como regra da Receita. POST com resultado incerto não é repetido automaticamente. |

## Condição para testes reais e evidências

Testes reais pertencem à etapa 005, após as aprovações anteriores e a autorização de execução dessa etapa. Preparar credenciais, ambiente confirmado, empresa/cenário autorizado, conectividade e saldo de cota. A ausência de qualquer item deve constar como pendência externa; teste simulado não pode ser descrito como homologação.

Registrar somente operação, ambiente, horário, resultado HTTP/situação, versão testada e identificador de correlação sanitizado, com comparação das propriedades esperadas. Não anexar resposta fiscal bruta, token, ClientSecret, URL assinada, CPF/CNPJ real ou certificado. Eventual massa útil será anonimizada antes de entrar no repositório.
