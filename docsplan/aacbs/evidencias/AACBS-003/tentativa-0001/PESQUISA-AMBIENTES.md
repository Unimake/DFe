# Pesquisa dos ambientes CBS — 24/11/2026

## Resultado

Os ambientes Produção Restrita/Piloto e Produção Beta são distintos. As mesmas credenciais podem acessar serviços de ambos, conforme autorização; isso não qualifica uma URL única nem permite deduzir o ambiente a partir da origem das chaves. Não foi encontrado, nas fontes primárias atuais consultadas, o endpoint de negócio v2 de Produção Restrita ou um parâmetro/cabeçalho que selecione esse ambiente na rota de produção.

| Fonte primária conferida | Evidência e limite |
|---|---|
| [Receita — ampliação do Piloto, 20/11/2026](https://www.gov.br/receitafederal/pt-br/assuntos/noticias/2026/novembro/receita-federal-amplia-acesso-ao-ambiente-de-producao-restrita-do-piloto-da-cbs/) | Piloto usa documentos dos autorizadores de homologação; Beta usa documentos efetivos dos autorizadores de produção. O portal Piloto é `https://piloto-cbs.tributos.gov.br/`, mas endereço de portal não é endpoint da API. |
| [Receita — credenciais, 14/08/2026, atualização 16/10/2026](https://www.gov.br/receitafederal/pt-br/assuntos/noticias/2026/agosto/credenciais-de-acesso-as-api-da-reforma-tributaria-serao-atualizadas) | Credenciais compartilhadas entre APIs, respeitadas as autorizações; lista separadamente Consultar débitos e [Produção Restrita] Consultar débitos. Não informa o roteamento HTTP. |
| [Documentação atual de débitos v1.3, 13/11/2026](https://docs.receitafederal.gov.br/apuracao-cbs/debito.html) | Publica somente URL de Produção: `POST https://api.receitafederal.gov.br/apuracao-cbs/v2/debitos/{cnpj}`. CNPJ base no path, Bearer no Authorization e urlRetorno no corpo; nenhum seletor de ambiente documentado. |
| [Guia assíncrono v1.1](https://docs.receitafederal.gov.br/apuracao-cbs/guiaapiassincrona.html) | Referencia manual Receita Integra no repositório público e reconhece ambientes distintos; não define rota PRR. |
| [Receita Integra 1.4 no CGIBS](https://cgibs.gov.br/upload/arquivos/202609/05164748-08-manual-de-integracao-tecnica-receita-integra-v-1-4.pdf) e [repositório público da Receita](https://arquivos.receitafederal.gov.br/) | Seção Ambientes publica somente Authorization Server Produção `https://api.receitafederal.gov.br/token`. Isso é autenticação, não a rota fiscal PRR. |

## Conferência do repositório público

Listagem pública WebDAV de `Documentos/Técnicos/Receita Integra` localizou somente `Receita Integra - Documentação Técnica - Prod - v1.4.pdf`. Nenhuma credencial particular foi usada: a identificação Basic usada na listagem é o identificador do compartilhamento público do próprio site, com senha vazia.

O PDF do repositório tem 445358 bytes e SHA-256 `261F50B5A1BECF9B047CF0045ED075337823BE5B8445EE4FC2304EE0944DE632`. Seus bytes diferem da cópia CGIBS qualificada na AACBS-001, mas o texto extraído integralmente das seis páginas com pypdf é idêntico. Não há nova definição de ambiente nesse arquivo. A cópia pública consultada está neste dossiê; a baseline aprovada não foi sobrescrita.

## Consequência para a execução

- A configuração local permanece Produção Restrita. Nenhuma chave foi usada nesta pesquisa e nenhuma API fiscal foi chamada.
- Não preencher homologação com a URL de produção nem introduzir seletor de ambiente inventado. Não tratar uma resposta HTTP de rota especulativa como qualificação do ambiente.
- O desenvolvimento e os testes offline da AACBS-003 continuam. O critério real depende de rota PRR oficial atual, ou de uma decisão explícita do DEV para mudar o ambiente configurado para Produção Beta.
- Se o DEV optar pelo Beta, o roteiro continuará limitado ao fluxo de débitos já autorizado, com journal, cache, orçamento de chamadas e DARF desabilitado. A seleção explícita exige atualizar os metadados locais de ambiente e permissão; não exige recifrar ou exibir a credencial existente. Este registro não altera permissões ou configuração, nem comprova autorização remota do par de chaves.

O que falta confirmar com a Receita é concreto: **qual é a URL base v2 de Produção Restrita para débitos, situação e link, e se o token é obtido no mesmo `/token` com as mesmas credenciais e autorizações**. Não é necessário enviar ClientSecret para esclarecer isso.

## Decisão posterior à pesquisa

O DEV autorizou expressamente os testes em Produção Beta. A configuração local foi atualizada para esse ambiente, sem alterar ou exibir as chaves, com os limites da AACBS-003 e DARF desabilitado. Essa decisão permite usar as rotas de produção publicadas; não qualifica rotas PRR por analogia.
