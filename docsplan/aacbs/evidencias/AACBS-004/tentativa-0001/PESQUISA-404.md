# Pagamentos e recolhimentos — HTTP 404 na abertura em Produção Beta

Em 24/11/2026 o executor registrou HTTP 404 em uma abertura de pagamentos e uma de recolhimentos. Nas duas tentativas, a DLL identificou uma resposta incompatível com o contrato e o roteiro registrou abertura ambígua. A pesquisa abaixo usou somente documentação pública.

## Requisições documentadas

| Operação | Método | Caminho no host de produção `api.receitafederal.gov.br` | Fonte |
| --- | --- | --- | --- |
| ConsultarPagamentos | POST | `/apuracao-cbs/v2/pagamentos/{cnpj}` | [Pagamentos, seção 4](https://docs.receitafederal.gov.br/apuracao-cbs/pagamentos.html) |
| ConsultarRecolhimentos | POST | `/apuracao-cbs/v2/recolhimentos/{cnpj}` | [Recolhimentos, seção 4](https://docs.receitafederal.gov.br/apuracao-cbs/recolhimentos.html) |

Conferi o host HTTPS, a versão, os segmentos no plural e o método no recurso `Servicos/Config/AACBS/AN.xml`. Os dois serviços usam a base de oito dígitos do CNPJ na rota, o transporte configura a autorização Bearer e envia JSON. O corpo do roteiro sem webhook é `{"urlRetorno":""}`, conforme o exemplo da seção 4.4 das duas páginas. Os hashes das duas fontes técnicas constam na [revalidação da etapa](REVALIDACAO-FONTES.csv).

## Publicações sobre a implantação

| Data | Alcance do comunicado |
| --- | --- |
| 14/09/2026 | A Receita previu a disponibilização gradual de pagamentos e recolhimentos no início de novembro para Produção Restrita e Produção Beta. [Cronograma da Receita Federal](https://www.gov.br/receitafederal/pt-br/assuntos/noticias/2026/setembro/receita-federal-publica-nova-documentacao-tecnica-das-apis-de-apuracao-de-cbs) |
| 09/11/2026 | A Receita confirmou a liberação de débitos e créditos em 28/10 e anunciou outros serviços para novembro. [Comunicado de liberação](https://www.gov.br/receitafederal/pt-br/assuntos/noticias/2026/novembro/receita-federal-libera-acesso-as-novas-apis-de-apuracao-da-cbs/) |
| 17/11/2026 | A Receita informou que a entrega do RAD naquela data era documental e que comunicaria a disponibilidade operacional posteriormente. A publicação inclui expressamente a consulta de recolhimentos como adquirente e a geração do DARF RAD. O comunicado trata apenas do RAD. [Documentação da API do RAD](https://www.gov.br/receitafederal/pt-br/assuntos/noticias/2026/novembro/virando-a-chave-para-a-reforma-tributaria-api-para-emissao-do-rad-ja-esta-disponivel/) |

A pesquisa até 24/11/2026 não localizou confirmação pública de ativação efetiva de nenhuma das duas consultas no Beta. As evidências demonstram a correspondência do cliente com os contratos publicados e o HTTP observado em cada tentativa. A causa dos 404 e o processamento das aberturas permanecem indeterminados.
