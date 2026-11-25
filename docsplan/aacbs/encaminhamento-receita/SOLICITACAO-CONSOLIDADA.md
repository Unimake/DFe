# Solicitação à Receita — minuta pública, não enviada

**Assunto: CBS v2 em Produção Beta — reconciliação de RAD e esclarecimento de HTTP 404 em três operações**

Prezados,

Estamos integrando as APIs v2 de Apuração Assistida da CBS. Solicitamos apoio para rastrear três chamadas em Produção Beta e esclarecer a disponibilidade operacional e a habilitação necessária para cada rota.

Identificação para o atendimento protegido:

- Empresa/matriz: [CNPJ_MATRIZ]
- Base usada nas consultas: [CNPJ_BASE]
- NF-e da única tentativa de RAD: [CHAVE_NFE_RAD01]

O CNPJ e a chave acima devem ser fornecidos somente no canal oficial protegido. Não integram esta cópia pública. Não encaminharemos ClientSecret, token OAuth, chave PIX, XML fiscal completo ou URL assinada.

## Chamadas a rastrear

Host: `https://api.receitafederal.gov.br`. Os horários abaixo são das observações/início das execuções locais e permitem correlacionar os registros do gateway e do backend; não são timestamps de request IDs remotos.

| Operação | Método e caminho | Horário em Brasília (UTC−03) | Equivalente UTC | Resultado observado |
|---|---|---|---|---|
| Consulta de pagamentos | POST /apuracao-cbs/v2/pagamentos/{cnpj-base} | 24/11/2026 21:58:43 | 25/11/2026 00:58:43 | HTTP 404, resposta incompatível com o contrato documentado; sem tíquete confirmado |
| Consulta de recolhimentos | POST /apuracao-cbs/v2/recolhimentos/{cnpj-base} | 24/11/2026 21:59:15 | 25/11/2026 00:59:15 | HTTP 404, resposta incompatível com o contrato documentado; sem tíquete confirmado |
| Geração de DARF RAD | POST /apuracao-cbs/v2/rad/{cnpj-completo} | 25/11/2026 10:06:53 | 25/11/2026 13:06:53 | HTTP 404; sem código de aplicação ou XML fiscal validado; resultado indeterminado |

Nas duas consultas usamos a base de oito dígitos da matriz, `Authorization: Bearer`, `Content-Type: application/json`, `Accept: application/json` e corpo `{"urlRetorno":""}`. Não repetimos essas aberturas. No mesmo contexto de acesso, a abertura e o ciclo de créditos funcionaram. Isso comprova somente aquele fluxo, não a habilitação das três operações investigadas.

No RAD usamos o CNPJ completo de quatorze dígitos, a mesma autenticação OAuth, um array com exatamente uma solicitação, a chave da NF-e identificada acima e `opcaoPagamento=PAGAMENTO_TOTAL`, sem campo de valor parcial. O contribuinte conferiu a proposta e confirmou ser adquirente. Houve uma única tentativa fiscal, precedida de autenticação aceita, e não houve reenvio. Nenhum pagamento foi efetuado.

Preservamos os registros locais e bloqueamos novas tentativas para não duplicar processamento. Não temos request ID remoto nos registros sanitizados nem corpo/cabeçalhos originais suficientes para atribuir os 404 ao gateway ou ao backend. Nossas correlações são locais e não foram enviadas como headers; não as apresentamos como identificadores rastreáveis pela Receita.

## Esclarecimentos solicitados

1. As três rotas/métodos estavam publicados e operacionais em Produção Beta nos horários indicados? Qual é sua situação operacional atual e eventual cronograma de liberação?
2. Cada chamada chegou ao backend ou foi recusada pelo gateway? Qual foi a causa do HTTP 404?
3. As aberturas de pagamentos/recolhimentos criaram solicitações? Se sim, como recuperar seus tíquetes sem repetir o POST?
4. A única chamada RAD gerou DARF ou outro registro de negócio? Se sim, qual o procedimento oficial para consultar/recuperar o documento e reconciliar a tentativa, sem nova emissão?
5. Para reconciliar o RAD, podem confirmar, de forma vinculada à chamada original, se não houve processamento de negócio nem geração de DARF ou outro registro? Se houve processamento ou registro, qual o desfecho e o procedimento oficial para reconciliar seus efeitos? A simples ausência de documento localizado não esclarece um pedido ainda pendente; uma resposta inconclusiva manterá nosso bloqueio. Somente depois da reconciliação definiremos com o contribuinte uma eventual nova tentativa.
6. Há habilitação, autorização ou escopo específico da credencial/contribuinte para cada operação? Como conferir/corrigir o acesso preservando a credencial já funcional nas consultas v2, quando possível?
7. Qual documentação ou contrato operacional atual se aplica a essas três rotas no Beta? A comunicação da Receita de 17/11 apresentou a entrega do RAD como documental e de uso futuro; a notícia do Ministério da Fazenda atualizada em 24/11 usa linguagem de disponibilização sem identificar a ativação por ambiente. Solicitamos esclarecer a referência operacional aplicável.

Agradecemos a orientação e o número de protocolo para acompanhar o esclarecimento. Esta solicitação é de diagnóstico e reconciliação; não autoriza nova geração, pagamento ou alteração de dados.

Atenciosamente,
Unimake — equipe de integração

## Referências públicas

- [Contrato de pagamentos](https://docs.receitafederal.gov.br/apuracao-cbs/pagamentos.html)
- [Contrato de recolhimentos](https://docs.receitafederal.gov.br/apuracao-cbs/recolhimentos.html)
- [Contrato do RAD](https://docs.receitafederal.gov.br/apuracao-cbs/gerar-darf-ad.html)
- [Comunicado da Receita de 17/11](https://www.gov.br/receitafederal/pt-br/assuntos/noticias/2026/novembro/virando-a-chave-para-a-reforma-tributaria-api-para-emissao-do-rad-ja-esta-disponivel)
- [Notícia do Ministério da Fazenda](https://www.gov.br/fazenda/pt-br/assuntos/noticias/2026/novembro/receita-federal-disponibiliza-api-para-emissao-do-recolhimento-como-adquirente-rad)
