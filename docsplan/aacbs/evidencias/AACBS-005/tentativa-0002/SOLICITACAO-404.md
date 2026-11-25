# Pedido de esclarecimento dos HTTP 404 — texto preparado

Não enviado pelo Codex. Encaminhar pelo [atendimento autenticado do Piloto](https://piloto-cbs.tributos.gov.br/servico/atendimento/home) ou canal técnico Receita Integra indicado pela Receita. Não incluir ClientSecret, token, XML fiscal ou URL assinada. A identificação necessária da empresa deve ser fornecida somente no canal oficial protegido.

> Estamos integrando as consultas v2 da Apuração Assistida da CBS em Produção Beta. Com a mesma credencial e no mesmo host, autenticação e abertura de créditos funcionaram, enquanto as duas requisições abaixo receberam HTTP 404 com resposta incompatível com o contrato de erro documentado:
>
> - POST /apuracao-cbs/v2/pagamentos/{cnpj-base}, observado em 24/11/2026 às 21:58:43 (UTC−03), equivalente a 25/11/2026 00:58:43 UTC.
> - POST /apuracao-cbs/v2/recolhimentos/{cnpj-base}, observado em 24/11/2026 às 21:59:15 (UTC−03), equivalente a 25/11/2026 00:59:15 UTC.
>
> Usamos CNPJ-base de oito dígitos da matriz, Authorization Bearer, Content-Type/Accept application/json e corpo sem webhook com urlRetorno vazio. As rotas correspondem às seções 4 dos documentos oficiais atuais. Não repetimos as aberturas nem recebemos tíquete confirmado para essas operações.
>
> Solicitamos confirmar, para cada método/rota: disponibilidade operacional atual em Beta, eventual habilitação específica da credencial/contribuinte e se as chamadas originais chegaram ao backend ou criaram solicitações. A correlação preservada pelo nosso consumidor é local e não foi enviada ao servidor; não temos request ID remoto nas evidências sanitizadas. Podemos fornecer a identificação da empresa no canal protegido para rastreamento pelos horários.
>
> O comunicado da Receita de 17/11 descreve a entrega do RAD como documental e de uso futuro; a notícia do Ministério da Fazenda atualizada em 24/11 usa linguagem de disponibilização, sem identificar ambiente ou data de ativação. Qual a referência de liberação operacional aplicável a essas consultas?

O pedido esclarece a causa e reconcilia as tentativas originais sem gastar cota por reenvio. Ausência de resposta não autoriza apagar a ambiguidade, afirmar que não houve processamento ou mudar para outra versão/ambiente.

Atualização de 25/11/2026: para o encaminhamento atual, usar a [solicitação única consolidada](../../../encaminhamento-receita/SOLICITACAO-CONSOLIDADA.md) e seu [guia de envio](../../../encaminhamento-receita/README.md), reunindo RAD, pagamentos e recolhimentos. Este rascunho permanece como histórico e não foi enviado.