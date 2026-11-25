# Esclarecimento da única tentativa RAD-01 — texto não enviado

Para o canal técnico oficial da Receita Integra, sem anexar credencial, token, XML fiscal real ou chave PIX:

> Em 25/11/2026, às 10:06:53 UTC−03 (13:06:53 UTC), iniciamos uma única requisição POST /apuracao-cbs/v2/rad/{cnpj-completo}, em Produção Beta, para um item com opção PAGAMENTO_TOTAL. O contribuinte confirmou a condição de adquirente antes da tentativa. A autenticação foi aceita e a execução fiscal recebeu HTTP 404, sem resposta compatível com o contrato fiscal documentado. Não recebemos código de aplicação ou XML validado e não repetimos a emissão.
>
> Solicitamos rastrear a chamada original, esclarecer se chegou ao backend e se houve geração de DARF. Favor confirmar a publicação operacional e as autorizações necessárias dessa rota em Beta. A identificação do contribuinte e do DF-e pode ser fornecida pelo responsável no canal oficial protegido. A correlação que temos é local; não há request ID remoto nas evidências.
>
> A tentativa ficou indeterminada e bloqueada para reconciliação. Não há pagamento ou nova emissão autorizados por este pedido de suporte.

A [evidência sanitizada do journal](JOURNAL-SANITIZADO.json) fixa o horário e a referência do arquivo aprovado; a [entrega](ENTREGA.md) registra o limite das conclusões. O texto foi somente preparado e não constitui mensagem enviada ou resposta da Receita.

Atualização de 25/11/2026: para o encaminhamento atual, usar a [solicitação única consolidada](../../../encaminhamento-receita/SOLICITACAO-CONSOLIDADA.md) e seu [guia de envio](../../../encaminhamento-receita/README.md), reunindo RAD, pagamentos e recolhimentos. Este rascunho permanece como histórico e não foi enviado.