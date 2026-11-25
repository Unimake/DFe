# Revisão independente do executor RAD-01

Em 25/11/2026, revisei estaticamente o executor privado de SHA-256 `9E844A3A9542683E72798182A2A4AB550F915A19E885B52B6C7F3A0A995558D5`. O hash da DLL do runtime selecionado corresponde a `0CA4192C051F3B10A8248E2BBA17CA590EC672A4B8E7A0804928A03A36ED6DE1`. O modo de emissão exige o identificador RAD-01 vinculado ao hash autorizado da proposta cifrada. O executor confere a matriz, um único item RAD com chave de 44 dígitos e a opção total, sem valor parcial. O serviço tipado envia o array ao `POST /apuracao-cbs/v2/rad/{cnpj}` no ambiente Beta configurado.

O executor usa o mutex e a política de cache e cota OAuth do harness. A existência do registro dedicado bloqueia outra tentativa antes de autenticar. Antes de chamar `Executar()` uma vez, grava `EmissaoIniciada` sob DPAPI. Interrupção após esse ponto deixa um registro que impede repetição, inclusive quando não se conhece o resultado remoto. A rotina cifra e relê o XML de retorno e não faz pagamento, aciona PIX ou baixa PDF.

Nenhum achado impeditivo no executor examinado para esta tentativa específica. A revisão não executou o script, não leu o XML fiscal nem realizou chamadas. Este parecer é anterior ao resultado da tentativa e não comprova geração do DARF.

## Conferência após a tentativa autorizada

O [resultado sanitizado](RESULTADO-REAL-RAD01.json) e o [log da execução](real-rad01.log) coincidem. A emissão consumiu a tentativa autorizada para aquele hash. Houve uma autenticação aceita e uma invocação fiscal RAD, com HTTP 404 e sem código de aplicação ou XML validado. O registro indica releitura do journal compartilhado e preservação das operações anteriores. Os cinco testes no runtime congelado passaram; o XML cifrado e relido da [prova controlada](PROVA-CONTROLADA.json) foi gerado pelo transporte em memória e pertence somente à simulação.

Reconfirmei o hash do executor e consultei o visualizador com `-SomenteVerificar`. Ele leu o registro dedicado à proposta e mostrou `ResultadoIndeterminado` sem exibir os parâmetros fiscais. A revisão pós-tentativa não fez autenticação nem envio fiscal. Os dados disponíveis não permitem determinar se a Receita processou o pedido ou gerou um DARF. O bloqueio de repetição e o estado `BLOCKED` registrado no PDCA são coerentes com a necessidade de reconciliação da tentativa original.
