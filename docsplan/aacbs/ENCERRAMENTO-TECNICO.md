# Encerramento técnico da AACBS — consolidação de 25/11/2026

**Entrega técnica consolidada para revisão do DEV. Validação real integral ainda bloqueada.** A AACBS-005 é a última etapa do plano; não há etapa 006 implícita. O [PDCA](PDCA.md) permanece a fonte do estado e das aprovações. Este documento não aprova a etapa, não declara sucesso dos testes reais pendentes e não autoriza novas chamadas.

O DEV solicitou consolidar as pendências, preparar a solicitação à Receita e as condições de retomada, sem repetir chamadas, emitir DARFs ou enviar mensagens externas. Este encerramento é documental: zero chamadas fiscais, autenticações ou envios ao atendimento. A proposta/journals e seus bloqueios foram preservados.

## Entrega técnica comprovada

| Entrega | Evidência e alcance |
|---|---|
| Dez serviços públicos, XML para o consumidor e transformação interna para API | Autenticação, quatro aberturas de consulta, situação, obtenção de link, download e dois DARFs. [Guia e 24 exemplos](uso/README.md), [catálogo XML](contratos/CATALOGO-XML.csv). O consumidor controla espera e retomada. |
| Serialização, XSDs, validação e compatibilidade | 24 raízes XML, 25 XSDs, catálogo/configurações embutidas e validação de envio/retorno. NI 8/14 de débitos e créditos conforme decisões explícitas; envios e demais retornos preservam seus limites. [Auditoria de créditos](evidencias/AACBS-005/tentativa-0001/AUDITORIA-NI-CREDITOS.md). |
| Testes e compatibilidade da primeira entrega final | 499 testes Debug e 509 INTEROP, zero falhas/ignorados; conferência COM nativa das dez operações e DryRun oficial do pacote. Esses números pertencem ao checkpoint da [tentativa-0001](evidencias/AACBS-005/tentativa-0001/ENTREGA.md). |
| Regressões complementares | Cinco casos novos aprovados em Debug e INTEROP: cache/401 entre RAD e PCONT, leitura interrompida depois dos headers e 404 incompatíveis sem replay. [Tentativa-0002](evidencias/AACBS-005/tentativa-0002/ENTREGA.md). Não somar execuções para anunciar outra suíte completa. |
| Executor pontual e proteção da RAD-01 | Runtime congelado validado por cinco testes direcionados; prova offline de uma execução e bloqueio de duplicação, revisão independente, intenção e resultado sob DPAPI. [Tentativa-0003](evidencias/AACBS-005/tentativa-0003/ENTREGA.md). A prova controlada não equivale a DARF real. |

Nenhum defeito de produção foi demonstrado como causa dos 404 nas investigações realizadas. Isso não prova que não exista falha: as evidências disponíveis não identificam a camada ou causa remota. A revisão técnica anterior não substitui o aceite do DEV. Publicação NuGet, adaptação de UniNFe/PRO e IBS não integram este fechamento.

## Situação das provas reais em Produção Beta

| Operação/cenário | Comprovado | Pendente |
|---|---|---|
| Autenticação | Credencial aceita nas execuções registradas, com cache/cota protegidos | Não há verificação permanente de validade ou autorização para todos os serviços. |
| Débitos, situação, link e arquivo | Ciclo concluído; XML real vazio validado na 003 | Materialização com itens reais; grupos preenchidos têm cobertura sintética. [Entrega 003](evidencias/AACBS-003/tentativa-0001/ENTREGA.md). |
| Créditos, situação, link e arquivo | Resposta real com dois períodos e 27 créditos, NI14 integral, campos/ordem/XML/XSD conferidos; XML protegido e journal finalizado | Ajuste NI foi comprovado reprocessando o original cifrado, sem novo download remoto. [Prova](evidencias/AACBS-005/tentativa-0001/VERIFICACAO-REAL-CREDITOS.json). |
| Pagamentos e recolhimentos | Uma abertura por família recebeu HTTP 404 com resposta incompatível; rotas/contratos conferidos | Causa, chegada ao backend, existência de solicitações e recuperação de tíquetes. Depois, ciclo real válido. As tentativas permanecem ambíguas, sem replay. [Investigação](evidencias/AACBS-005/tentativa-0002/INVESTIGACAO-404.md). |
| RAD-01 | DEV confirmou parâmetros/adquirente e autorizou uma tentativa total; OAuth aceito e um POST fiscal recebeu HTTP 404 | **ResultadoIndeterminado**: não há prova de geração nem de ausência de processamento. O ledger bloqueia repetição. [Resultado](evidencias/AACBS-005/tentativa-0003/RESULTADO-REAL-RAD01.json). |
| PCONT | Implementação e provas controladas entregues | Nenhuma emissão real. DEV confirmou que o CNPJ atual não tem cenário/saldo; aguardamos empresa representada e período elegível. Não foi escolhido PA ou valor fictício. [Preparo](evidencias/AACBS-005/tentativa-0002/PREPARO-DARF.md). |

Produção Restrita está **adiada por decisão do DEV**, com [pesquisa e ponto de retomada preservados](evidencias/PESQUISA-PRR-2026-11-25/RESULTADO.md). Ela não é usada como fallback para contornar os 404 de Beta.

## Encaminhamento à Receita — enviado pelo DEV, aguardando resposta

A [solicitação consolidada](encaminhamento-receita/SOLICITACAO-CONSOLIDADA.md) substitui a necessidade de enviar separadamente os rascunhos das tentativas 002/003; os rascunhos anteriores permanecem históricos. Ela reúne três rotas, métodos, horários em Brasília/UTC, contexto de autenticação e perguntas sobre ativação, habilitação, tíquetes e eventual geração de DARF.

O [guia de encaminhamento](encaminhamento-receita/README.md) lista os anexos sanitizados mínimos e o comando para o DEV visualizar a versão completa sob DPAPI em console privado. Não enviar o checkout, pasta .local, journal completo ou segredos. A versão pública tem marcadores para CNPJ/chave; a privada já foi preenchida com a proposta aprovada. O Codex não acessou atendimento autenticado nem enviou mensagem.

Em 25/11/2026, após receber a versão resumida em texto de até 5.000 caracteres, o DEV informou que enviou a solicitação e que devemos aguardar. O protocolo, o canal efetivamente utilizado e o horário não foram informados no chat; não há resposta da Receita registrada. Acrescentar essas informações quando forem fornecidas, mantendo a resposta original com dados particulares fora do Git. Não repetir o envio. Não confundir protocolo de atendimento, GUID local e tíquete fiscal.

## Condições de retomada e responsáveis

| Pendência | Informação/condição indispensável | Ação permitida depois de satisfeita |
|---|---|---|
| RAD-01 indeterminada | Receita ou mecanismo oficial precisa esclarecer a chamada original, identificada por empresa/DF-e/horário, e o eventual documento gerado. Responsável pelo contato: DEV/representante; análise da resposta: orquestrador AACBS. | Se gerou, usar o procedimento oficial de recuperação/reconciliação, sem novo POST de emissão. Se a Receita confirmar, de forma vinculada à chamada original, que não houve processamento de negócio nem geração de DARF ou outro registro, guardar a confirmação e só planejar nova tentativa com parâmetros, orçamento e autorização específica do DEV. Se houve processamento/registro, reconciliar seu desfecho pelo procedimento oficial antes de avaliar nova emissão. Ausência de documento localizado, por si, não encerra a reconciliação; resposta inconclusiva mantém o bloqueio. |
| Pagamentos/recolhimentos | Confirmar por operação se as aberturas originais criaram solicitações; obter tíquete se existente, além de disponibilidade e habilitação da rota em Beta. | Retomar o tíquete confirmado no contexto correto. Se não houve criação e a operação estiver liberada/habilitada, definir uma nova abertura limitada com o DEV, preservando tentativas/cotas antigas. Não obter outro status com rota especulativa como substituto de reconciliação. |
| PCONT | DEV providenciar contribuinte representado com acesso autorizado, período com saldo e opção total/parcial; no parcial, valor. | Preparar XML concreto, validar offline e apresentar todos os parâmetros ao DEV. Só após confirmação específica executar uma tentativa e tratar resultado ambíguo sem reenvio. Acesso da empresa atual não comprova acesso de outra. |
| Débitos com itens reais | Massa autorizada e contexto em que o retorno contenha itens, respeitando incrementalidade e orçamento. | Comparar todos os campos do retorno com XML/XSD/Result. Não abrir solicitações repetidas somente para tentar obter itens. |
| PRR | DEV solicitar retomada e existir rota/contrato atual oficialmente qualificado, com autorizações e isolamento de ambiente confirmados. | Retomar a pesquisa preservada, implementar/validar o delta necessário dentro do gate autorizado e só então propor prova PRR sem fallback. |

A retomada de tráfego depende de orientação do DEV; receber resposta da Receita ou localizar uma rota não inicia execução automaticamente. Antes de qualquer retomada, conferir fonte atual, ambiente, identidade da empresa/credencial, cache, cotas e estado protegido. A confirmação posterior de disponibilidade não esclarece automaticamente uma chamada anterior. O atual comando de emissão RAD-01 é de uso único; **não remover, zerar, recifrar ou renomear artefatos para contornar seu ledger**. Uma eventual tentativa futura deverá ter registro próprio ligado à reconciliação documentada e à nova autorização, preservando o original.

Se houver alteração de contrato/código, repetir somente os checks afetados e revisar a evidência correspondente. Sem mudança, reutilizar a prova válida no seu checkpoint. Nenhuma geração ou pagamento passa a ser autorizado apenas porque a etapa técnica foi aceita.

## Aceite e limite do fechamento

A consolidação documental foi entregue para revisão. O estado da AACBS-005 continua **BLOCKED nos critérios reais**, sem aprovação integral presumida. O DEV pode revisar a entrega técnica e decidir sobre seu aceite com ressalvas explícitas; isso não transforma testes pendentes em sucessos, não reconcilia RAD-01 e não elimina a confirmação de cada emissão. Para encerrar a validação real integral, atender aos critérios reais acima e obter o aceite do DEV.

Não há novo tráfego fiscal, emissão de token/DARF, envio externo, alteração de configuração privada, desbloqueio de journal, build, pacote, commit ou publicação nesta consolidação. Mudanças externas no projeto/exemplos foram preservadas. Os deltas documentais e os testes complementares ainda pendentes no Git poderão ser commitados quando o DEV solicitar.

Conferência desta consolidação: [revisão independente sem achados pendentes](encaminhamento-receita/REVISAO-CONSOLIDACAO.md), [preparo cifrado conferido](encaminhamento-receita/VERIFICACAO-PREPARO.json) e [verificação documental](encaminhamento-receita/VERIFICACAO-DOCUMENTAL.json). O hash do ledger RAD permaneceu igual ao registro pós-execução. Foram examinados somente documentos e integridade de arquivos; nenhuma nova execução fiscal ou envio externo integrou a revisão.

## Acompanhamento após o envio informado pelo DEV

Situação atual: aguardando a Receita; protocolo ainda não informado. A informação de envio não reconcilia os três resultados nem libera consumo. Mantêm-se os bloqueios e a ausência de chamadas automáticas. Não foi criada automação de acompanhamento. A implementação e as provas offline previstas e possíveis com a massa atual estão entregues; o fechamento Git dos deltas posteriores aos quatro commits da tentativa-0001 continua pendente de solicitação do DEV. Outra execução de testes somente se uma mudança, falha ou preocupação concreta justificar.