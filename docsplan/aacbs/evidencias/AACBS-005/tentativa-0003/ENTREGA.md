# AACBS-005 — Execução real autorizada da RAD-01

Estado BLOCKED na prova real, 25/11/2026: RAD-01 recebeu HTTP 404 sem retorno fiscal validado; resultado indeterminado e reconciliação pendente. O DEV revisou os parâmetros privados pelo visualizador, confirmou ser adquirente e autorizou uma única tentativa de geração do DARF RAD em Produção Beta, opção PAGAMENTO_TOTAL, para a proposta cifrada com SHA-256 D663880BB7351232C3896C18BD5D60F16B1A42E41D3C259F3E6294F74056F4D1. Resposta explícita: "Sim. Confirmo. Acabei de pesquisar para ver."

Limites: no máximo uma execução fiscal de GerarDarfAdquirente para o arquivo exato; nenhum PCONT, consulta adicional, pagamento, PIX ou download de PDF. Sem retry automático, inclusive se o retorno for perdido. Usar cache OAuth e limite de emissão sob o mutex compartilhado, registrando intenção antes do POST fiscal em journal DPAPI dedicado e protegendo retorno. O estado das consultas anteriores deve ser preservado.

A autorização é pontual e vinculada ao hash; não habilita geração geral na configuração local. O XML, os dados fiscais, token e credenciais permanecem somente em .local/aacbs. Não publicar valores ou corpo fiscal nas evidências.

Os binários da saída compartilhada foram alterados por outros trabalhos desde a validação anterior. Para não disputar builds nem consumir um arquivo substituído durante a execução, foi criada uma cópia estável de 50 arquivos do runtime Debug em .local/aacbs/cenarios-darf005, com hashes idênticos entre origem e destino antes/depois da cópia. Essa cópia será validada offline e usada em processo isolado; não é pacote NuGet nem fluxo de empacotamento paralelo.

## Verificações anteriores ao envio

O modo Preflight validou o hash da proposta, matriz do XML contra a configuração local, uma única solicitação com PAGAMENTO_TOTAL e ausência do campo parcial, construção do serviço público e o hash do runtime. Não existia journal RAD-01 e nenhum token válido estava em cache. O modo Offline usou transporte em memória e um journal de prova separado, demonstrando uma execução, XML cifrado/relido e bloqueio da segunda execução sem request adicional. Nenhuma dessas provas usou rede.

Os cinco casos AACBSDarfContinuidadeTest passaram sobre o runtime congelado: [runtime-testes.log](runtime-testes.log). A primeira preparação da cópia não incluía os XMLs sintéticos do diretório AACBS/Resources; os cinco casos falharam por arquivos ausentes, antes de qualquer execução real. As fixtures necessárias foram copiadas e seus hashes conferidos, e a classe passou na repetição. O log preliminar fica ignorado. Não houve alteração de código nem build concorrente. [Prova controlada](PROVA-CONTROLADA.json).

A [revisão independente do executor](REVISAO-EXECUTOR.md) verificou o vínculo com a autorização, o uso do mutex/cache/cota e a gravação de intenção antes da execução, sem reenviar após interrupção. O código temporário do executor permanece em .local/aacbs/cenarios-darf005/Executar-RAD01.ps1 e não integra a DLL ou o dossiê público.

## Resultado real e limite da conclusão

[Resultado sanitizado](RESULTADO-REAL-RAD01.json) e [log final](real-rad01.log): uma autenticação aceita (HTTP 200, condição necessária para o cache fornecer o token), seguida de **uma execução fiscal RAD com HTTP 404**. A resposta não produziu XML fiscal validado nem código de aplicação reconhecido. Não é possível afirmar que o DARF foi gerado ou que não houve processamento remoto.

O journal DPAPI dedicado foi persistido como **ResultadoIndeterminado**. Seu caminho é derivado do hash da proposta aprovada e sua existência bloqueia outra execução antes mesmo da autenticação. O journal compartilhado de consultas foi relido; os registros de negócio anteriores permaneceram integralmente iguais. O cache novo foi preservado com a validade e a contagem de emissão. Nenhum POST foi repetido, nenhum PCONT foi executado e não houve pagamento, PIX ou download de PDF.

A configuração geral continua sem habilitação de geração de DARF; a autorização usada foi somente a exceção expressa para o hash RAD-01. Tokens e segredos nunca foram colocados em argumentos ou evidências. Não existe XML de retorno real validado a entregar nesta tentativa; os XMLs da prova offline são sintéticos e não contam como DARF real.

## Retomada

A prova real RAD fica **bloqueada para reconciliação**. Esclarecer com a Receita o processamento da única chamada e a disponibilidade/habilitação operacional da rota POST /apuracao-cbs/v2/rad/{cnpj}, além dos 404 já observados em pagamentos/recolhimentos. Sucesso OAuth não demonstra permissão nessa operação. Não trocar rota/ambiente, inferir inexistência do serviço ou tratar o 404 como motivo para retry. Uma nova autorização, por si, também não esclarece se houve geração anterior; reconciliar primeiro.

PCONT continua sem cenário no CNPJ atual, conforme informado pelo DEV. PRR permanece adiada. O preparo e a cobertura offline das tentativas anteriores permanecem históricos, sem transformar esta resposta em emissão comprovada. Nenhum commit, pacote ou publicação foi executado.

A conferência posterior, somente leitura, confirmou um token válido em cache e a existência do ledger RAD-01, sem request adicional. O visualizador foi atualizado para ler o estado existente e passou a mostrar ResultadoIndeterminado, preservando o mesmo hash aprovado. [Metadados sanitizados](JOURNAL-SANITIZADO.json). Há [texto pronto para reconciliação](SOLICITACAO-RECONCILIACAO.md), não enviado, com o horário original e perguntas objetivas ao suporte. A revisão independente pós-execução foi acrescentada ao parecer.
