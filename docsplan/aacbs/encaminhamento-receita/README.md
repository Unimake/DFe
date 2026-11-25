# Material para encaminhar à Receita

Estado atual: **AGUARDANDO ATENDIMENTO**, conforme os dados fornecidos pelo DEV em 25/11/2026. O Codex não abriu atendimento nem enviou mensagem e não repetirá o envio.

## Protocolo e consulta do chamado

| Campo | Informação fornecida pelo DEV |
|---|---|
| Número do atendimento | 202601396763 |
| Origem | Receita Atende |
| Data | 25/11/2026 |
| Serviço | Apuração Assistida |
| Status | Aguardando atendimento |
| Abertura e consulta | [Portal Tributação sobre Consumo — categoria 146](https://consumo.tributos.gov.br/lista-grupo?categoria=146) |

O DEV confirmou que abriu o chamado nesse endereço e que deve consultá-lo ali. Estes dados foram registrados a partir de sua mensagem; não houve consulta ao portal ou confirmação independente de atualização do status. O horário da abertura não foi informado. O protocolo identifica o atendimento e não é tíquete fiscal ou prova de processamento da RAD-01. A AACBS-005 permanece BLOCKED nos critérios reais.

## Mensagem

A [minuta pública consolidada](SOLICITACAO-CONSOLIDADA.md) reúne RAD, pagamentos e recolhimentos e permanece como base histórica da preparação. O DEV recebeu também uma versão resumida em texto simples, limitada a 5.000 caracteres, antes de informar o envio; a minuta cifrada não é comprovação do texto exato submetido. Ela usa marcadores em lugar dos identificadores fiscais. A versão completa foi preparada sob DPAPI em `.local/aacbs/encaminhamento-receita`, com os dados da proposta RAD-01 já aprovada; não contém credenciais ou token.

Na raiz do repositório, na mesma conta Windows que protegeu os dados, o DEV pode abrir a versão completa em console privado:

```powershell
pwsh -NoProfile -File ".local/aacbs/encaminhamento-receita/Visualizar-Solicitacao.ps1"
```

O visualizador somente lê o texto protegido. A opção `-SomenteVerificar` confere sua integridade sem mostrar identificadores. Nenhum desses modos autentica, envia mensagem ou chama serviço. O [atendimento do Portal Piloto](https://piloto-cbs.tributos.gov.br/servico/atendimento/home) permanece como referência da pesquisa inicial; o canal efetivamente utilizado pelo DEV é o link de consulta registrado acima. O chamado já foi enviado e não deve ser reenviado.

## Anexos sanitizados, caso o atendimento solicite

| Arquivo | Utilidade |
|---|---|
| [Log original de pagamentos](../evidencias/AACBS-004/tentativa-0001/real-pagamentos.log) | HTTP/fase e horário da observação, sem corpo fiscal |
| [Log original de recolhimentos](../evidencias/AACBS-004/tentativa-0001/real-recolhimentos.log) | HTTP/fase e horário da observação, sem corpo fiscal |
| [Resultado RAD-01](../evidencias/AACBS-005/tentativa-0003/RESULTADO-REAL-RAD01.json) | Uma execução fiscal, HTTP 404 e ausência de retorno validado |
| [Metadados sanitizados do journal RAD](../evidencias/AACBS-005/tentativa-0003/JOURNAL-SANITIZADO.json) | Horário, estado indeterminado e hash do arquivo aprovado |

Enviar somente o que o atendimento precisar. Os GUIDs/hashes locais servem à integridade da evidência, não são protocolos da Receita. Não anexar `.local`, credenciais, tokens, journal completo, XML fiscal real, chave PIX ou URL temporária. O corpo desta mensagem já identifica a empresa e a NF-e na versão privada apropriada ao canal protegido.

## Depois do encaminhamento

Registrar protocolo, canal, data e a resposta no ponto de retomada do [encerramento técnico](../ENCERRAMENTO-TECNICO.md). O protocolo do atendimento não equivale a tíquete de consulta ou confirmação de geração de DARF. Preservar a resposta original com seus dados privados fora do Git; registrar aqui apenas a conclusão sanitizada e referência local. Não zerar cotas ou remover bloqueios enquanto se aguarda resposta.
