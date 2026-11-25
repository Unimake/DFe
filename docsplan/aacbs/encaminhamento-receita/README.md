# Material para encaminhar à Receita

Estado atual: **ENVIADO PELO DEV — AGUARDANDO RESPOSTA DA RECEITA**. Em 25/11/2026, após preparar a versão em texto de até 5.000 caracteres, o DEV informou: "Enviado, agora temos que aguardar." Canal efetivamente usado, horário e protocolo não foram informados neste chat. O Codex não abriu atendimento nem enviou mensagem e não repetirá o envio.

## Mensagem

A [minuta pública consolidada](SOLICITACAO-CONSOLIDADA.md) reúne RAD, pagamentos e recolhimentos e permanece como base histórica da preparação. O DEV recebeu também uma versão resumida em texto simples, limitada a 5.000 caracteres, antes de informar o envio; a minuta cifrada não é comprovação do texto exato submetido. Ela usa marcadores em lugar dos identificadores fiscais. A versão completa foi preparada sob DPAPI em `.local/aacbs/encaminhamento-receita`, com os dados da proposta RAD-01 já aprovada; não contém credenciais ou token.

Na raiz do repositório, na mesma conta Windows que protegeu os dados, o DEV pode abrir a versão completa em console privado:

```powershell
pwsh -NoProfile -File ".local/aacbs/encaminhamento-receita/Visualizar-Solicitacao.ps1"
```

O visualizador somente lê o texto protegido. A opção `-SomenteVerificar` confere sua integridade sem mostrar identificadores. Nenhum desses modos autentica, envia mensagem ou chama serviço. O DEV copia o texto e encaminha pelo [atendimento do Portal Piloto](https://piloto-cbs.tributos.gov.br/servico/atendimento/home) ou pelo canal técnico Receita Integra orientado pela Receita. O acesso autenticado ao portal e o envio são ações do DEV.

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
