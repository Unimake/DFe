# Evidência mínima por etapa AACBS

Criar um dossiê em `evidencias/<etapa>/tentativa-NNNN/ENTREGA.md`. Usar somente as seções necessárias para demonstrar a entrega; registrar como não aplicável o que não pertence à etapa. Planejamento não exige build ou teste de código. Nenhum campo é preenchido com um resultado presumido.

Uma tentativa corresponde a uma entrega para revisão. Correções durante a execução podem ser reunidas nela; retrabalho solicitado após a entrega abre uma nova tentativa, preservando o dossiê anterior. Acrescentar a aprovação real ao dossiê entregue quando recebida, sem alterar os resultados que a fundamentaram.

Não arquivar segredos, certificados, senhas, PIN, tokens, payloads fiscais reais ou exceções com dados identificáveis. Logs completos e arquivos auxiliares só entram quando necessários para demonstrar o resultado, após sanitização.

---

# Entrega `<AACBS-NNN>` — `<título>`

## Identificação e base

| Campo | Registro |
|---|---|
| Etapa e tentativa | `<AACBS-NNN / tentativa-NNNN>` |
| Estado da entrega | `<IN_PROGRESS / BLOCKED / DELIVERED_FOR_REVIEW / REWORK / APPROVED>` |
| Início e entrega | `<datas; horário e fuso quando relevantes>` |
| Base Git | `<branch e HEAD; identificar alterações preexistentes preservadas>` |
| Liberação | `<pedido inicial ou aprovação real da predecessora, com data>` |
| Ambiente | `<SDK/runtime e configuração relevantes; offline ou ambiente real autorizado>` |
| Fontes e decisões | `<referências qualificadas, versão/data e decisões aplicáveis>` |
| Agentes | `<papel, modelo e esforço efetivos; ou execução sem subagentes>` |

## Entrega e critérios

Descrever o comportamento entregue, os principais arquivos e o vínculo com os endpoints ou contratos da etapa. Registrar separadamente qualquer mudança de escopo autorizada.

| Critério da etapa | Evidência verificável | Resultado |
|---|---|---|
| `<critério do PLANO.md>` | `<arquivo, teste, comando ou referência>` | `<atendido / não atendido / não aplicável; motivo>` |

## Trabalho paralelo e conteúdo validado

- Propriedade de arquivos: `<responsável → arquivos/subárvores; transferências relevantes>`.
- Checkpoint: `<HEAD, lista dos arquivos relevantes e SHA-256 dos conteúdos validados; arquivo auxiliar apenas se necessário>`.
- Validade: `<confirmar que nenhum arquivo ou artefato usado mudou durante build/testes; registrar repetição dos checks afetados>`.
- Para etapa somente documental: `<fontes e documentos revisados; checks de código não aplicáveis>`.

## Verificação executada

| Comando ou cenário | Base/ambiente | Exit code ou resultado | Evidência e limite |
|---|---|---|---|
| `<comando exato com filtro, sem segredos>` | `<checkpoint e configuração>` | `<código; aprovados/falhos/ignorados>` | `<resumo ou log sanitizado quando necessário>` |

Descrever as falhas relevantes, sua causa, a correção e o resultado da reexecução. Diferenciar falha ambiental de regressão. Não apresentar testes do pacote anterior ou de outro checkpoint como validação do incremento entregue.

Testes reais ficam separados dos offline. Quando executados, registrar autorização existente, ambiente, operação, data, status HTTP e resultado estrutural sanitizado, incluindo a correlação necessária para demonstrar o fluxo sem expor tickets, URLs temporárias, tokens ou dados fiscais. Quando não executados, declarar a pendência e seu impacto nos critérios de aceite.

## Revisão técnica

Registrar autor/contexto da revisão, achados verificáveis e tratamento. Para cada problema impeditivo, indicar a correção e os checks repetidos. Uma revisão sem achados vale somente para o escopo e o conteúdo efetivamente revisados.

## Limitações, retomada e reversão

- Pendências: `<o que falta, responsável ou condição de resolução e efeito no aceite>`.
- Próximo checkpoint: `<como retomar esta etapa ou o retrabalho>`.
- Reversão: `<como retirar somente esta entrega preservando alterações anteriores e do usuário; não executar remoção por rotina>`.
- Sucessora: `<confirmar que não iniciou antes da aprovação humana>`.

## Gate humano

- Entregue para revisão em: `<data>`.
- Decisão do DEV: `AGUARDANDO` até uma resposta humana efetiva.
- Após resposta: `<aprovação ou pedido de retrabalho; data e referência/trecho fiel da mensagem>`.
- Atualização do PDCA: `<transição registrada e tentativa correspondente>`.

A aprovação desta etapa libera a próxima conforme o plano. Registrar o início da próxima em outra transição, sem exigir segunda confirmação. Para AACBS-005, a aprovação encerra o plano, sem autorizar publicação NuGet ou expansão IBS.
