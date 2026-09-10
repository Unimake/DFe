# Unimake.DFe - NF-e ABI - Etapa ABI-004: modelos de eventos NF-e ABI

> **Tipo:** EXECUTION
> **Dependências:** ABI-003 APPROVED
> **Ambientes:** ENV-DLL
> **Decisões necessárias:** DEC-001 e DEC-004
> **Manifesto:** docs/plans/manifests/ABI-004.json
> **Orquestrador:** $(System.Collections.Hashtable.Id.ToLowerInvariant())-orchestrator
> **Regra:** execute somente esta etapa e pare após dossiê/PDCA.

## 1. Objetivo e valor executável

A DLL representa e valida cancelamento, pagamento de parcela e apropriação de crédito individual, inclusive procEvento.

## 2. Definition of Ready

- Predecessora aprovada e manifesto coerente com o PDCA.
- Árvore de trabalho inspecionada; alterações preexistentes preservadas.
- Ler integralmente os dois MOCs e os XSDs aplicáveis em $SourceRoot; recalcular SHA-256 e comparar com docs/architecture/INTEGRATION-CATALOG.md.
- Decisões listadas fechadas ou contingência explicitamente autorizada.

## 3. Skills e instructions

- Ler .agents/instructions/pdca-execution.instructions.md, .agents/instructions/model-routing.instructions.md e .agents/instructions/nfabi-execution.instructions.md.
- Usar o orquestrador da etapa, o plan-linter no encerramento e as instruções AGENTS.md do subtree tocado.

## 4. Escopo

eventoNFeABI, retEvento, procEvento e detalhes 110111, 112110 e 112120 conforme MOC/XSD.

## 5. Fora de escopo

Recepção remota sem endpoint publicado e tarefas UniNFe.

## 6. Subtrees permitidos

Modelos/validator e testes NFeABI.

## 7. Contratos, invariantes e arquitetura

- Namespace oficial http://www.portalfiscal.inf.br/nfeabi, versão de schema 1.00, modelo fiscal 77 e processamento síncrono.
- Preservar compatibilidade binária, contratos públicos de arquivo e padrões do repositório; não modernizar stack nem introduzir dependência.
- Não cadastrar endpoint de produção enquanto a autoridade fiscal não o publicar. Nunca copiar senha, certificado ou XML fiscal real para logs/evidence.
- Id de evento, sequência, chNFeABI, cOrgaoAutor, tpAutor, timestamps e regras de ocorrência devem seguir o XSD.

## 8. Partes PDCA e roteamento

| Parte | Fase | Perfil | Responsabilidade | Saída |
|---|---|---|---|---|
| ABI-004-P01 | Plan | DEEP | reler fontes, inventariar call sites e fechar readiness | checkpoint e matriz de impacto |
| ABI-004-P02 | Do | BALANCED | executar somente o escopo autorizado | incremento da etapa |
| ABI-004-P03 | Check | INDEPENDENT_REVIEW | testes, diff, contratos e revisão independente quando crítica | relatório de gate |
| ABI-004-P04 | Act | ECONOMY | dossiê, hashes, PDCA e parada | DELIVERED_FOR_REVIEW |

## 9. Execução detalhada

1. Implementar envelope/retorno/processado. 2. Implementar os três detalhes. 3. Cobrir round-trip, Id e regras negativas.

## 10. Compatibilidade, migration e rollback

Reverter somente classes/fixtures de eventos, mantendo modelo principal.

## 11. Validação específica

| Ordem | Comando/cenário | Ambiente | Timeout | Resultado esperado | Artefato |
|---:|---|---|---:|---|---|
| 1 | build DLL + classe focada de eventos NFeABI | ENV-DLL | 15 min | três eventos e retornos validam/round-trip | docs/plans/evidence/ABI-004/evidence/validation.txt |

## 12. Testes

Casos positivos dos três eventos e negativos de tpEvento, nSeq, campos condicionais e precisão monetária.

## 13. Segurança, privacidade e observabilidade

- Transporte com certificado e proxy segue infraestrutura existente; nenhum secret entra em source, plano, log ou screenshot.
- Evidência externa deve ser sanitizada. Falhas distinguem rejeição fiscal de DNS, TLS, proxy, certificado e configuração.
- Testes online fiscais só ocorrem com autorização e ambiente preparado; nenhuma autorização real é repetida como sonda.

## 14. Critérios mensuráveis

| Critério | Gate | Como medir |
|---|---|---|
| Escopo | PASS | diff somente nos subtrees permitidos |
| Contrato XML/ERP | PASS | fixture representativa preserva estrutura, ordem, namespace e nomes |
| Regressão | PASS | build e testes focados verdes; limitações ambientais registradas |
| Fontes | PASS | hashes comparados e divergências analisadas antes de codificar |

## 15. Stop/Blocked

- Bloquear se a documentação externa mudar, se um contrato público depender de decisão aberta, se o endpoint necessário não estiver publicado ou se o ambiente obrigatório faltar.
- Não enfraquecer validação, certificado ou teste para obter verde. Não iniciar a sucessora.

## 16. Definition of Done

Contratos de eventos disponíveis sem fingir endpoint remoto; testes e dossiê verdes.

## 17. Dossiê, evidence e retomada

- Pasta: docs/plans/evidence/ABI-004/.
- Criar no início com IN_PROGRESS, AttemptId e checkpoint.
- Arquivar o dossiê completo em attempts/AttemptId/; retrabalho usa novo AttemptId.
