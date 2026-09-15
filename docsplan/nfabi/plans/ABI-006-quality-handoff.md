# Unimake.DFe - NF-e ABI - Etapa ABI-006: qualidade final e compatibilidade pública da DLL

> **Tipo:** EXECUTION
> **Dependências:** ABI-005 APPROVED
> **Ambientes:** ENV-DLL
> **Decisões necessárias:** DEC-004 e DEC-005
> **Manifesto:** docsplan/nfabi/plans/manifests/ABI-006.json
> **Orquestrador:** `$abi-006-orchestrator`
> **Regra:** execute somente esta etapa e pare após dossiê/PDCA.

## 1. Objetivo e valor executável

Concluir a qualidade da DLL Unimake.DFe, com API/INTEROP, build, testes focados e documentação de uso comprovados.

## 2. Definition of Ready

- Predecessora aprovada e manifesto coerente com o PDCA.
- Árvore de trabalho inspecionada; alterações preexistentes preservadas.
- Ler integralmente os dois MOCs e os XSDs aplicáveis em `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi`; recalcular SHA-256 e comparar com docsplan/nfabi/architecture/INTEGRATION-CATALOG.md.
- Decisões listadas fechadas ou contingência explicitamente autorizada.

## 3. Skills e instructions

- Ler .agents/instructions/pdca-execution.instructions.md, .agents/instructions/model-routing.instructions.md e .agents/instructions/nfabi-execution.instructions.md.
- Usar o orquestrador da etapa, o plan-linter no encerramento e as instruções AGENTS.md do subtree tocado.

## 4. Escopo

Auditoria final de XML/schema/API/INTEROP, exemplos mínimos, build da DLL e testes NFeABI focados.

## 5. Fora de escopo

NuGet, tag, commit/push e qualquer produto consumidor externo à DLL.

## 6. Subtrees permitidos

Correções NFeABI estritamente necessárias, testes e documentação de uso.

## 7. Contratos, invariantes e arquitetura

- Namespace oficial http://www.portalfiscal.inf.br/nfeabi, versão de schema 1.00, modelo fiscal 77 e processamento síncrono.
- Preservar compatibilidade binária, contratos públicos de arquivo e padrões do repositório; não modernizar stack nem introduzir dependência.
- Não cadastrar endpoint de produção enquanto a autoridade fiscal não o publicar. Nunca copiar senha, certificado ou XML fiscal real para logs/evidence.
- Não mascarar falha ambiental da suíte completa; registrar testes não executados.

## 8. Partes PDCA e roteamento

| Parte | Fase | Perfil | Responsabilidade | Saída |
|---|---|---|---|---|
| ABI-006-P01 | Plan | DEEP | reler fontes, inventariar call sites e fechar readiness | checkpoint e matriz de impacto |
| ABI-006-P02 | Do | BALANCED | executar somente o escopo autorizado | incremento da etapa |
| ABI-006-P03 | Check | INDEPENDENT_REVIEW | testes, diff, contratos e revisão independente quando crítica | relatório de gate |
| ABI-006-P04 | Act | ECONOMY | dossiê, hashes, PDCA e parada | DELIVERED_FOR_REVIEW |

## 9. Execução detalhada

1. Revisar diff e API pública. 2. Executar build e testes NFeABI. 3. Validar INTEROP e exemplos da DLL quando aplicável. 4. Produzir relatório final com hashes e contratos.

## 10. Compatibilidade, migration e rollback

Reverter somente correções finais; não publicar artifact.

## 11. Validação específica

| Ordem | Comando/cenário | Ambiente | Timeout | Resultado esperado | Artefato |
|---:|---|---|---:|---|---|
| 1 | build DLL e testes NFeABI focados | ENV-DLL | 15 min | todos os gates determinísticos da DLL verdes | docsplan/nfabi/plans/evidence/ABI-006/evidence/validation.txt |

## 12. Testes

Round-trip/schema/serviços/config, negativos, API pública e INTEROP quando disponível.

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

Qualidade final da DLL comprovada por build, testes, revisão de API/INTEROP, hashes e limitações claras, sem publicação iniciada.

## 17. Dossiê, evidence e retomada

- Pasta: docsplan/nfabi/plans/evidence/ABI-006/.
- Criar no início com IN_PROGRESS, AttemptId e checkpoint.
- Arquivar o dossiê completo em attempts/AttemptId/; retrabalho usa novo AttemptId.
