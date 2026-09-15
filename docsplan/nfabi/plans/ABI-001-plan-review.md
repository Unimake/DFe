# Unimake.DFe - NF-e ABI - Etapa ABI-001: revisão decisória e congelamento

> **Tipo:** PLAN_REVIEW
> **Dependências:** ABI-000 APPROVED
> **Ambientes:** ENV-PLAN
> **Decisões necessárias:** todas as decisões propostas do DEV
> **Manifesto:** docsplan/nfabi/plans/manifests/ABI-001.json
> **Orquestrador:** `$abi-001-orchestrator`
> **Regra:** execute somente esta etapa e pare após dossiê/PDCA.

## 1. Objetivo e valor executável

Fechar escopo remoto, prefixo e critérios de aceite; tornar `ABI-002` executável.

## 2. Definition of Ready

- Predecessora aprovada e manifesto coerente com o PDCA.
- Árvore de trabalho inspecionada; alterações preexistentes preservadas.
- Ler integralmente os dois MOCs e os XSDs aplicáveis em `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi`; recalcular SHA-256 e comparar com docsplan/nfabi/architecture/INTEGRATION-CATALOG.md.
- Decisões listadas fechadas ou contingência explicitamente autorizada.

## 3. Skills e instructions

- Ler .agents/instructions/pdca-execution.instructions.md, .agents/instructions/model-routing.instructions.md e .agents/instructions/nfabi-execution.instructions.md.
- Usar o orquestrador da etapa, o plan-linter no encerramento e as instruções AGENTS.md do subtree tocado.

## 4. Escopo

Rodadas curtas com o DEV, propagação das respostas, possível renumeração única, revisão independente e linter.

## 5. Fora de escopo

Qualquer alteração em `source/`, schemas incorporados ou artefactos binários.

## 6. Subtrees permitidos

`docsplan/nfabi/`, `.agents/` e seção DevPlanner do README/AGENTS.

## 7. Contratos, invariantes e arquitetura

- Namespace oficial http://www.portalfiscal.inf.br/nfeabi, versão de schema 1.00, modelo fiscal 77 e processamento síncrono.
- Preservar compatibilidade binária, contratos públicos de arquivo e padrões do repositório; não modernizar stack nem introduzir dependência.
- Não cadastrar endpoint de produção enquanto a autoridade fiscal não o publicar. Nunca copiar senha, certificado ou XML fiscal real para logs/evidence.
- Não entregar enquanto decisão material do DEV permanecer OPEN/PROPOSED.

## 8. Partes PDCA e roteamento

| Parte | Fase | Perfil | Responsabilidade | Saída |
|---|---|---|---|---|
| ABI-001-P01 | Plan | DEEP | reler fontes, inventariar call sites e fechar readiness | checkpoint e matriz de impacto |
| ABI-001-P02 | Do | DEEP | executar somente o escopo autorizado | incremento da etapa |
| ABI-001-P03 | Check | INDEPENDENT_REVIEW | testes, diff, contratos e revisão independente quando crítica | relatório de gate |
| ABI-001-P04 | Act | ECONOMY | dossiê, hashes, PDCA e parada | DELIVERED_FOR_REVIEW |

## 9. Execução detalhada

1. Manter o prefixo `ABI` e os IDs candidatos `ABI-000` a `ABI-006`, sem renumeração. 2. Manter consulta e eventos como contratos XML, sem transporte enquanto não houver endpoint publicado. 3. Aplicar INTEROP às classes públicas equivalentes aos DFes vizinhos, sem criar superfície COM artificial para tipos internos. 4. Manter testes fiscais online como opt-in autorizado, nunca como gate determinístico único. 5. Propagar as decisões; o congelamento dos IDs torna-se efetivo somente após `ABI-001 APPROVED`.

## 10. Compatibilidade, migration e rollback

Restaurar a revisão anterior do pacote; nenhum ID 002+ é congelado antes da aprovação.

## 11. Validação específica

| Ordem | Comando/cenário | Ambiente | Timeout | Resultado esperado | Artefato |
|---:|---|---|---:|---|---|
| 1 | `pwsh -NoProfile -File .agents/skills/plan-linter/scripts/test-plan.ps1 -RepositoryRoot .` | ENV-PLAN | 15 min | zero decisão material aberta e linter retorna 0 | docsplan/nfabi/plans/evidence/ABI-001/evidence/validation.txt |

## 12. Testes

Revisão independente de cobertura, causalidade, manifests e primeira etapa.

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

Decisões materiais fechadas e pacote entregue para revisão com código intocado. O plano e os IDs candidatos tornam-se `READY_FOR_EXECUTION`/congelados somente após aprovação humana da ABI-001.

## 17. Dossiê, evidence e retomada

- Pasta: docsplan/nfabi/plans/evidence/ABI-001/.
- Criar no início com IN_PROGRESS, AttemptId e checkpoint.
- Arquivar o dossiê completo em attempts/AttemptId/; retrabalho usa novo AttemptId.
