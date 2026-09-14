# Unimake.DFe - NF-e ABI - Etapa ABI-000: base do planejamento

> **Tipo:** PLANNING_BASELINE
> **Dependências:** nenhuma
> **Ambientes:** ENV-PLAN
> **Decisões registradas:** DEC-001 a DEC-005; as propostas serão fechadas em ABI-001
> **Manifesto:** docsplan/nfabi/plans/manifests/ABI-000.json
> **Orquestrador:** `$abi-000-orchestrator`
> **Regra:** execute somente esta etapa e pare após dossiê/PDCA.

## 1. Objetivo e valor executável

Gerar o contrato retomável do trabalho sem iniciar produto.

## 2. Definition of Ready

- A etapa não possui predecessora; manifesto e PDCA devem estar coerentes com `ABI-000`.
- Árvore de trabalho inspecionada; alterações preexistentes preservadas.
- Ler integralmente os dois MOCs e inventariar os XSDs em `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi`; recalcular SHA-256 e comparar com docsplan/nfabi/architecture/INTEGRATION-CATALOG.md.
- Registrar as decisões propostas para fechamento em ABI-001; nenhuma decisão proposta é considerada aprovada pela entrega desta baseline.

## 3. Skills e instructions

- Ler .agents/instructions/pdca-execution.instructions.md, .agents/instructions/model-routing.instructions.md e .agents/instructions/nfabi-execution.instructions.md.
- Usar o orquestrador da etapa, o plan-linter no encerramento e as instruções AGENTS.md do subtree tocado.

## 4. Escopo

Descoberta, arquitetura, etapas, riscos, manifests, orquestradores, dossiê, inventário hash das fontes e linter.

## 5. Fora de escopo

Código funcional, schemas no projeto, dependências, commit, push e publicação.

## 6. Subtrees permitidos

`docsplan/nfabi/`, `.agents/` e seção DevPlanner do README/AGENTS.

## 7. Contratos, invariantes e arquitetura

- Namespace oficial http://www.portalfiscal.inf.br/nfeabi, versão de schema 1.00, modelo fiscal 77 e processamento síncrono.
- Preservar compatibilidade binária, contratos públicos de arquivo e padrões do repositório; não modernizar stack nem introduzir dependência.
- Não cadastrar endpoint de produção enquanto a autoridade fiscal não o publicar. Nunca copiar senha, certificado ou XML fiscal real para logs/evidence.
- A aprovação aceita a base, não fecha decisões propostas.

## 8. Partes PDCA e roteamento

| Parte | Fase | Perfil | Responsabilidade | Saída |
|---|---|---|---|---|
| ABI-000-P01 | Plan | DEEP | reler fontes, inventariar call sites e fechar readiness | checkpoint e matriz de impacto |
| ABI-000-P02 | Do | DEEP | executar somente o escopo autorizado | incremento da etapa |
| ABI-000-P03 | Check | INDEPENDENT_REVIEW | testes, diff, contratos e revisão independente quando crítica | relatório de gate |
| ABI-000-P04 | Act | ECONOMY | dossiê, hashes, PDCA e parada | DELIVERED_FOR_REVIEW |

## 9. Execução detalhada

1. Inspecionar MOCs/XSDs e os padrões documentais aplicáveis. 2. Corrigir e sincronizar o pacote de planejamento. 3. Validar placeholders, caracteres de controle, referências, links, hashes e estado. 4. Arquivar o dossiê e entregar.

## 10. Compatibilidade, migration e rollback

Remover somente o pacote DevPlanner desta tentativa ou reverter sua seção delimitada; produto permanece intacto.

## 11. Validação específica

| Ordem | Comando/cenário | Ambiente | Timeout | Resultado esperado | Artefato |
|---:|---|---|---:|---|---|
| 1 | `pwsh -NoProfile -File .agents/skills/plan-linter/scripts/test-plan.ps1 -RepositoryRoot .` | ENV-PLAN | 15 min | linter retorna 0 | docsplan/nfabi/plans/evidence/ABI-000/evidence/validation.txt |

## 12. Testes

Somente linter e revisão de links; testes do produto são proibidos.

## 13. Segurança, privacidade e observabilidade

- Transporte com certificado e proxy segue infraestrutura existente; nenhum secret entra em source, plano, log ou screenshot.
- Evidência externa deve ser sanitizada. Falhas distinguem rejeição fiscal de DNS, TLS, proxy, certificado e configuração.
- Testes online fiscais só ocorrem com autorização e ambiente preparado; nenhuma autorização real é repetida como sonda.

## 14. Critérios mensuráveis

| Critério | Gate | Como medir |
|---|---|---|
| Escopo | PASS | diff somente nos subtrees permitidos |
| Integridade textual | PASS | zero placeholder malformado e zero caractere de controle proibido |
| Referências | PASS | links locais válidos, decisões existentes e orquestradores `$abi-000-orchestrator` a `$abi-006-orchestrator` corretos |
| Fontes | PASS | caminhos normativos explícitos e hashes dos 2 MOCs e 20 XSDs registrados |
| Validação | PASS | linter retorna 0 e o diff completo permanece restrito a planejamento |

## 15. Stop/Blocked

- Bloquear se a fonte normativa estiver inacessível, se o inventário hash não puder ser reproduzido, se o linter permanecer vermelho ou se a correção exigir alteração de produto.
- Não enfraquecer validação, certificado ou teste para obter verde. Não iniciar a sucessora.

## 16. Definition of Done

Plano substancial, linter verde, etapa entregue e `ABI-001` não iniciada.

## 17. Dossiê, evidence e retomada

- Pasta: docsplan/nfabi/plans/evidence/ABI-000/.
- Criar no início com IN_PROGRESS, AttemptId e checkpoint.
- Arquivar o dossiê completo em attempts/AttemptId/; retrabalho usa novo AttemptId.
