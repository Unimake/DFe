# Unimake.DFe - NF-e ABI - Etapa ABI-002: schemas e fundação do tipo fiscal

> **Tipo:** EXECUTION
> **Dependências:** ABI-001 APPROVED
> **Ambientes:** ENV-DLL
> **Decisões necessárias:** DEC-001 e DEC-004
> **Manifesto:** docs/plans/manifests/ABI-002.json
> **Orquestrador:** `$abi-002-orchestrator`
> **Regra:** execute somente esta etapa e pare após dossiê/PDCA.

## 1. Objetivo e valor executável

A DLL reconhece tipo NFeABI/modelo 77 e valida fixtures brutas contra o pacote oficial embutido.

## 2. Definition of Ready

- Predecessora aprovada e manifesto coerente com o PDCA.
- Árvore de trabalho inspecionada; alterações preexistentes preservadas.
- Ler integralmente os dois MOCs e os XSDs aplicáveis em `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi`; recalcular SHA-256 e comparar com docs/architecture/INTEGRATION-CATALOG.md.
- Decisões listadas fechadas ou contingência explicitamente autorizada.

## 3. Skills e instructions

- Ler .agents/instructions/pdca-execution.instructions.md, .agents/instructions/model-routing.instructions.md e .agents/instructions/nfabi-execution.instructions.md.
- Usar o orquestrador da etapa, o plan-linter no encerramento e as instruções AGENTS.md do subtree tocado.

## 4. Escopo

Copiar exatamente os 20 XSDs, embutir recursos, registrar TipoDFe/ModeloDFe/serviços necessários e detecção/config de validação.

## 5. Fora de escopo

Modelo OO completo, serviços SOAP, endpoints de produção e qualquer arquivo do UniNFe.

## 6. Subtrees permitidos

Schemas NFeABI, enums/modelo/tipo, validação central, csproj e testes NFeABI.

## 7. Contratos, invariantes e arquitetura

- Namespace oficial http://www.portalfiscal.inf.br/nfeabi, versão de schema 1.00, modelo fiscal 77 e processamento síncrono.
- Preservar compatibilidade binária, contratos públicos de arquivo e padrões do repositório; não modernizar stack nem introduzir dependência.
- Não cadastrar endpoint de produção enquanto a autoridade fiscal não o publicar. Nunca copiar senha, certificado ou XML fiscal real para logs/evidence.
- Hash e nome de cada XSD devem coincidir com o catálogo; o diretório escrito pelo usuário foi resolvido para `PL_NFeABI_1.00`.

## 8. Partes PDCA e roteamento

| Parte | Fase | Perfil | Responsabilidade | Saída |
|---|---|---|---|---|
| ABI-002-P01 | Plan | DEEP | reler fontes, inventariar call sites e fechar readiness | checkpoint e matriz de impacto |
| ABI-002-P02 | Do | BALANCED | executar somente o escopo autorizado | incremento da etapa |
| ABI-002-P03 | Check | INDEPENDENT_REVIEW | testes, diff, contratos e revisão independente quando crítica | relatório de gate |
| ABI-002-P04 | Act | ECONOMY | dossiê, hashes, PDCA e parada | DELIVERED_FOR_REVIEW |

## 9. Execução detalhada

1. Copiar XSDs sem edição. 2. Registrar enums sem renumerar valores existentes. 3. Integrar validação/detecção usando padrão NFGas. 4. Criar fixtures sintéticas mínimas e testes de schema.

## 10. Compatibilidade, migration e rollback

Remover somente entradas NFeABI e diretório novo; confirmar build base restaurado.

## 11. Validação específica

| Ordem | Comando/cenário | Ambiente | Timeout | Resultado esperado | Artefato |
|---:|---|---|---:|---|---|
| 1 | build DLL + classe focada de validação NFeABI | ENV-DLL | 15 min | build verde e XSDs/fixtures validados | docs/plans/evidence/ABI-002/evidence/validation.txt |

## 12. Testes

Teste positivo por raiz principal e negativo para namespace, modelo e campo obrigatório; sem internet.

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

Schemas íntegros/embutidos, tipo detectado, validação focada verde e dossiê entregue.

## 17. Dossiê, evidence e retomada

- Pasta: docs/plans/evidence/ABI-002/.
- Criar no início com IN_PROGRESS, AttemptId e checkpoint.
- Arquivar o dossiê completo em attempts/AttemptId/; retrabalho usa novo AttemptId.
