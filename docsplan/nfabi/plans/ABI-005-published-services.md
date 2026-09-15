# Unimake.DFe - NF-e ABI - Etapa ABI-005: serviços publicados de homologação

> **Tipo:** EXECUTION
> **Dependências:** ABI-004 APPROVED
> **Ambientes:** ENV-DLL
> **Decisões necessárias:** DEC-001, DEC-002 e DEC-003
> **Manifesto:** docsplan/nfabi/plans/manifests/ABI-005.json
> **Orquestrador:** `$abi-005-orchestrator`
> **Regra:** execute somente esta etapa e pare após dossiê/PDCA.

## 1. Objetivo e valor executável

Status e autorização síncrona NFeABI resolvem a configuração oficial de homologação e usam transporte/assinatura padrão da DLL.

## 2. Definition of Ready

- Predecessora aprovada e manifesto coerente com o PDCA.
- Árvore de trabalho inspecionada; alterações preexistentes preservadas.
- Ler integralmente os dois MOCs e os XSDs aplicáveis em `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi`; recalcular SHA-256 e comparar com docsplan/nfabi/architecture/INTEGRATION-CATALOG.md.
- Decisões listadas fechadas ou contingência explicitamente autorizada.

## 3. Skills e instructions

- Ler .agents/instructions/pdca-execution.instructions.md, .agents/instructions/model-routing.instructions.md e .agents/instructions/nfabi-execution.instructions.md.
- Usar o orquestrador da etapa, o plan-linter no encerramento e as instruções AGENTS.md do subtree tocado.

## 4. Escopo

ServicoBase, StatusServico, AutorizacaoSinc, Result/NFeABIProcResults, configuração-base nacional e os 27 arquivos estaduais que herdam de `SVRS.xml`, somente para os dois endpoints publicados.

## 5. Fora de escopo

URLs de produção, transporte para serviço não publicado e qualquer produto consumidor externo à DLL.

## 6. Subtrees permitidos

Serviços/config NFeABI, recursos do csproj e testes NFeABI.

## 7. Contratos, invariantes e arquitetura

- Namespace oficial http://www.portalfiscal.inf.br/nfeabi, versão de schema 1.00, modelo fiscal 77 e processamento síncrono.
- Preservar compatibilidade binária, contratos públicos de arquivo e padrões do repositório; não modernizar stack nem introduzir dependência.
- Não cadastrar endpoint de produção enquanto a autoridade fiscal não o publicar. Nunca copiar senha, certificado ou XML fiscal real para logs/evidence.
- Capturar WSDL/SOAPAction em ambiente fiscal preparado; o host de planejamento recebeu alerta TLS e não autoriza adivinhação.

## 8. Partes PDCA e roteamento

| Parte | Fase | Perfil | Responsabilidade | Saída |
|---|---|---|---|---|
| ABI-005-P01 | Plan | DEEP | reler fontes, inventariar call sites e fechar readiness | checkpoint e matriz de impacto |
| ABI-005-P02 | Do | BALANCED | executar somente o escopo autorizado | incremento da etapa |
| ABI-005-P03 | Check | INDEPENDENT_REVIEW | testes, diff, contratos e revisão independente quando crítica | relatório de gate |
| ABI-005-P04 | Act | ECONOMY | dossiê, hashes, PDCA e parada | DELIVERED_FOR_REVIEW |

## 9. Execução detalhada

1. Obter WSDL ou evidência equivalente oficial. 2. Configurar somente homologação. 3. Implementar serviços pelo padrão NFGas, incluindo um XML embutido para cada UF com herança de `SVRS.xml`. 4. Testar a resolução offline de todas as UFs. 5. Manter testes de integração no padrão BPe, com objetos tipados e chamada a `Executar()`, exclusivamente em homologação e sem retry automático.

## 10. Compatibilidade, migration e rollback

Remover serviço/config NFeABI; modelos e schemas continuam utilizáveis.

## 11. Validação específica

| Ordem | Comando/cenário | Ambiente | Timeout | Resultado esperado | Artefato |
|---:|---|---|---:|---|---|
| 1 | testes offline de configuração + build; integração tipada de Status e Autorização com `Executar()` em homologação | ENV-DLL | 15 min | status/autorização alcançam o endpoint, desserializam retorno tipado e produção falha fechada | docsplan/nfabi/plans/evidence/ABI-005/evidence/validation.txt |

## 12. Testes

Mocks/fixtures para retorno sucesso/rejeição, certificado ausente, produção sem endpoint, proxy e resultado vazio. Testes de integração separados para Status e Autorização, construindo os objetos tipados e chamando `Executar()` como no BPe, somente em homologação; online não substitui gate offline.

## 13. Segurança, privacidade e observabilidade

- Transporte com certificado e proxy segue infraestrutura existente; nenhum secret entra em source, plano, log ou screenshot.
- Evidência externa deve ser sanitizada. Falhas distinguem rejeição fiscal de DNS, TLS, proxy, certificado e configuração.
- Testes online fiscais só ocorrem com autorização e ambiente preparado; nenhuma autorização real é repetida como sonda.

## 14. Critérios mensuráveis

| Critério | Gate | Como medir |
|---|---|---|
| Escopo | PASS | diff somente nos subtrees permitidos |
| Contrato XML/ERP | PASS | fixture representativa preserva estrutura, ordem, namespace e nomes |
| Regressão | PASS | build, testes offline e integrações tipadas de homologação verdes; limitações ambientais registradas |
| Fontes | PASS | hashes comparados e divergências analisadas antes de codificar |

## 15. Stop/Blocked

- Bloquear se a documentação externa mudar, se um contrato público depender de decisão aberta, se o endpoint necessário não estiver publicado ou se o ambiente obrigatório faltar.
- Não enfraquecer validação, certificado ou teste para obter verde. Não iniciar a sucessora.

## 16. Definition of Done

Dois serviços publicados funcionam conforme contrato; ausências explícitas e dossiê verde.

## 17. Dossiê, evidence e retomada

- Pasta: docsplan/nfabi/plans/evidence/ABI-005/.
- Criar no início com IN_PROGRESS, AttemptId e checkpoint.
- Arquivar o dossiê completo em attempts/AttemptId/; retrabalho usa novo AttemptId.
