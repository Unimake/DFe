# Unimake.DFe - NF-e ABI - Etapa ABI-003: modelo XML principal e retornos

> **Tipo:** EXECUTION
> **Dependências:** ABI-002 APPROVED
> **Ambientes:** ENV-DLL
> **Decisões necessárias:** DEC-004
> **Manifesto:** docs/plans/manifests/ABI-003.json
> **Orquestrador:** $(System.Collections.Hashtable.Id.ToLowerInvariant())-orchestrator
> **Regra:** execute somente esta etapa e pare após dossiê/PDCA.

## 1. Objetivo e valor executável

Consumidores constroem, serializam e desserializam NFeABI, retorno, protocolo, consulta e status com fidelidade ao XSD.

## 2. Definition of Ready

- Predecessora aprovada e manifesto coerente com o PDCA.
- Árvore de trabalho inspecionada; alterações preexistentes preservadas.
- Ler integralmente os dois MOCs e os XSDs aplicáveis em $SourceRoot; recalcular SHA-256 e comparar com docs/architecture/INTEGRATION-CATALOG.md.
- Decisões listadas fechadas ou contingência explicitamente autorizada.

## 3. Skills e instructions

- Ler .agents/instructions/pdca-execution.instructions.md, .agents/instructions/model-routing.instructions.md e .agents/instructions/nfabi-execution.instructions.md.
- Usar o orquestrador da etapa, o plan-linter no encerramento e as instruções AGENTS.md do subtree tocado.

## 4. Escopo

Classes XMLBase para NFeABI/infNFeABI, grupos A-ZZ, tipos básicos, retNFeABI/procNFeABI, cons/ret situação e status; chave modelo 77 e assinatura.

## 5. Fora de escopo

Eventos, configuração SOAP, endpoint, UniNFe e refatoração de tipos compartilhados não exigida.

## 6. Subtrees permitidos

`Xml/NFeABI`, enums estritamente necessários, status de protocolo e testes/Resources NFeABI.

## 7. Contratos, invariantes e arquitetura

- Namespace oficial http://www.portalfiscal.inf.br/nfeabi, versão de schema 1.00, modelo fiscal 77 e processamento síncrono.
- Preservar compatibilidade binária, contratos públicos de arquivo e padrões do repositório; não modernizar stack nem introduzir dependência.
- Não cadastrar endpoint de produção enquanto a autoridade fiscal não o publicar. Nunca copiar senha, certificado ou XML fiscal real para logs/evidence.
- Preservar ordem XSD, precisões decimais como strings quando o padrão exigir, escolhas CNPJ/CPF e INTEROP em API pública.

## 8. Partes PDCA e roteamento

| Parte | Fase | Perfil | Responsabilidade | Saída |
|---|---|---|---|---|
| ABI-003-P01 | Plan | DEEP | reler fontes, inventariar call sites e fechar readiness | checkpoint e matriz de impacto |
| ABI-003-P02 | Do | BALANCED | executar somente o escopo autorizado | incremento da etapa |
| ABI-003-P03 | Check | INDEPENDENT_REVIEW | testes, diff, contratos e revisão independente quando crítica | relatório de gate |
| ABI-003-P04 | Act | ECONOMY | dossiê, hashes, PDCA e parada | DELIVERED_FOR_REVIEW |

## 9. Execução detalhada

1. Mapear XSD para classes por grupos. 2. Implementar enums sem valores implícitos frágeis. 3. Implementar chave/Id e processed document. 4. Round-trip de fixtures completas e simplificadas.

## 10. Compatibilidade, migration e rollback

Remover boundary `Xml/NFeABI` e registros associados, preservando schemas da ABI-002.

## 11. Validação específica

| Ordem | Comando/cenário | Ambiente | Timeout | Resultado esperado | Artefato |
|---:|---|---|---:|---|---|
| 1 | build DLL + execução direta da classe de serialização NFeABI | ENV-DLL | 15 min | round-trip InnerText, XPath e chave 44 dígitos verdes | docs/plans/evidence/ABI-003/evidence/validation.txt |

## 12. Testes

Fixtures sintéticas para tpNF completo/simplificado, múltiplos transmitentes/adquirentes, imóvel, tributação, pagamentos, totais e campos opcionais.

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

Modelo principal/retornos completos, API documentada, INTEROP coerente e testes focados verdes.

## 17. Dossiê, evidence e retomada

- Pasta: docs/plans/evidence/ABI-003/.
- Criar no início com IN_PROGRESS, AttemptId e checkpoint.
- Arquivar o dossiê completo em attempts/AttemptId/; retrabalho usa novo AttemptId.
