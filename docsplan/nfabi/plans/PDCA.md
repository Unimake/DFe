# PDCA

| Etapa | Incremento | Estado | Dependências | Ambiente |
|---|---|---|---|---|
| ABI-000 | base do planejamento | APPROVED | nenhuma | ENV-PLAN |
| ABI-001 | revisão decisória e congelamento | APPROVED | ABI-000 APPROVED | ENV-PLAN |
| ABI-002 | schemas e fundação do tipo fiscal | APPROVED | ABI-001 APPROVED | ENV-DLL |
| ABI-003 | modelo XML principal e retornos | APPROVED | ABI-002 APPROVED | ENV-DLL |
| ABI-004 | modelos de eventos NF-e ABI | APPROVED | ABI-003 APPROVED | ENV-DLL |
| ABI-005 | serviços publicados de homologação | APPROVED | ABI-004 APPROVED | ENV-DLL |
| ABI-006 | qualidade final e compatibilidade pública da DLL | PLANNED | ABI-005 APPROVED | ENV-DLL |

## Histórico

| Data | Etapa | De | Para | Motivo/autoridade | AttemptId |
|---|---|---|---|---|---|
| 2026-09-10 | ABI-000 | — | PLANNED | geração autorizada pelo DEV no pedido atual | attempt-0001 |
| 2026-09-10 | ABI-000 | PLANNED | IN_PROGRESS | descoberta e geração do plano | attempt-0001 |
| 2026-09-10 | ABI-000 | IN_PROGRESS | DELIVERED_FOR_REVIEW | pacote gerado e linter verde; aguarda DEV | attempt-0001 |
| 2026-09-10 | ABI-000 | DELIVERED_FOR_REVIEW | REWORK | retrabalho solicitado pelo DEV para corrigir placeholders, fontes, referências, hashes e dossiê | attempt-0002 |
| 2026-09-10 | ABI-000 | REWORK | IN_PROGRESS | auditoria integral e correção exclusiva dos artefatos de planejamento | attempt-0002 |
| 2026-09-10 | ABI-000 | IN_PROGRESS | DELIVERED_FOR_REVIEW | retrabalho concluído, fontes e hashes registrados, auditorias e linter verdes; aguarda DEV | attempt-0002 |
| 2026-09-11 | ABI-000 | DELIVERED_FOR_REVIEW | APPROVED | revisão e aprovação explícitas pelo DEV | attempt-0002 |
| 2026-09-14 | ABI-001 | PLANNED | IN_PROGRESS | execução exclusiva autorizada pelo DEV; predecessora aprovada e fonte normativa íntegra | attempt-0001 |
| 2026-09-14 | ABI-001 | IN_PROGRESS | DELIVERED_FOR_REVIEW | decisões materiais fechadas, fontes reconferidas, revisão independente e linter verdes; aguarda DEV | attempt-0001 |
| 2026-09-14 | ABI-001 | DELIVERED_FOR_REVIEW | REWORK | DEV retirou do plano todo escopo e referência ao produto consumidor externo | attempt-0002 |
| 2026-09-14 | ABI-001 | REWORK | IN_PROGRESS | revisão do plano restrita à DLL Unimake.DFe | attempt-0002 |
| 2026-09-14 | ABI-001 | IN_PROGRESS | DELIVERED_FOR_REVIEW | plano ativo restrito à DLL, referências externas removidas, revisão independente e linter verdes; aguarda DEV | attempt-0002 |
| 2026-09-14 | ABI-001 | DELIVERED_FOR_REVIEW | REWORK | DEV determinou a migração integral do plano para `docsplan/nfabi` | attempt-0003 |
| 2026-09-14 | ABI-001 | REWORK | IN_PROGRESS | árvore documental, referências, linter e dossiês em migração para a nova raiz | attempt-0003 |
| 2026-09-14 | ABI-001 | IN_PROGRESS | DELIVERED_FOR_REVIEW | migração concluída, referências e hashes atualizados, revisão e linter verdes; aguarda DEV | attempt-0003 |
| 2026-09-14 | ABI-001 | DELIVERED_FOR_REVIEW | APPROVED | revisão e aprovação explícitas pelo DEV | attempt-0003 |
| 2026-09-14 | ABI-002 | PLANNED | IN_PROGRESS | execução exclusiva autorizada pelo DEV; predecessora aprovada e fontes normativas sem divergência | attempt-0001 |
| 2026-09-14 | ABI-002 | IN_PROGRESS | DELIVERED_FOR_REVIEW | schemas íntegros e embutidos, fundação de tipo/detecção/validação concluída, testes, revisão independente e hashes verdes; aguarda DEV | attempt-0001 |
| 2026-09-14 | ABI-002 | DELIVERED_FOR_REVIEW | APPROVED | revisão e aprovação explícitas pelo DEV | attempt-0001 |
| 2026-09-14 | ABI-003 | PLANNED | IN_PROGRESS | execução exclusiva autorizada pelo DEV; predecessora aprovada e fontes normativas sem divergência | attempt-0001 |
| 2026-09-14 | ABI-003 | IN_PROGRESS | DELIVERED_FOR_REVIEW | modelo XML principal e retornos concluídos, builds, testes focados, revisão independente e hashes verdes; aguarda DEV | attempt-0001 |
| 2026-09-14 | ABI-003 | DELIVERED_FOR_REVIEW | APPROVED | revisão e aprovação explícitas pelo DEV | attempt-0001 |
| 2026-09-14 | ABI-004 | PLANNED | IN_PROGRESS | execução exclusiva autorizada pelo DEV; predecessora aprovada e fontes normativas sem divergência | attempt-0001 |
| 2026-09-14 | ABI-004 | IN_PROGRESS | DELIVERED_FOR_REVIEW | modelos dos três eventos, retorno e processado concluídos; builds, testes, revisão independente e hashes verdes; aguarda DEV | attempt-0001 |
| 2026-09-14 | ABI-004 | DELIVERED_FOR_REVIEW | APPROVED | revisão e aprovação explícitas pelo DEV | attempt-0001 |
| 2026-09-14 | ABI-005 | PLANNED | IN_PROGRESS | execução exclusiva autorizada pelo DEV; predecessora aprovada e fontes normativas sem divergência | attempt-0001 |
| 2026-09-14 | ABI-005 | IN_PROGRESS | BLOCKED | WSDL oficial exige certificado no handshake TLS; revisão independente rejeitou metadados SOAP inferidos e o protótipo foi integralmente retirado | attempt-0001 |
| 2026-09-14 | ABI-005 | BLOCKED | IN_PROGRESS | DEV forneceu e autorizou uso direto de certificado A1; os dois WSDLs oficiais foram obtidos com HTTP 200 e o contrato wire deixou de ser inferido | attempt-0002 |
| 2026-09-14 | ABI-005 | IN_PROGRESS | DELIVERED_FOR_REVIEW | WSDLs oficiais arquivados, serviços publicados implementados, produção fail-closed, builds, 39 testes focados e revisão independente verdes; aguarda DEV | attempt-0002 |
| 2026-09-14 | ABI-005 | DELIVERED_FOR_REVIEW | REWORK | DEV solicitou configuração estadual completa no padrão NFGas | attempt-0003 |
| 2026-09-14 | ABI-005 | REWORK | IN_PROGRESS | criação dos arquivos das 27 UFs, resolução por UF, recursos e testes em ajuste | attempt-0003 |
| 2026-09-14 | ABI-005 | IN_PROGRESS | DELIVERED_FOR_REVIEW | configuração das 27 UFs concluída; builds, 67 testes focados, hashes, revisão independente e linter verdes; aguarda DEV | attempt-0003 |
| 2026-09-14 | ABI-005 | DELIVERED_FOR_REVIEW | REWORK | DEV solicitou testes de integração dos serviços no padrão BPe, somente em homologação | attempt-0004 |
| 2026-09-14 | ABI-005 | REWORK | IN_PROGRESS | criação de base e testes tipados de Status e Autorização com chamada a Executar() | attempt-0004 |
| 2026-09-14 | ABI-005 | IN_PROGRESS | DELIVERED_FOR_REVIEW | testes offline e integrações reais de Status e Autorização em homologação verdes; compatibilidade do cUF textual centralizada, revisão independente e linter verdes; aguarda DEV | attempt-0004 |
| 2026-09-14 | ABI-005 | DELIVERED_FOR_REVIEW | APPROVED | revisão e aprovação explícitas pelo DEV | attempt-0004 |
