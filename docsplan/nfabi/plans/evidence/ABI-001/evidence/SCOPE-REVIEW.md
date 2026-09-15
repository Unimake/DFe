# Revisão de escopo exclusivo da DLL - ABI-001 attempt-0003

## Decisão do DEV

O plano NF-e ABI altera e valida exclusivamente a DLL Unimake.DFe e os testes pertencentes a este repositório. Qualquer produto consumidor externo possui planejamento próprio e não integra nenhuma etapa ABI.

## Propagação verificada

- `PROJECT-BRIEF`, `QUESTION-LEDGER`, DEC-005, visão, dados, limites, riscos, segurança, UI/UX e laboratório de testes;
- planos ABI-002 a ABI-006, manifests, PDCA, índice, ambientes, validações e rastreabilidade;
- instruções NF-e ABI, guia inicial e bloco DevPlanner do README;
- ABI-006 redefinida como qualidade final e compatibilidade pública da DLL, usando somente `ENV-DLL`.

## Varredura

A busca case-insensitive pelo nome do produto retirado e pelo identificador legado do ambiente integrado retornou zero ocorrência no plano ativo sob `docsplan/nfabi`. Permanecem somente referências históricas em snapshots/dossiês de tentativas anteriores e instruções gerais do repositório fora do plano NF-e ABI; esses registros não foram reescritos para preservar contexto histórico.

## Guardrail

Nenhuma etapa ABI pode acessar, modificar, compilar ou testar repositório ou produto consumidor externo. Qualquer trabalho desse tipo exige planejamento e autorização separados.
