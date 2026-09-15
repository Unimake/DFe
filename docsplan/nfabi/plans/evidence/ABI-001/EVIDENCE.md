# Unimake.DFe - NF-e ABI - Evidência ABI-001

- Etapa: ABI-001
- Status: DELIVERED_FOR_REVIEW
- AttemptId: attempt-0003
- Estado-base: `attempt-0002` entregue para revisão; migração da raiz documental solicitada pelo DEV
- Início: 2026-09-14T14:08:00-03:00
- Término: 2026-09-14T14:18:00-03:00
- Próxima etapa iniciada: NÃO

## Escopo e checkpoint

Retrabalho autorizado pelo DEV para mover integralmente o pacote NF-e ABI de `docs/` para `docsplan/nfabi/`. As referências em `AGENTS.md`, `README.md`, `.agents/`, manifests, dossiês e documentos internos foram ajustadas para a nova raiz. Nenhum arquivo em `source/`, schema incorporado, binário ou endpoint de produção foi alterado.

## Fonte normativa

- Fonte normativa: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi`.
- Origem dos schemas: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi\PL_NFeABI_1.00`.
- Conteúdo observado: 2 MOCs Markdown v1.00a e 20 XSDs.
- Snapshot agregado da fonte: `0383F95D81140925C4EF91046C2D723CA9D94F477E138069F7202D79D9CCCAD8`.
- Snapshot agregado dos schemas: `BA44B39981C4DF3860A6BE9AA7C0739904D664377EABE963AB52AC8C2E74AE40`.
- Comparação com ABI-000: PASS, sem divergência em hashes agregados ou individuais.

## Decisões e impacto

- DEC-001, DEC-003 e DEC-005 foram fechadas; DEC-002 e DEC-004 foram preservadas.
- Prefixo `ABI` e IDs candidatos `ABI-000` a `ABI-006` foram mantidos sem renumeração.
- Consulta e eventos permanecem como contratos XML; transporte fica restrito aos serviços publicados.
- API pública equivalente segue INTEROP; tipos internos não ganham superfície COM artificial.
- Teste fiscal online é opt-in autorizado e não substitui gates offline.
- Todo produto consumidor externo à DLL foi retirado do escopo e do plano ativo.
- A ABI-006 passou a validar exclusivamente qualidade, API/INTEROP, build e testes da DLL em `ENV-DLL`.
- O congelamento dos IDs e `READY_FOR_EXECUTION` dependem de `ABI-001 APPROVED`.

## Validação e revisão

- Migração: PASS, 92 arquivos movidos e diretório legado `docs/` removido.
- Referências: PASS, nenhuma referência operacional ou canônica ao caminho legado `docs/` permaneceu; a notação antiga aparece apenas nesta evidência de migração.
- Plan linter: PASS, adaptado à raiz `docsplan/nfabi`; 7 etapas e 7 manifests.
- Fonte: PASS, 22 arquivos e 20 XSDs com hashes reproduzidos.
- Parsing XSD: PASS, vinte documentos válidos e includes/imports presentes.
- Varredura do plano ativo: PASS, nenhuma referência nominal ao produto excluído nem ao identificador legado do ambiente integrado.
- Revisão independente: PASS, em contexto distinto do executor.
- Diff: PASS, somente planejamento; ABI-002 permanece `PLANNED`.

## Ambiente

Windows win-x64; PowerShell 7.6.5; .NET SDK 10.0.401; checkout `C:\projetos\github\Unimake.DFe`. Credenciais, certificados e XML fiscal não foram acessados.

## Rollback e cleanup

Reverter somente os arquivos declarados no manifesto de entrega da `attempt-0003`, restaurando a árvore `docs/` se a convenção `docsplan/nfabi` for rejeitada. Não há cleanup de produto.

## Limitações

Nenhum endpoint fiscal foi acionado. A verificação remota ficou restrita às páginas públicas do Portal NF-e ABI; smoke fiscal exige autorização e ambiente preparado em etapa própria.
