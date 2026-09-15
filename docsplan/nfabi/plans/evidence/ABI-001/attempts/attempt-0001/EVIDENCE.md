# Unimake.DFe - NF-e ABI - Evidência ABI-001

- Etapa: ABI-001
- Status: DELIVERED_FOR_REVIEW
- AttemptId: attempt-0001
- Estado-base: `ABI-000 APPROVED`, `ABI-001 PLANNED` e árvore Git limpa
- Início: 2026-09-14T13:34:10-03:00
- Término: 2026-09-14T13:46:31-03:00
- Próxima etapa iniciada: NÃO

## Escopo e checkpoint

Execução exclusiva autorizada pelo DEV e restrita a artefatos de planejamento em `docsplan/nfabi/`. Nenhum arquivo em `source/`, schema incorporado, binário, endpoint de produção ou integração UniNFe foi alterado.

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
- Nenhuma tela nova foi autorizada no UniNFe.
- O congelamento dos IDs e `READY_FOR_EXECUTION` dependem de `ABI-001 APPROVED`.

## Validação e revisão

- Plan linter: PASS, 7 etapas e 7 manifests.
- Fonte: PASS, 22 arquivos e 20 XSDs com hashes reproduzidos.
- Parsing XSD: PASS, vinte documentos válidos e includes/imports presentes.
- Revisão independente: PASS após sincronização dos registros de estado, em contexto distinto do executor.
- Diff: PASS, somente planejamento; ABI-002 permanece `PLANNED`.

## Ambiente

Windows win-x64; PowerShell 7.6.5; .NET SDK 10.0.401; checkout `C:\projetos\github\Unimake.DFe`. Credenciais, certificados e XML fiscal não foram acessados.

## Rollback e cleanup

Reverter somente os arquivos declarados no manifesto de entrega da `attempt-0001`. O snapshot da ABI-000 permanece inalterado. Não há cleanup de produto.

## Limitações

Nenhum endpoint fiscal foi acionado. A verificação remota ficou restrita às páginas públicas do Portal NF-e ABI; smoke fiscal exige autorização e ambiente preparado em etapa própria.
