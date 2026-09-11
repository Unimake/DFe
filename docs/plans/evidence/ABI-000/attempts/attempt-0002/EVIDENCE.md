# Unimake.DFe - NF-e ABI - Evidência ABI-000

- Etapa: ABI-000
- Status: DELIVERED_FOR_REVIEW
- AttemptId: attempt-0002
- Estado-base: `attempt-0001` entregue para revisão e árvore Git limpa no início do retrabalho
- Início: 2026-09-10T19:30:00-03:00
- Término: 2026-09-10T19:53:44-03:00
- Próxima etapa iniciada: NÃO

## Escopo e checkpoint

Retrabalho restrito a `docs/` e `.agents/`. Nenhum arquivo de produto, teste de produto, schema incorporado, endpoint de produção ou integração UniNFe foi alterado.

## Fonte normativa

- Fonte normativa: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi`
- Origem dos schemas: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi\PL_NFeABI_1.00`
- Conteúdo observado: 2 MOCs Markdown v1.00a e 20 XSDs.
- Snapshot agregado da fonte: `0383F95D81140925C4EF91046C2D723CA9D94F477E138069F7202D79D9CCCAD8`.
- Snapshot agregado dos schemas: `BA44B39981C4DF3860A6BE9AA7C0739904D664377EABE963AB52AC8C2E74AE40`.
- Inventário reproduzível: `docs/plans/evidence/ABI-000/evidence/SOURCE-SNAPSHOT.md`.

## Correções e verificações

- Removidas todas as expansões de template malformadas e os aliases de caminho não resolvidos.
- Corrigidos os orquestradores para `$abi-000-orchestrator` até `$abi-006-orchestrator`.
- `READINESS.md` foi reescrito sem byte NUL e com dependências explícitas.
- A referência órfã `DEC-005` foi registrada e propagada; a referência incorreta a “plano UAB” foi removida.
- Links locais foram validados pelo linter. O Portal de Serviços e o Portal de Documentos NF-e ABI responderam e confirmaram os dois endpoints de homologação publicados em 2026-09-10.
- O linter passou após ser reforçado para bloquear placeholders de geração, caracteres de controle, decisões inexistentes, caminhos normativos incorretos e nomes de orquestrador divergentes.

## Ambiente

Windows win-x64; PowerShell 7.6.5; checkout `C:\projetos\github\Unimake.DFe`. Credenciais, certificados e XML fiscal não foram acessados.

## Rollback e cleanup

Reverter somente os arquivos de planejamento declarados no manifesto de `attempt-0002`. O snapshot de `attempt-0001` permanece inalterado.
