# Unimake.DFe - NF-e ABI - Evidência ABI-004

- Etapa: ABI-004
- Status: DELIVERED_FOR_REVIEW
- AttemptId: attempt-0001
- Estado-base: ABI-003 `APPROVED`; modelo XML principal e retornos presentes
- Início: 2026-09-14T16:48:19-03:00
- Término: 2026-09-14T17:06:39-03:00
- Próxima etapa iniciada: NÃO

## Resultado

A DLL agora representa o envelope `evento`, `retEventoNFeABI`, `procEventoNFeABI` e os detalhes 110111, 112110 e 112120. Os contratos preservam namespace e ordem do XSD, assinatura, identificador calculado, datas tipadas, precisão F2/F4, opcionais, escolhas CNPJ/CPF e compatibilidade INTEROP/COM. Nenhum transporte ou endpoint foi implementado.

## Fonte normativa

- Fonte: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi`.
- Schemas: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi\PL_NFeABI_1.00`.
- Snapshot geral no Plan e no Check: `0383F95D81140925C4EF91046C2D723CA9D94F477E138069F7202D79D9CCCAD8` — PASS, 22/22 arquivos.
- Snapshot dos schemas no Plan e no Check: `BA44B39981C4DF3860A6BE9AA7C0739904D664377EABE963AB52AC8C2E74AE40` — PASS, 20/20 arquivos.
- Inventário: `docsplan/nfabi/plans/evidence/ABI-004/evidence/SOURCE-HASHES.md`.

## Validação e revisão

- Build da DLL normal: PASS, 0 erros e 4 avisos preexistentes.
- Build da DLL com `DefineConstants=INTEROP`: PASS, 0 erros e 11 avisos preexistentes.
- Build do projeto de testes: PASS, 0 erros e 28 avisos preexistentes.
- `EventosNFeABITest`: PASS, 9/9 casos.
- Regressão `SerializacaoNFeABITest`: PASS, 3/3 casos.
- Regressão `SchemaFoundationTest`: PASS, 14/14 casos.
- Revisão crítica independente: findings corrigidos e verificação final PASS.
- Plano: linter e revisão integral do diff — PASS.

## Cobertura entregue

Quatro fixtures sintéticas cobrem cancelamento, as modalidades 01 e 02 de pagamento de parcela e apropriação individual de créditos. Os testes validam schema e round-trip, `Id`, retorno/processado, atributos opcionais incluindo porta zero, precisão monetária/percentual, correspondência entre tipo e detalhe, sequência única, campos condicionais, igualdade `vBC`/`vAcrescParcela`, CNPJ/CPF exclusivos, sequência de adquirentes e soma de participação em 100%.

## Ambiente, segurança e limitações

Windows win-x64, PowerShell 7.6.5 e .NET SDK 10.0.401. Testes inteiramente offline; credenciais, certificados, endpoints e XML fiscal real não foram acessados. O pós-build registrou a mensagem informativa preexistente sobre `*Undefined*Build.bat`, sem afetar o exit code ou o artefato. A suíte integral não foi executada porque a cobertura determinística focada é suficiente para o incremento.

## Rollback e parada

Rollback: remover somente as classes e fixtures de eventos desta etapa, preservando a fundação ABI-003. ABI-004 aguarda exclusivamente a revisão do DEV; não está aprovada e ABI-005 permanece `PLANNED`.
