# Unimake.DFe - NF-e ABI - Evidência ABI-002

- Etapa: ABI-002
- Status: DELIVERED_FOR_REVIEW
- AttemptId: attempt-0001
- Estado-base: ABI-001 `APPROVED`; árvore de produto sem suporte NFeABI
- Início: 2026-09-14T14:30:00-03:00
- Término: 2026-09-14T14:51:00-03:00
- Próxima etapa iniciada: NÃO

## Resultado

A fundação NFeABI da DLL foi entregue com os 20 XSDs oficiais copiados byte a byte e embutidos, contratos públicos de tipo/modelo/serviço, detecção pública das quatro entradas, configuração central de validação e fixtures sintéticas. Nenhum endpoint, transporte, modelo OO completo ou produto consumidor externo foi alterado.

## Fonte normativa

- Fonte: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi`.
- Schemas: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi\PL_NFeABI_1.00`.
- Snapshot geral no Plan e no Check: `0383F95D81140925C4EF91046C2D723CA9D94F477E138069F7202D79D9CCCAD8` — PASS, 22/22 arquivos.
- Snapshot dos schemas no Plan e no Check: `BA44B39981C4DF3860A6BE9AA7C0739904D664377EABE963AB52AC8C2E74AE40` — PASS, 20/20 arquivos.
- Parsing, compilação XSD com dependências locais e comparação origem/destino: 20/20 — PASS.
- Inventário detalhado: `docsplan/nfabi/plans/evidence/ABI-002/evidence/SOURCE-HASHES.md`.

## Validação e revisão

- Build da DLL: PASS, 0 erros; 4 avisos de API obsoleta preexistentes.
- Build dos testes: PASS, 0 erros; avisos preexistentes na execução detalhada.
- Classe `SchemaFoundationTest`: PASS, 14 casos.
- Regressões: `ValidarSchemaTest` 3/3, `MonofasiaTest` 11/11 e `TipoXMLTest` 14/14 — PASS.
- Revisão crítica independente: PASS após três findings técnicos e uma lacuna de detecção corrigidos, sem finding material remanescente.
- Plano: linter e revisão integral do diff — PASS.

## Escopo reconciliado

O manifesto passou a autorizar o arquivo exato `Utility/XMLUtility.cs`, necessário para cumprir o requisito normativo já existente de detecção no padrão NFGas e dar produtor aos quatro novos valores `TipoXML`. Não houve expansão para transporte ou modelo XML.

## Ambiente, segurança e limitações

Windows win-x64; PowerShell 7.6.5; .NET SDK 10.0.401. Testes inteiramente offline; credenciais, certificados, endpoints e XML fiscal real não foram acessados. O pós-build registrou a mensagem informativa preexistente sobre `*Undefined*Build.bat`, sem afetar o exit code ou o artefato.

## Rollback e parada

Rollback: remover somente os XSDs/recursos, enums, detecção/configuração NFeABI e testes introduzidos na etapa. ABI-002 aguarda exclusivamente a revisão do DEV; não está aprovada e ABI-003 permanece `PLANNED`.
