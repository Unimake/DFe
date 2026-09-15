# ABI-005 attempt-0001 - Integridade da fonte e dos schemas

- Coleta final: 2026-09-14.
- Fonte normativa: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi`.
- Origem: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi\PL_NFeABI_1.00`.
- Regra: SHA-256 do texto UTF-8 com linhas `caminho/relativo|tamanho|SHA256`, ordenadas por caminho, separadas por LF e com LF final.

| Conjunto | Arquivos | Snapshot aprovado | Snapshot ABI-005 Check | Resultado |
|---|---:|---|---|---|
| Fonte normativa completa | 22 | `0383F95D81140925C4EF91046C2D723CA9D94F477E138069F7202D79D9CCCAD8` | `0383F95D81140925C4EF91046C2D723CA9D94F477E138069F7202D79D9CCCAD8` | PASS |
| Pacote de schemas | 20 | `BA44B39981C4DF3860A6BE9AA7C0739904D664377EABE963AB52AC8C2E74AE40` | `BA44B39981C4DF3860A6BE9AA7C0739904D664377EABE963AB52AC8C2E74AE40` | PASS |

Os 22 hashes individuais coincidem com `docsplan/nfabi/plans/evidence/ABI-000/evidence/SOURCE-SNAPSHOT.md`. A integridade da fonte não resolve a ausência dos metadados wire, que somente o WSDL oficial pode comprovar.
