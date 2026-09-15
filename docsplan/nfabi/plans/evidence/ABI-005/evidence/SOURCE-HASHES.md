# ABI-005 attempt-0004 - Integridade das fontes

- Coleta Plan e Check: 2026-09-14.
- Fonte normativa: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi`.
- Origem dos schemas: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi\PL_NFeABI_1.00`.
- Regra do snapshot local: SHA-256 do texto UTF-8 com linhas `caminho/relativo|tamanho|SHA256`, ordenadas por caminho, separadas por LF e com LF final.

| Conjunto | Arquivos | Snapshot aprovado | Plan | Check | Resultado |
|---|---:|---|---|---|---|
| Fonte normativa completa | 22 | `0383F95D81140925C4EF91046C2D723CA9D94F477E138069F7202D79D9CCCAD8` | igual | igual | PASS |
| Pacote de schemas | 20 | `BA44B39981C4DF3860A6BE9AA7C0739904D664377EABE963AB52AC8C2E74AE40` | igual | igual | PASS |

Os hashes individuais permanecem iguais ao snapshot aprovado da ABI-000.
