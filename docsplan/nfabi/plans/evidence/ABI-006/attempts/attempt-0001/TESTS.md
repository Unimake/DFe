# Unimake.DFe - NF-e ABI - Testes ABI-006

| Gate | Resultado |
|---|---|
| Fonte normativa Plan/Check | PASS: 22/22, agregado `0383F95D...` |
| Schemas Plan/Check | PASS: 20/20, agregado `BA44B399...` |
| Schemas incorporados | PASS: 20/20 byte a byte |
| Raízes XML NFeABI | PASS: 13/13; XMLDSig comum reutilizado |
| Build DLL normal | PASS: 0 erros, 4 avisos preexistentes |
| Build DLL INTEROP | PASS: 0 erros, 11 avisos preexistentes |
| Build de testes | PASS: 0 erros, 28 avisos preexistentes |
| `ServicosPublicadosTest` | PASS: 43/43 |
| `SerializacaoNFeABITest` | PASS: 3/3 |
| `EventosNFeABITest` | PASS: 9/9 |
| `SchemaFoundationTest` | PASS: 14/14 |
| `PublicApiTest` | PASS: 2/2 |
| Total determinístico | PASS: 71/71 |
| Superfície pública/INTEROP | PASS: 64 tipos, 62 classes, 2 enums, zero `ProgId` duplicado |
| Revisão independente | PASS após correções |
| Plano e diff | PASS |

Os testes online de Status e Autorização da ABI-005 não foram repetidos. O gate desta etapa é determinístico e não usa operação fiscal real como sonda.
