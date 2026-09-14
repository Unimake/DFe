# Migração da raiz documental - ABI-001 attempt-0003

## Origem e destino

- Origem removida: `C:\projetos\github\Unimake.DFe\docs`
- Destino canônico: `C:\projetos\github\Unimake.DFe\docsplan\nfabi`
- Arquivos inventariados e movidos: 92

## Ajustes propagados

- referências operacionais em `AGENTS.md`, `README.md` e `.agents/`;
- caminhos normativos, manifests, rastreabilidade, validações e dossiês;
- runner do plan-linter, incluindo arquivos obrigatórios, descoberta de etapas, links e hashes;
- referências históricas internas, para que continuem resolvendo depois da mudança física.

## Resultado

O diretório legado `docs/` não existe. O pacote NF-e ABI reside integralmente em `docsplan/nfabi/`, pronto para coexistir futuramente com outros planos irmãos em `docsplan/`.
