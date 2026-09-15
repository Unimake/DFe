# Unimake.DFe - NF-e ABI - Evidência ABI-006

- Etapa: ABI-006
- Status: DELIVERED_FOR_REVIEW
- AttemptId: attempt-0001
- Estado-base: ABI-005 `APPROVED`
- Início: 2026-09-14T21:52:22-03:00
- Término: 2026-09-14T22:00:28-03:00
- Próxima etapa iniciada: NÃO

## Resultado

A auditoria final confirmou os contratos XML, schemas, configuração, serviços, API pública e INTEROP da NFeABI. A lacuna encontrada na documentação executável foi resolvida de forma aditiva: `NFeABI` agora expõe `LoadFromFile(string)` e `LoadFromXML(string)`, seguindo o padrão da NFe. O novo teste protege esses métodos e os construtores tipados dos serviços publicados.

O guia mínimo demonstra certificado A1 carregado diretamente do PFX, status e autorização em homologação, limpeza de assinatura antes de alterar XML e avaliação explícita de `cStat`/`xMotivo`. Produção permanece sem endpoint.

## Fontes normativas

- Fonte: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi` — 22/22 arquivos, agregado `0383F95D81140925C4EF91046C2D723CA9D94F477E138069F7202D79D9CCCAD8` no Plan e no Check.
- Schemas: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi\PL_NFeABI_1.00` — 20/20 arquivos, agregado `BA44B39981C4DF3860A6BE9AA7C0739904D664377EABE963AB52AC8C2E74AE40` no Plan e no Check.
- Schemas incorporados: 20/20 idênticos byte a byte à origem.
- Raízes do namespace NFeABI: 13/13 cobertas por `XmlRoot`; `Signature` reutiliza XMLDSig comum.

## Validação

- Build DLL normal: PASS, 0 erros e 4 avisos preexistentes.
- Build DLL INTEROP: PASS, 0 erros e 11 avisos preexistentes.
- Build de testes: PASS, 0 erros e 28 avisos preexistentes.
- Testes determinísticos NFeABI: PASS, 71/71.
- API pública: PASS, 64 tipos exportados; 62 classes e 2 enums.
- INTEROP: PASS, 62 classes com `ComVisible`, `ClassInterface` e `ProgId`; nenhum `ProgId` duplicado.
- Configuração: PASS, 20 schemas e 29 XMLs com pares `None Remove`/`EmbeddedResource`; 27 UFs herdando de `SVRS.xml`.
- Produção/transportes não publicados: ausentes — PASS.
- Revisão crítica independente: PASS após correção dos dois findings do guia/API.
- Linter, JSON/XML, controles, segredos, links, snapshot e diff: PASS.

## Limitações e parada

Os testes reais de homologação da ABI-005 não foram repetidos na ABI-006: a autorização fiscal não pode ser usada como sonda. A suíte integral não foi executada; os 71 testes determinísticos cobrem integralmente o recorte NFeABI e evitam dependências ambientais alheias. Produtos consumidores externos, publicação, pacote e produção não foram acessados.

ABI-006 aguarda revisão do DEV e não está aprovada.
