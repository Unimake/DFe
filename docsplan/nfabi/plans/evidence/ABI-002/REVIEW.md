# Unimake.DFe - NF-e ABI - Revisão ABI-002

## Resultado

PASS. A revisão crítica foi realizada em contexto independente do executor e terminou sem finding material remanescente.

## Findings encontrados e resolvidos

1. O enforcement inicialmente global de `targetNS` poderia quebrar downloads válidos do eSocial. A alteração global foi removida e a verificação ficou restrita à NFeABI em `ValidadorEstruturalXML`; regressão `ValidarSchemaTest` permaneceu verde.
2. O envelope de evento usa `xs:any processContents="skip"`; sem guarda específica, um detalhe em namespace incorreto poderia escapar da validação. Foi incluída verificação restrita à NFeABI e teste negativo atravessando o evento completo.
3. A chave da fixture principal usava AAMM `1409` com emissão em 2026-09. Id, QR Code e referência foram alinhados para AAMM `2609`; o dígito verificador calculado permanece `0`.
4. Os novos valores `TipoXML` não tinham produtor no detector público. `XMLUtility.DetectXMLType` passou a reconhecer status, consulta, evento e documento principal; a raiz genérica `evento` é discriminada pelo namespace oficial.

## Verificações independentes

- 20 XSDs do destino têm nomes, tamanhos e SHA-256 idênticos à fonte normativa;
- compilação individual em memória com includes/imports locais: 20/20 PASS;
- projeto contém 20 `EmbeddedResource` e 20 `None Remove` para NFeABI;
- valores explícitos preexistentes dos enums não foram renumerados;
- configuração, isolador, detecção, namespace e fixtures positivas/negativas foram revisados integralmente;
- `git diff --check`: PASS;
- ABI-003 permanece `PLANNED`.

## Gate humano

Esta revisão não aprova a ABI-002. A promoção para `APPROVED` pertence exclusivamente ao DEV e não autoriza automaticamente o início da ABI-003.
