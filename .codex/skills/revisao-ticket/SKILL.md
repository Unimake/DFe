---
name: revisao-ticket
description: 'Use quando Codex precisar revisar commits vinculados a um ticket Redmine no projeto Unimake.DFe pelo padrão "ID #000000", consolidando todos os commits do ticket e gerando justificativa objetiva em Textile somente para problemas técnicos que permanecem no estado final.'
---

# Revisão técnica de ticket por commits

## Objetivo

Revisar os commits relacionados a um ticket informado pelo usuário e avaliar o resultado final do conjunto de alterações.

Localizar o ticket nas mensagens de commit pelo padrão exato:

```text
ID #123456
```

Use o restante do repositório apenas como contexto para entender arquitetura, padrões, convenções, compatibilidade e impacto. Não critique código antigo não alterado pelos commits do ticket, exceto quando ele for indispensável para explicar o impacto direto da alteração revisada.

## Entrada

O usuário deve informar o número do ticket, por exemplo:

```text
123456
```

Se o número do ticket não for informado, peça somente essa informação.

## Fluxo obrigatório

1. Localize todos os commits cuja mensagem contenha exatamente `ID #NUMERO`.
2. Considere todos os branches disponíveis quando possível.
3. Analise os commits em ordem cronológica.
4. Levante arquivos, métodos, classes, XMLs, schemas e testes alterados pelo ticket.
5. Compare achados intermediários com o estado final após todos os commits do ticket.
6. Reporte somente problemas que ainda permanecem no resultado final.
7. Se não houver nenhum arquivo de código alterado pelo ticket, responda com o aviso específico definido em "Sem código alterado".

Comandos de referência:

```bash
git log --all --grep="ID #123456" --format="%H %an %ad %s" --date=iso --reverse
git show --stat --patch HASH_DO_COMMIT
git show HEAD:CAMINHO_DO_ARQUIVO
```

No PowerShell, liste arquivos únicos assim:

```powershell
git log --all --grep="ID #123456" --name-only --format="" | Sort-Object -Unique
```

Quando houver sequência linear clara, use diff acumulado:

```bash
git diff COMMIT_BASE..COMMIT_FINAL -- CAMINHO_DO_ARQUIVO
```

Se o branch atual não contiver o estado final do ticket, use o último commit do ticket como referência para os arquivos alterados.

## Regra de consolidação

A revisão é do ticket, não de cada commit isolado.

Não reporte:

- erro introduzido em commit intermediário e corrigido depois;
- código removido posteriormente no mesmo ticket;
- implementação parcial completada por commit posterior;
- inconsistência temporária entre commits do mesmo ticket;
- problema que não existe no estado final do código.

Antes de reprovar, confirme se algum commit posterior do mesmo ticket alterou o mesmo arquivo, classe, método, XML, schema, teste ou comportamento relacionado.

## Escopo da revisão

Analise somente alterações feitas pelo ticket, incluindo efeitos diretos sobre:

- código C# da biblioteca principal;
- classes XML e serialização/desserialização;
- serviços SOAP/API;
- configurações em `Servicos/Config`;
- schemas XSD e recursos embutidos;
- assinatura digital e certificado;
- validadores;
- testes e recursos XML de teste;
- exemplos somente quando o ticket alterá-los.

Não faça implementação nem correção durante a revisão, a menos que o usuário peça explicitamente.

## Padrões do projeto Unimake.DFe

Use estes padrões como referência principal ao revisar.

### Arquitetura

- A solução fica em `source/Unimake.DFe.sln`.
- A biblioteca principal fica em `source/.NET Standard/Unimake.Business.DFe` e mira `netstandard2.0`.
- O projeto principal usa C# 7.3. Não aceitar uso de recursos incompatíveis, como file-scoped namespace, records, nullable reference types, init-only setters, global usings ou APIs indisponíveis em `netstandard2.0`.
- O projeto `source/.NET Framework/Unimake.Security.Platform` mantém compatibilidade `net472`.
- Testes ficam em `source/Unimake.DFe.Test`, usam xUnit v3 e miram `net8.0`.

### Organização por DFe

- Novos itens devem respeitar a separação por tipo de documento fiscal:
  - `Xml/<DFe>`;
  - `Servicos/<DFe>`;
  - `Servicos/Config/<DFe>`;
  - `Validator/<DFe>`;
  - `Unimake.DFe.Test/<DFe>`.
- Não aceitar solução genérica ou paralela quando já existe padrão por DFe equivalente.
- Não aceitar mudança que misture responsabilidades de XML, serviço, configuração, transporte e validação sem necessidade.

### Serialização XML

- Classes que representam XML fiscal devem seguir `XMLBase` e usar atributos explícitos como `[XmlRoot]`, `[XmlElement]`, `[XmlAttribute]`, `[XmlIgnore]` e `[XmlText]`.
- Nomes de tags, namespaces e atributos devem seguir schema/manual fiscal. Não traduzir nomes de tags.
- Quando a propriedade pública usa enum ou tipo amigável, mas o XML precisa de número/string, verificar se há propriedade auxiliar de serialização no padrão `...Field`.
- Não aceitar XML fiscal montado por concatenação de strings quando há estrutura de objeto, `XmlDocument`, `XDocument` ou `XMLUtility`.
- Verificar se o XML gerado preserva namespace oficial, tags obrigatórias, ordem esperada pelo schema e valores default já usados pelo projeto.

### Serviços

- Serviços devem herdar da `ServicoBase` específica do DFe quando existir.
- O fluxo esperado é: receber XML/objeto e `Configuracao`, chamar `Inicializar(...)`, definir configurações, assinar quando necessário, validar e executar transporte.
- Em `DefinirConfiguracao()`, verificar se o serviço define corretamente `Servico`, `CodigoUF`, `TipoAmbiente`, `SchemaVersao`, `TipoDFe` e demais dados derivados do XML.
- Não aceitar duplicação de transporte SOAP/API se a alteração deveria reutilizar `ConsumirServico`, `Builders`, `Parsers`, `Transport` ou mappers existentes.
- Retornos tipados devem seguir o padrão de propriedade `Result`, desserializando `RetornoWSXML` com `XMLUtility.Deserializar<T>()` e tratando ausência de retorno com mensagem clara.

### Configurações, schemas e recursos

- Ao adicionar serviço, versão, provedor, município ou schema, verificar se foram atualizados juntos:
  - XML de configuração em `Servicos/Config/<DFe>`;
  - XSD em `Xml/Schemas/<DFe>`;
  - inclusão como `EmbeddedResource` no `.csproj` quando necessário;
  - testes e recursos XML correspondentes.
- Verificar se `Configuracao.Load(...)` conseguirá localizar o recurso pelo namespace esperado.
- Atenção a produção/homologação, SOAP/API, certificado digital, assinatura, GZip, autenticação municipal, UF, município e versão do schema.

### INTEROP e compatibilidade pública

- Classes públicas usadas por integrações externas devem preservar suporte a `INTEROP`/COM quando o padrão existir.
- Verificar blocos `#if INTEROP`, `[ClassInterface(ClassInterfaceType.AutoDual)]`, `[ProgId(...)]`, `[ComVisible(true)]` e `[ComVisible(false)]`.
- Não aceitar remoção/renomeação de propriedades, métodos, construtores, classes ou namespaces públicos sem justificativa forte de breaking change.
- APIs públicas novas devem ter comentários XML `/// <summary>`, pois `CS1591` é tratado como erro no projeto principal.

### Validações

- Validação de schema deve permanecer em `ValidarSchema`.
- Validações manuais devem usar `Validator/<DFe>`, herdar de `XmlValidatorBase` e registrar regras com `ValidateTag(...)`.
- Mensagens de erro devem ser claras, em português, indicando tag, grupo, valor informado e impacto quando possível.
- `Warnings` deve ser usado apenas para alertas não impeditivos.

### Testes

- Novo XML/classe de serialização deve ter teste de serialização/desserialização.
- Novo serviço deve ter teste no DFe correspondente.
- Novo schema/provedor deve ter recurso XML realista em `Resources`.
- Testes devem usar `[Trait("DFe", "<DFe>")]` quando aplicável.
- Caminhos devem seguir o padrão existente, como `@"..\..\..\NFe\Resources\arquivo.xml"`.
- Não exigir execução de toda a suíte por padrão. Se for necessário validar, rode somente testes novos, alterados ou filtrados por classe, método ou DFe.
- Só recomendar suíte completa quando a alteração atingir infraestrutura compartilhada, serialização base, assinatura, transporte, validação global ou quando o usuário solicitar.

## O que reprovar

Reporte somente problema concreto e remanescente, como:

- bug ou comportamento incorreto;
- quebra de compatibilidade pública ou INTEROP;
- uso de recurso incompatível com C# 7.3, `netstandard2.0` ou `net472`;
- XML gerado fora do schema/manual fiscal;
- configuração, schema ou recurso embutido ausente/inconsistente;
- serviço sem configuração correta, assinatura, validação ou tratamento de retorno;
- falha de validação ou mensagem ruim para o usuário;
- teste ausente para comportamento novo ou bug corrigido;
- risco de segurança, certificado, assinatura ou transporte;
- duplicação técnica desnecessária frente a padrão já existente;
- organização em diretório/namespace errado;
- documentação XML pública ausente;
- código confuso, difícil de manter ou inconsistente com o estilo local.

Não reprovar por preferência pessoal, modernização estética, refatoração opcional ou estilo que o próprio projeto usa amplamente.

## Como escrever achados

Cada achado deve explicar:

- o que foi encontrado;
- por que é problema neste projeto;
- impacto prático;
- como corrigir;
- confiança da análise.

Se depender de contexto externo ou regra fiscal não confirmada no código, marque como risco potencial e use confiança média ou baixa.

Não invente problemas. Não use comentários genéricos como "melhorar código", "refatorar" ou "avaliar impacto" sem apontar um defeito concreto.

## Formato da resposta

Não escreva preâmbulo, resumo de investigação, lista de commits analisados, conclusão extra ou arquivos sem problema.

### Sem problemas

Se nenhum problema concreto permanecer após a análise consolidada, responda somente:

```text
Não encontramos falhas.
```

### Sem código alterado

Se o ticket não tiver nenhum código alterado associado aos commits encontrados, responda somente:

```text
Não encontramos códigos alterados para o ticket informado.
```

### Com problemas

Se houver problemas remanescentes, comece exatamente com:

```text
h2. Justificativa da reprova na revisão do ticket:
```

Para cada problema, use Textile simples:

```text
h3. Problema N - título curto

Explique em poucas linhas o problema identificado.

h4. Trecho de código relevante

<pre>
mostre somente o trecho final necessário para justificar a análise
</pre>

h4. Por que isso é um problema

Explique objetivamente por que pode falhar, gerar comportamento incorreto, dificultar manutenção ou quebrar padrão do projeto.

h4. Impacto prático

Explique em linguagem simples o impacto para produção, manutenção, compatibilidade, XML fiscal, integração ou testes.

h4. Sugestão de correção

Explique o que mudar, como mudar e por que a alternativa respeita melhor o padrão do projeto.

h4. Confiança da análise

Alto: problema claro e evidente.
Médio: forte indício, mas depende de contexto.
Baixo: risco potencial.

h4. Observação didática

Inclua uma orientação curta para evitar esse tipo de problema no futuro.
```

## Restrições finais

- Entregue somente o relatório final.
- Seja direto, didático e respeitoso.
- Não repreenda, não ironize e não escreva teoria longa.
- Não repita trechos grandes de código.
- Mostre o trecho de código no estado final, não em commit intermediário.
- Não liste commits aprovados.
- Não inclua problemas corrigidos no próprio ticket.
