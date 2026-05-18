---
name: novo-dfe-serializacao
description: Implementar classes C# de serialização/desserialização para um novo documento fiscal eletrônico na DLL Unimake.DFe, a partir do nome da subpasta do documento, da pasta de documentação oficial e da pasta explícita de XSDs a implementar, criando uma classe/arquivo de serialização para cada XSD aplicável conforme padrões NFCom/NFe/DCe, INTEROP e testes filtrados do projeto.
---

# Novo DF-e - Serialização/Desserialização

## Objetivo

Implementar classes C# de serialização/desserialização para um novo documento fiscal eletrônico na DLL `Unimake.DFe`, seguindo a documentação oficial informada pelo usuário e o padrão existente do projeto.

A entrada obrigatória do usuário deve conter:

1. o nome da subpasta/documento;
2. o caminho da pasta onde ficam PDFs, manuais, exemplos XML e demais arquivos técnicos da documentação do novo documento;
3. o caminho da pasta onde ficam os XSDs que devem ser implementados.

Exemplos:

```text
Documento: DCe
Documentação: C:\docs\DCe
XSDs: C:\docs\DCe\Schemas

Documento: NFCom
Documentação: C:\docs\NFCom
XSDs: C:\docs\NFCom\Schemas

Documento: MDFe
Documentação: C:\docs\MDFe
XSDs: C:\docs\MDFe\Schemas
```

Com `{Documento}` informado, trabalhar principalmente em:

```text
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Xml\{Documento}
C:\projetos\github\Unimake.DFe\source\Unimake.DFe.Test\{Documento}
```

Se o nome da subpasta, a pasta de documentação ou a pasta de XSDs não forem informados, solicite somente as informações faltantes antes de alterar arquivos.

## Exemplo de uso

Exemplo de mensagem do usuário para acionar a skill:

```text
Use a skill novo-dfe-serializacao.

Documento: NFGas
Documentação: C:\projetos\docs\NFGas
XSDs: C:\projetos\docs\NFGas\Schemas
```

## Princípio central

Não invente um modelo novo. Gere ou ajuste as classes para que os XMLs reais do documento façam round-trip corretamente:

1. carregar XML de recurso;
2. desserializar para objeto;
3. serializar novamente;
4. comparar o conteúdo gerado com o XML original.

O round-trip não pode ser obtido por serialização genérica. As classes devem modelar explicitamente as tags dos XSDs, como é feito em `NFCom`.

## Antes de implementar

1. Localize e leia a pasta de documentação informada pelo usuário.
2. Localize a pasta de XSDs informada pelo usuário.
3. Faça uma varredura recursiva da pasta de XSDs e inventarie todos os arquivos `.xsd` existentes nela.
4. Identifique PDFs, notas técnicas, manuais, exemplos XML, tabelas, leiautes e arquivos auxiliares na pasta de documentação.
5. Use a documentação informada como fonte principal para tags, grupos, atributos, tipos, cardinalidade, namespaces, versões, regras de assinatura e exemplos.
6. Localize a pasta do documento em `Xml/{Documento}` e `Unimake.DFe.Test/{Documento}`.
7. Verifique se já existem classes parciais, recursos XML, schemas XSD ou testes do documento.
8. Analise as referências obrigatórias:
   - `source/.NET Standard/Unimake.Business.DFe/Xml/NFCom`;
   - `source/.NET Standard/Unimake.Business.DFe/Xml/NFe`;
   - `source/.NET Standard/Unimake.Business.DFe/Xml/DCe`, quando existir no checkout;
   - `source/Unimake.DFe.Test/NFCom/SerializacaoDesserializacaoTest.cs`;
   - testes de serialização do DFe mais parecido.
9. Procure tipos reaproveitáveis antes de criar novos:
   - enums em `Servicos`;
   - classes comuns em outros `Xml/<DFe>`;
   - `Signature`;
   - utilitários em `Utility`.
10. Se a documentação ou a pasta de XSDs estiver incompleta, ilegível, inacessível ou contraditória, pare e peça esclarecimento ou arquivo complementar. Não adivinhe layout fiscal.

## Documentação obrigatória

A pasta de documentação é obrigatória e deve orientar a interpretação dos schemas.

Ao analisá-la:

- use a pasta de XSDs informada pelo usuário como escopo obrigatório de implementação;
- inventarie todos os XSDs da pasta de XSDs antes de implementar;
- implemente a serialização/desserialização para todos os XSDs aplicáveis existentes na pasta de XSDs, não somente para o primeiro schema encontrado;
- considere também XSDs importados, incluídos ou referenciados por outros XSDs;
- prefira schemas XSD e exemplos XML reais para definir a estrutura das classes;
- use PDFs, manuais e notas técnicas para confirmar cardinalidade, obrigatoriedade, descrições e regras de negócio;
- preserve nomes oficiais de tags, atributos, grupos e namespaces;
- identifique versões do leiaute e diferenças entre homologação/produção quando existirem;
- não implemente campos ausentes na documentação apenas por analogia com outro DFe;
- quando documentação e padrão do projeto divergirem, preserve o padrão técnico do projeto sem violar o leiaute oficial.

Se houver múltiplas versões do leiaute, implemente somente a versão solicitada ou a versão indicada pelos XMLs/schemas da pasta de XSDs. Se não for possível identificar a versão correta, pergunte antes de codificar.

## Cobertura obrigatória dos XSDs

A implementação deve cobrir todos os schemas `.xsd` aplicáveis encontrados na pasta de XSDs informada pelo usuário.

Antes de codificar:

- liste recursivamente todos os `.xsd` da pasta de XSDs informada;
- agrupe schemas por mensagem/documento quando houver arquivos principais e arquivos auxiliares;
- identifique imports/includes/dependências entre XSDs;
- identifique quais schemas representam XMLs de entrada, retorno, processamento, evento, protocolo, consulta, inutilização, distribuição ou outros artefatos do novo DFe;
- descarte somente XSD claramente genérico, duplicado, legado ou não aplicável, e registre o motivo no relatório final.

Durante a implementação:

- crie uma classe de serialização raiz para cada XSD aplicável que represente um XML principal;
- crie um arquivo `.cs` separado para cada classe raiz, seguindo o padrão de `NFCom` e `NFe` (por exemplo, `ConsStatServNFCom.cs`, `RetConsStatServNFCom.cs`, `NFCom.cs`, `NFComProc.cs`);
- não agrupe todas as classes raiz de XSDs diferentes em um único arquivo;
- crie classes auxiliares necessárias para grupos complexos reutilizados;
- implemente propriedades explícitas para cada tag, grupo, atributo e lista definidos nos XSDs aplicáveis;
- siga o estilo de modelagem da pasta `source/.NET Standard/Unimake.Business.DFe/Xml/NFCom`, em que cada tag relevante vira propriedade tipada com atributo XML adequado;
- reaproveite classes existentes quando a semântica for igual;
- garanta que cada schema aplicável tenha pelo menos um caminho de serialização/desserialização representado por classe e teste, quando houver XML de exemplo ou for possível criar exemplo confiável a partir da documentação.

Se algum XSD aplicável não puder ser implementado por falta de informação, não ignore silenciosamente. Informe o bloqueio e peça o arquivo, exemplo ou regra faltante.

## Padrões do projeto

### Compatibilidade

- O projeto principal é `netstandard2.0` com `LangVersion` 7.3.
- Não use recursos incompatíveis com C# 7.3 no projeto principal: file-scoped namespace, records, nullable reference types, init-only setters, global usings, collection expressions ou APIs indisponíveis em `netstandard2.0`.
- Testes miram `net8.0`, mas devem seguir o estilo local e não forçar modernização desnecessária.

### Organização

- Classes XML ficam em `Unimake.Business.DFe.Xml.{Documento}`.
- Testes ficam em `Unimake.DFe.Test.{Documento}`.
- Cada XSD aplicável que represente XML principal deve ter sua própria classe raiz e seu próprio arquivo `.cs`.
- Arquivos devem seguir o nome da classe/raiz do XML, no padrão já usado por `NFCom` e `NFe`.
- Não misture serialização com serviço, transporte, configuração ou validação manual, exceto quando for indispensável para compilar ou seguir padrão já existente.
- Não altere exemplos, configs, serviços ou schemas fora do necessário para a serialização/testes.

### Estrutura das classes XML

Siga o padrão das classes existentes, incluindo:

- `#pragma warning disable CS1591` quando o arquivo de referência usar;
- blocos `#if INTEROP` no topo;
- `using System.Runtime.InteropServices` dentro de `#if INTEROP`;
- namespace em bloco;
- `[Serializable()]` quando usado em classes equivalentes;
- `[XmlRoot(..., Namespace = "...", IsNullable = false)]`;
- herança de `XMLBase` nas classes raiz serializáveis;
- comentários XML `/// <summary>` em classes e propriedades públicas, salvo quando o arquivo de referência do mesmo padrão claramente não tiver;
- nomes de classes, propriedades e arquivos alinhados ao XML/schema.
- uma classe raiz por XSD principal, em arquivo separado, evitando arquivo monolítico com todas as raízes.
- propriedades explícitas e tipadas para as tags do XML, no padrão da `NFCom`; não use propriedades genéricas para esconder a estrutura do schema.

### Atributos XML

Use os atributos de serialização conforme o schema:

- `[XmlRoot]` para raiz;
- `[XmlElement]` para elementos;
- `[XmlAttribute]` para atributos;
- `[XmlText]` para conteúdo textual;
- `[XmlIgnore]` para propriedades auxiliares;
- `[XmlArray]`/`[XmlArrayItem]` somente quando o padrão/schema exigir;
- `Namespace` explícito em elementos como assinatura digital.

Preserve nomes, case, ordem lógica e hierarquia do XML. Não traduza tag fiscal.

### Proibição de serialização genérica

Não use atalhos genéricos para representar conteúdo que está definido no XSD.

São proibidos como solução para tags conhecidas:

- `public XmlElement[] Conteudo { get; set; }`;
- `[XmlAnyElement]`;
- `[XmlAnyAttribute]`;
- `object`, `dynamic` ou `string` contendo XML bruto;
- listas genéricas de nós XML para preservar conteúdo sem modelar as tags.

Esses recursos só podem ser usados quando o próprio schema permitir conteúdo aberto/extensível e não houver tags conhecidas a modelar. Nesse caso, justifique no relatório final.

Para cada grupo do XSD, crie classe/propriedades explícitas como no padrão `NFCom`: propriedades com `[XmlElement]`, `[XmlAttribute]`, listas tipadas, propriedades auxiliares com `[XmlIgnore]` quando necessário e métodos `Add...`/`Get...`/`Get...Count` para `INTEROP` quando houver listas.

### Propriedades auxiliares

Quando o código usa tipo amigável mas o XML serializa outro formato, use o padrão existente:

- propriedade principal com `[XmlIgnore]`;
- propriedade `...Field` serializável para conversão;
- `Enum.Parse`, cast ou conversor já usado no projeto;
- `ShouldSerialize...()` quando a omissão condicional for necessária e já existir padrão semelhante.

Exemplos típicos:

- enum serializado como inteiro;
- data/hora serializada em string com formato específico;
- decimal com cultura invariável;
- chave calculada a partir de dados do documento.

### Listas e INTEROP

Para listas públicas, siga o padrão COM quando a classe equivalente tiver `INTEROP`:

```csharp
public List<Item> Item { get; set; }

#if INTEROP
public void AddItem(Item item) { ... }
public Item GetItem(int index) { ... }
public int GetItemCount => (Item != null ? Item.Count : 0);
#endif
```

Não exponha APIs novas de lista com padrão diferente se a pasta de referência usa `Add...`, `Get...` e `Get...Count`.

### Assinatura digital

Se o XML tiver assinatura, reutilize obrigatoriamente:

```text
source/.NET Standard/Unimake.Business.DFe/Xml/Signature.cs
```

Use o padrão:

```csharp
[XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
public Signature Signature { get; set; }
```

Não recrie classes de assinatura.

### Reuso

Antes de criar qualquer classe, enum ou helper:

- pesquise por nome de tag/classe equivalente no projeto;
- reaproveite enums e tipos já existentes quando a semântica for a mesma;
- não duplique tipos comuns apenas mudando o namespace;
- crie tipo novo somente quando o schema do novo DFe tiver semântica própria.

## Testes unitários

Crie ou ajuste `SerializacaoDesserializacaoTest.cs` em:

```text
source\Unimake.DFe.Test\{Documento}
```

Siga o padrão de `source\Unimake.DFe.Test\NFCom\SerializacaoDesserializacaoTest.cs`.

Cada XML de recurso relevante deve ter teste que:

- valida que o arquivo existe;
- carrega com `XmlDocument`;
- desserializa usando `LerXML<T>()` ou `XMLUtility.Deserializar<T>()`, conforme padrão local;
- serializa com `GerarXML()`;
- compara `InnerText` do original com o gerado;
- usa `[Trait("DFe", "{Documento}")]`.

Cada XSD aplicável deve ter cobertura por teste de serialização/desserialização quando existir XML de exemplo ou quando for possível montar exemplo confiável a partir da documentação. Não deixe schema aplicável sem teste e sem justificativa.

Use caminhos relativos no padrão:

```csharp
[InlineData(@"..\..\..\{Documento}\Resources\arquivo.xml")]
```

Inclua recursos XML realistas em `Resources` somente quando necessário e quando houver material confiável.

## Execução de testes

Não rode toda a suíte por padrão.

Execute somente os testes criados ou alterados, filtrando por classe, método ou trait do documento. Exemplos:

```powershell
dotnet test source\Unimake.DFe.Test\Unimake.DFe.Test.csproj --no-restore --filter "FullyQualifiedName~Unimake.DFe.Test.{Documento}.SerializacaoDesserializacaoTest"
dotnet test source\Unimake.DFe.Test\Unimake.DFe.Test.csproj --no-restore --filter "DFe={Documento}"
```

Rode todos os testes apenas se o usuário solicitar ou se a alteração atingir infraestrutura compartilhada como `XMLBase`, `XMLUtility`, `Signature`, conversores globais ou comportamento comum de serialização.

## Build

Quando possível, compile o projeto principal:

```powershell
dotnet build "source\.NET Standard\Unimake.Business.DFe\Unimake.Business.DFe.csproj" --no-restore
```

Se o build falhar por dependência/restauração ausente, informe isso no resultado e não tente baixar pacotes sem aprovação.

## Restrições

- Não alterar arquivos fora das pastas necessárias, exceto quando indispensável para compilar ou reaproveitar estrutura existente.
- Não introduzir novo padrão arquitetural.
- Não duplicar tipos já existentes.
- Não recriar `Signature`.
- Não alterar padrões globais do projeto.
- Não alterar `.csproj` exceto para incluir recursos ou compilação realmente necessários.
- Não atualizar versão de pacote, framework, linguagem ou dependência.
- Não executar toda a suíte de testes se for possível executar apenas os testes criados.
- Não fazer refatoração ampla junto da implementação.
- Não implementar estrutura baseada apenas em suposição quando a documentação oficial não confirmar o leiaute.
- Não implementar apenas um XSD quando a pasta de documentação contiver vários schemas aplicáveis.
- Não ignorar XSD aplicável sem registrar o motivo no relatório final.
- Não colocar todas as classes raiz de XSDs diferentes em um único arquivo.
- Não seguir XSDs encontrados fora da pasta de XSDs informada sem confirmação do usuário.
- Não usar `XmlElement[]`, `XmlAnyElement`, `XmlAnyAttribute`, `object`, `dynamic` ou XML bruto como substituto de propriedades explícitas para tags conhecidas no XSD.

## Checklist antes de finalizar

- [ ] A entrada `{Documento}` foi usada para namespace, pasta e testes.
- [ ] A pasta de documentação informada foi analisada.
- [ ] A pasta de XSDs informada foi analisada.
- [ ] Todos os XSDs da pasta de XSDs foram inventariados.
- [ ] Todos os XSDs aplicáveis foram implementados ou tiveram bloqueio/justificativa registrado.
- [ ] PDFs/manuais, schemas e exemplos XML relevantes foram considerados quando disponíveis.
- [ ] Classes seguem os padrões de `NFCom`, `NFe` e DFe semelhante.
- [ ] Cada XSD principal aplicável tem classe raiz e arquivo `.cs` próprios.
- [ ] As tags dos XSDs foram modeladas com propriedades explícitas, não com conteúdo XML genérico.
- [ ] Não há uso de `XmlElement[]`, `XmlAnyElement`, `XmlAnyAttribute`, `object`, `dynamic` ou XML bruto para tags conhecidas.
- [ ] Código do projeto principal continua compatível com C# 7.3 e `netstandard2.0`.
- [ ] Estrutura reflete fielmente schemas/XMLs de referência.
- [ ] Atributos XML foram aplicados corretamente.
- [ ] Namespaces XML oficiais foram preservados.
- [ ] INTEROP segue o padrão existente.
- [ ] Propriedades públicas têm `summary` quando aplicável.
- [ ] Tipos existentes foram reutilizados quando possível.
- [ ] Não há duplicação evitável.
- [ ] `Signature.cs` foi reutilizada quando aplicável.
- [ ] Testes foram criados ou ajustados no padrão de serialização/desserialização.
- [ ] Somente os testes criados/alterados foram executados.
- [ ] Build/testes executados foram registrados no relatório final.

## Saída esperada

Ao finalizar, responda somente com um relatório objetivo:

```text
Documento implementado:
- {Documento}

Pasta de documentação:
- ...

Pasta de XSDs:
- ...

Arquivos criados/modificados:
- ...

Estruturas implementadas:
- ...

Reaproveitamento de código:
- ...

XSDs analisados:
- ...

XSDs implementados:
- ...

XSDs não implementados e motivo:
- ...

Testes executados:
- ...

Resultado:
- ...
```

Não explique passo a passo.
Não inclua introduções.
Não inclua justificativas longas.
Não descreva o processo de execução.
