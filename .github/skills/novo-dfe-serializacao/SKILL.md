---
name: novo-dfe-serializacao
description: Implementar classes C# de serialização/desserialização para um novo documento fiscal eletrônico na DLL Unimake.DFe, a partir do nome da subpasta do documento e da pasta de documentação oficial com PDFs/arquivos técnicos, seguindo documentação, schemas, XMLs de recurso, padrões NFCom/NFe/DCe, INTEROP e testes filtrados do projeto.
---

# Novo DF-e - Serialização/Desserialização

## Objetivo

Implementar classes C# de serialização/desserialização para um novo documento fiscal eletrônico na DLL `Unimake.DFe`, seguindo a documentação oficial informada pelo usuário e o padrão existente do projeto.

A entrada obrigatória do usuário deve conter:

1. o nome da subpasta/documento;
2. o caminho da pasta onde ficam PDFs, schemas, manuais, exemplos XML e demais arquivos técnicos da documentação do novo documento.

Exemplos:

```text
Documento: DCe
Documentação: C:\docs\DCe

Documento: NFCom
Documentação: C:\docs\NFCom

Documento: MDFe
Documentação: C:\docs\MDFe
```

Com `{Documento}` informado, trabalhar principalmente em:

```text
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Xml\{Documento}
C:\projetos\github\Unimake.DFe\source\Unimake.DFe.Test\{Documento}
```

Se o nome da subpasta ou a pasta de documentação não forem informados, solicite somente as informações faltantes antes de alterar arquivos.

## Princípio central

Não invente um modelo novo. Gere ou ajuste as classes para que os XMLs reais do documento façam round-trip corretamente:

1. carregar XML de recurso;
2. desserializar para objeto;
3. serializar novamente;
4. comparar o conteúdo gerado com o XML original.

## Antes de implementar

1. Localize e leia a pasta de documentação informada pelo usuário.
2. Identifique PDFs, notas técnicas, manuais, schemas XSD, exemplos XML, tabelas, leiautes e arquivos auxiliares.
3. Use a documentação informada como fonte principal para tags, grupos, atributos, tipos, cardinalidade, namespaces, versões, regras de assinatura e exemplos.
4. Localize a pasta do documento em `Xml/{Documento}` e `Unimake.DFe.Test/{Documento}`.
5. Verifique se já existem classes parciais, recursos XML, schemas XSD ou testes do documento.
6. Analise as referências obrigatórias:
   - `source/.NET Standard/Unimake.Business.DFe/Xml/NFCom`;
   - `source/.NET Standard/Unimake.Business.DFe/Xml/NFe`;
   - `source/.NET Standard/Unimake.Business.DFe/Xml/DCe`, quando existir no checkout;
   - `source/Unimake.DFe.Test/NFCom/SerializacaoDesserializacaoTest.cs`;
   - testes de serialização do DFe mais parecido.
7. Procure tipos reaproveitáveis antes de criar novos:
   - enums em `Servicos`;
   - classes comuns em outros `Xml/<DFe>`;
   - `Signature`;
   - utilitários em `Utility`.
8. Se a documentação estiver incompleta, ilegível, inacessível ou contraditória, pare e peça esclarecimento ou arquivo complementar. Não adivinhe layout fiscal.

## Documentação obrigatória

A pasta de documentação é obrigatória e deve orientar a implementação.

Ao analisá-la:

- prefira schemas XSD e exemplos XML reais para definir a estrutura das classes;
- use PDFs, manuais e notas técnicas para confirmar cardinalidade, obrigatoriedade, descrições e regras de negócio;
- preserve nomes oficiais de tags, atributos, grupos e namespaces;
- identifique versões do leiaute e diferenças entre homologação/produção quando existirem;
- não implemente campos ausentes na documentação apenas por analogia com outro DFe;
- quando documentação e padrão do projeto divergirem, preserve o padrão técnico do projeto sem violar o leiaute oficial.

Se houver múltiplas versões do leiaute, implemente somente a versão solicitada ou a versão indicada pelos XMLs/schemas da pasta. Se não for possível identificar a versão correta, pergunte antes de codificar.

## Padrões do projeto

### Compatibilidade

- O projeto principal é `netstandard2.0` com `LangVersion` 7.3.
- Não use recursos incompatíveis com C# 7.3 no projeto principal: file-scoped namespace, records, nullable reference types, init-only setters, global usings, collection expressions ou APIs indisponíveis em `netstandard2.0`.
- Testes miram `net8.0`, mas devem seguir o estilo local e não forçar modernização desnecessária.

### Organização

- Classes XML ficam em `Unimake.Business.DFe.Xml.{Documento}`.
- Testes ficam em `Unimake.DFe.Test.{Documento}`.
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

## Checklist antes de finalizar

- [ ] A entrada `{Documento}` foi usada para namespace, pasta e testes.
- [ ] A pasta de documentação informada foi analisada.
- [ ] PDFs/manuais, schemas e exemplos XML relevantes foram considerados quando disponíveis.
- [ ] Classes seguem os padrões de `NFCom`, `NFe` e DFe semelhante.
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

Arquivos criados/modificados:
- ...

Estruturas implementadas:
- ...

Reaproveitamento de código:
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
