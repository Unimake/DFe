---
name: novo-dfe-serializacao
description: Use quando Codex precisar implementar ou revisar classes C# de serialização/desserialização XML para um novo documento fiscal eletrônico na DLL Unimake.DFe, a partir do nome da subpasta do documento, da pasta de documentação oficial e da pasta explícita de XSDs a implementar, criando uma classe/arquivo para cada XSD aplicável conforme padrões NFCom/NFe/DCe, tipagem forte por XSD, INTEROP e testes filtrados do projeto.
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

## Regra de ouro: XSD é contrato

Use o XSD como contrato técnico principal da serialização. Exemplos XML, recursos de teste e classes de outros DF-e ajudam a entender o padrão, mas podem estar incompletos, antigos ou inválidos.

Quando houver divergência:

- se o XML de exemplo contrariar o XSD, não deforme a classe para aceitar o exemplo inválido;
- se uma classe parecida de outro DF-e tiver estrutura diferente, siga o XSD do documento atual;
- se um teste existente falhar porque o recurso XML não obedece ao XSD, registre o problema do recurso separadamente e corrija a classe conforme o schema;
- não copie campos de protocolos, retornos, eventos ou grupos por analogia sem confirmar a posição exata no XSD.

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
   - especialmente `source/.NET Standard/Unimake.Business.DFe/Servicos/Enums/Enums.cs`;
   - classes comuns em outros `Xml/<DFe>`;
   - `Signature`;
   - utilitários em `Utility`.
10. Para cada tag/propriedade que será implementada, verifique no XSD e na documentação se há domínio fechado de valores, lista de códigos, `xs:enumeration`, tabela de domínio ou valores predefinidos.
11. Para cada tag/propriedade que será implementada, classifique o tipo C# correto antes de codificar: enum, data/hora, número, booleano, lista, classe de grupo ou string preservada por formato/código.
12. Se a documentação ou a pasta de XSDs estiver incompleta, ilegível, inacessível ou contraditória, pare e peça esclarecimento ou arquivo complementar. Não adivinhe layout fiscal.

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

## Extração obrigatória da árvore XSD

Antes de criar ou alterar classes, extraia a estrutura real dos XSDs aplicáveis, seguindo `xs:include`, `xs:import`, tipos globais e tipos locais. Prefira análise estruturada do XML/XSD a leitura por aproximação textual.

Monte uma tabela de conferência para cada schema principal:

```text
XSD raiz:
Classe raiz:
Tipo raiz:
Namespace:
Filhos em xs:sequence/xs:choice, na ordem:
Atributos:
Tipos complexos reutilizados:
Tipos simples/enumerations:
Classe/propriedade C# correspondente:
```

Ao preencher essa tabela, confira obrigatoriamente:

- ordem exata de `xs:sequence`;
- alternativas de `xs:choice`;
- `minOccurs` e `maxOccurs`;
- atributos `use="required"` e opcionais;
- elementos que são grupos diretos versus elementos dentro de subgrupos;
- elementos de retorno que pertencem ao protocolo interno, e não ao XML raiz;
- elementos de assinatura e conteúdo aberto declarado com `xs:any`.

Não comece a codificar uma classe raiz enquanto a árvore do XSD correspondente não estiver clara. Essa etapa deve evitar erros como criar wrapper inexistente, achatar grupo aninhado, transformar atributo em elemento ou colocar campo de protocolo no retorno raiz.

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
- em `INTEROP`, toda classe pública de XML exposta no arquivo, inclusive grupos auxiliares e subtotais, deve ter `[ClassInterface(ClassInterfaceType.AutoDual)]`, `[ProgId("Unimake.Business.DFe.Xml.{Documento}.{Classe}")]` e `[ComVisible(true)]`.

#### Nomenclatura de classes de grupos XML

Classes que representam grupos/tags do XML devem, por padrão, ter exatamente o mesmo nome da tag ou grupo que elas modelam, apenas convertido para o padrão PascalCase do C# quando necessário.

Exemplos:

```text
<ide>        -> class Ide
<emit>       -> class Emit
<enderEmit>  -> class EnderEmit
<infBPe>     -> class InfBPe
<det>        -> class Det
<imp>        -> class Imp
<IBSCBS>     -> class IBSCBS
<gIBSCBS>    -> class GIBSCBS
<ICMSTot>    -> class ICMSTot
<pgtoVinc>   -> class PgtoVinc
```

Não acrescente o nome do documento ao final da classe auxiliar apenas para indicar contexto, como `IdeBPeTM`, `EmitBPeTM`, `ImpBPeTM` ou `ICMSBPeTM`, quando a tag real for somente `ide`, `emit`, `imp` ou `ICMS`.

Use sufixo/prefixo do documento somente quando ele fizer parte da tag real, da raiz ou do grupo no XSD, como `BPeTM` e `DetBPeTM`, ou quando houver conflito técnico inevitável com outra classe no mesmo namespace/arquivo. Mesmo em caso de conflito, tente primeiro resolver com o menor qualificador semântico possível e registre o motivo no relatório final.

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

Além disso:

- propriedades C# devem aparecer na mesma ordem dos filhos em `xs:sequence`;
- atributos do XSD devem ser `[XmlAttribute]`, nunca `[XmlElement]`;
- elementos do XSD devem ser `[XmlElement]`, nunca `[XmlAttribute]`;
- `xs:choice` deve ser modelado como alternativas reais, sem criar wrapper inexistente;
- grupos aninhados devem permanecer aninhados na classe, sem achatar propriedades para o nível errado;
- não invente grupos por analogia com outro documento quando o XSD atual não declarar o grupo.

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

Para campos opcionais (`minOccurs="0"`) com tipo valor, enum ou número:

- não serialize valor default como se fosse informado;
- use `ShouldSerialize...()` quando o padrão local permitir omitir o campo com segurança;
- use propriedade `...Specified` quando o valor default também puder ser válido;
- para enum opcional, use sentinela existente do projeto ou `ShouldSerialize...()` coerente com o domínio;
- para inteiro opcional em que `0` não é valor válido, `ShouldSerialize...() => Campo > 0` é aceitável quando seguir o padrão local;
- strings opcionais podem ser omitidas por `null`, mas use `ShouldSerialize...()` se string vazia não deve sair no XML.

Não adicione `ShouldSerialize...()` em campo obrigatório. A omissão condicional deve refletir o XSD.

### Tipagem forte obrigatória

Não implemente campos como `string` por padrão. Em cada propriedade, escolha o tipo de domínio mais seguro a partir do XSD, documentação e padrões `NFCom`, `NFGas`, `NFe`, `CTe`, `NF3e`, `CIOT` ou DFe mais parecido.

Use esta regra de decisão antes de escrever a propriedade:

```text
xs:complexType / grupo / tag com filhos        -> classe tipada com nome da tag
maxOccurs > 1                                  -> List<T> tipada + helpers INTEROP
xs:enumeration / tabela fechada                -> enum em Servicos/Enums/Enums.cs quando reutilizavel
UF / TCodUfIBGE / TUf / TUf_sem_EX             -> UFBrasil ou enum existente equivalente
TAmb / tpAmb                                   -> TipoAmbiente
TMod* / mod                                    -> ModeloDFe ou enum especifico existente
CRT / regime tributario                        -> CRT ou enum equivalente
TDateTimeUTC / dateTime com timezone           -> DateTimeOffset fora de INTEROP e DateTime em INTEROP + ...Field
TData / xs:date / data sem hora                -> DateTime + ...Field no formato yyyy-MM-dd
TTime / hora                                   -> string somente se o projeto nao tiver tipo/helper melhor
TDec* / valor monetario / quantidade decimal   -> double + ...Field com CultureInfo.InvariantCulture
percentual / aliquota / p* decimal             -> double + ...Field; use F4 quando o tipo aceitar 2 a 4 casas e o padrao RTC/NFCom usar 4 casas
valor monetario / v* decimal                   -> double + ...Field; normalmente F2
quantidade inteira sem zeros a esquerda        -> int ou long conforme faixa do XSD; se exigir long em classe exposta a INTEROP, usar string dentro de #if INTEROP
atributo numerico sem zeros a esquerda         -> int/long + [XmlAttribute]
booleano ou indicador binario fechado          -> enum se os valores fiscais forem codificados; bool somente se o XSD for boolean real
```

Use `string` somente quando o valor for texto livre ou quando preservar o formato for parte do contrato fiscal:

- documentos e inscricoes: `CNPJ`, `CPF`, `IE`, `IM`, `fone`, `CEP`;
- chaves, hashes, protocolos, recibos, identificadores fiscais e referencias de DFe;
- códigos com zeros a esquerda ou tamanho fixo textual: `cBP`, `cNF`, `cClassTrib`, `CST` RTC de 3 digitos, `qComp`, `nPag`, `idTransacao`;
- CFOP, NCM, CEST, municipio IBGE e códigos oficiais quando o projeto equivalente os mantém como texto para preservar formato;
- campos de pagamento ou dominio em que nao houver enum central compativel. Para `tpMeioPgto`/`TMeioPgto`, conferir primeiro `MeioPagamento` em `Servicos/Enums/Enums.cs`; se os codigos de dois digitos forem compativeis, usar o enum mesmo que algum DFe antigo ainda tenha a propriedade como `string`.

Quando usar `string` em uma tag que parece numerica, confirme e registre mentalmente o motivo: preservar zeros a esquerda, codigo oficial textual, tamanho fixo, formato livre, compatibilidade com DFe equivalente ou ausencia de dominio fechado. Se o motivo não existir, tipar como `int`, `long`, `double`, `DateTime`, `DateTimeOffset` ou enum.

Para propriedades tipadas que precisam serializar texto:

- exponha a propriedade amigavel com `[XmlIgnore]`;
- crie a propriedade `...Field` com `[XmlElement]` ou `[XmlAttribute]` no ponto exato da sequencia;
- use `Converter.ToDouble(value)` ou conversor local ja usado no projeto para numeros;
- use `CultureInfo.InvariantCulture` ao formatar `double`;
- mantenha helper privado/interno pequeno somente quando reduzir repeticao real, sem criar arquitetura nova;
- para opcionais de tipo valor, use `double?`, enum opcional/sentinela ou `ShouldSerialize...Field()` para nao serializar default indevido.

Para propriedades inteiras que precisariam ser `long` no C# e que serão expostas via `INTEROP`/COM, considere a compatibilidade com linguagens como Lazarus. `long` pode não funcionar bem nesses consumidores. Nesse caso, use compilação condicional:

```csharp
[XmlElement("qPass")]
#if INTEROP
public string QPass { get; set; }
#else
public long QPass { get; set; }
#endif
```

Use esse padrão somente quando o campo realmente exigir faixa acima de `int` ou quando o XSD permitir tamanho que justifique `long`; para inteiros comuns, prefira `int`.

Antes de finalizar, procure no arquivo novo por campos suspeitos:

```powershell
rg -n "public string (V[A-Z]|P[A-Z]|Q[A-Z]|Tp[A-Z]|Mod|Modal|Dh|D[A-Z].*|UF|CRT|CST|Ind[A-Z]|CDV)" "source/.NET Standard/Unimake.Business.DFe/Xml/{Documento}"
```

Cada resultado deve ser `...Field` de serializacao ou string justificada pelo contrato fiscal. Se aparecer campo fiscal comum como `public string VBC`, `public string PICMS`, `public string DhEmi`, `public string TpAmb`, `public string Mod`, `public string UF` ou `public string CRT`, corrija antes de testar.

### Enumeradores obrigatórios

Para cada propriedade/tag implementada, analise se o valor possui domínio fechado.

Crie ou reutilize enum quando houver:

- `xs:enumeration` no XSD;
- tipo simples restrito por lista de valores;
- tabela de códigos no manual/PDF/nota técnica;
- valores predefinidos como ambiente, modelo, emissão, status, tipo de evento, indicador, finalidade, modalidade, UF, município/provedor quando aplicável;
- propriedade que no projeto equivalente já usa enum em outro DFe.

Antes de criar enum novo:

- procure enum compatível em `source/.NET Standard/Unimake.Business.DFe/Servicos/Enums/Enums.cs`;
- procure também em outros arquivos existentes sob `Servicos` e `Xml`;
- reutilize enum existente quando a semântica e os valores forem iguais;
- não crie enum duplicado com outro nome para o mesmo domínio.

Quando criar enum novo:

- coloque no local já usado pelo projeto para enums de DFe, preferencialmente `Servicos/Enums/Enums.cs` quando o padrão local indicar;
- use nomes claros e alinhados ao domínio fiscal;
- mantenha valores numéricos/string serializados por propriedade auxiliar quando necessário;
- preserve compatibilidade de serialização usando `[XmlIgnore]` na propriedade enum e uma propriedade `...Field` com `[XmlElement]`/`[XmlAttribute]` quando o XML exigir número ou string;
- documente os membros quando o padrão do arquivo exigir.

Não deixe como `int`, `string` ou `double` uma tag com valores predefinidos se for possível modelar com enum sem quebrar a serialização. Use tipo primitivo apenas quando o valor for realmente aberto, texto livre, número sem domínio fechado ou quando o schema permitir ampla variação.

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

Além dos helpers de lista, confira que cada `public class` serializável tenha o bloco COM completo no padrão:

```csharp
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.{Documento}.{Classe}")]
    [ComVisible(true)]
#endif
public class Classe
```

Não deixe classes auxiliares sem esse bloco apenas por não serem raiz XML; se a classe é pública e representa grupo/tag fiscal, ela deve ficar acessível para COM/OLE no mesmo padrão das demais.

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

Quando um teste de round-trip falhar, valide a causa contra o XSD antes de ajustar a classe:

- se a classe serializa fora da ordem, com nível errado, atributo como elemento ou opcional indevido, corrija a classe;
- se o recurso XML tiver campos inexistentes no XSD, campos no nível errado ou estrutura antiga, reporte o recurso inválido;
- não aceite `InnerText` igual como prova suficiente se a estrutura XML estiver errada;
- quando possível, valide o XML gerado contra o XSD do schema principal.

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
- Não criar wrapper, grupo intermediário ou propriedade achatada sem correspondência direta no XSD.
- Não transformar atributo do XSD em elemento XML, ou elemento do XSD em atributo XML.
- Não serializar campo opcional de tipo valor apenas porque o default do C# existe.
- Não mover para o XML raiz campos que pertencem a protocolo, evento interno, `infProt`, `infEvento` ou outro grupo aninhado.
- Não usar `XmlElement[]`, `XmlAnyElement`, `XmlAnyAttribute`, `object`, `dynamic` ou XML bruto como substituto de propriedades explícitas para tags conhecidas no XSD.
- Não deixar campos fiscais de data, hora com timezone, valor monetário, quantidade, percentual, alíquota, UF, ambiente, modelo, CRT, emissão, modalidade ou indicador como `string` quando houver tipo C# mais adequado e formato serializável via `...Field`.
- Não expor `long` em propriedade pública de XML sob `INTEROP`; quando a faixa exigir `long` no C#, usar `string` no bloco `#if INTEROP`.
- Não deixar tag com valores predefinidos como `string`, `int` ou outro primitivo sem antes verificar e justificar se enum existente ou novo enum deveria ser usado.
- Não criar enum novo sem antes pesquisar enum compatível em `Enums.cs` e nos tipos existentes do projeto.
- Não sufixar classes auxiliares de grupos/tags com o nome do documento quando esse sufixo não existir na tag XML; use o nome da tag como nome da classe e diferencie somente em caso de conflito real.
- Não deixar classe pública de XML sem bloco `INTEROP` com `ClassInterface`, `ProgId` e `ComVisible`.

## Checklist antes de finalizar

- [ ] A entrada `{Documento}` foi usada para namespace, pasta e testes.
- [ ] A pasta de documentação informada foi analisada.
- [ ] A pasta de XSDs informada foi analisada.
- [ ] Todos os XSDs da pasta de XSDs foram inventariados.
- [ ] Todos os XSDs aplicáveis foram implementados ou tiveram bloqueio/justificativa registrado.
- [ ] Includes/imports/tipos globais dos XSDs aplicáveis foram seguidos.
- [ ] Foi montada conferência de XSD raiz, tipo raiz, filhos, atributos, cardinalidade e classe/propriedade C#.
- [ ] PDFs/manuais, schemas e exemplos XML relevantes foram considerados quando disponíveis.
- [ ] Classes seguem os padrões de `NFCom`, `NFe` e DFe semelhante.
- [ ] Classes auxiliares de grupos/tags usam o mesmo nome da tag XML, sem sufixo do documento salvo quando a tag contém esse sufixo ou há conflito real.
- [ ] Cada XSD principal aplicável tem classe raiz e arquivo `.cs` próprios.
- [ ] As tags dos XSDs foram modeladas com propriedades explícitas, não com conteúdo XML genérico.
- [ ] Não há uso de `XmlElement[]`, `XmlAnyElement`, `XmlAnyAttribute`, `object`, `dynamic` ou XML bruto para tags conhecidas.
- [ ] A ordem das propriedades segue `xs:sequence`.
- [ ] `xs:choice` foi modelado como alternativas reais.
- [ ] Atributos do XSD foram implementados com `[XmlAttribute]`.
- [ ] Grupos aninhados não foram achatados no nível errado.
- [ ] Campos opcionais de tipo valor/enum/número não serializam default indevido.
- [ ] Campos de protocolo/retorno/evento foram conferidos no grupo correto do XSD.
- [ ] Cada propriedade foi tipada pelo XSD, documentação e padrão de DFe equivalente antes de codificar.
- [ ] Datas/hora não ficaram como `string`, salvo justificativa real do schema/padrão.
- [ ] Valores monetários, percentuais, alíquotas e quantidades decimais usam `double` + `...Field` com cultura invariável.
- [ ] Quantidades inteiras sem zeros à esquerda usam `int`/`long`.
- [ ] Propriedades `long` em classes públicas de XML foram tratadas para `INTEROP` com alternativa `string` quando necessário.
- [ ] UF, ambiente, modelo, CRT, emissão, modalidade e indicadores fechados usam enum/tipo existente ou enum novo centralizado.
- [ ] Strings remanescentes em campos aparentemente numéricos/codificados foram conferidas e têm motivo: texto livre, zeros à esquerda, código textual oficial, chave/hash/documento ou compatibilidade comprovada.
- [ ] Foi feita varredura por `public string` suspeito em campos `V*`, `P*`, `Q*`, `Tp*`, `Dh*`, `UF`, `CRT`, `CST`, `Ind*` e similares.
- [ ] Cada propriedade/tag foi analisada para identificar valores predefinidos.
- [ ] Enums existentes em `Enums.cs` e no projeto foram reaproveitados quando compatíveis.
- [ ] Novos enums foram criados para domínios fechados ainda inexistentes.
- [ ] Tags com domínio fechado não ficaram como primitivo sem justificativa.
- [ ] Código do projeto principal continua compatível com C# 7.3 e `netstandard2.0`.
- [ ] Estrutura reflete fielmente schemas/XMLs de referência.
- [ ] Atributos XML foram aplicados corretamente.
- [ ] Namespaces XML oficiais foram preservados.
- [ ] INTEROP segue o padrão existente.
- [ ] Todas as classes públicas de XML, incluindo classes auxiliares, têm bloco `INTEROP` com `ClassInterface`, `ProgId` e `ComVisible`.
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

Enums reutilizados/criados:
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
