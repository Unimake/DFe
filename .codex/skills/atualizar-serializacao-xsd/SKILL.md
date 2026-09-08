---
name: atualizar-serializacao-xsd
description: Use quando Codex precisar analisar alterações em um ou mais schemas XSD existentes da Unimake.DFe, atualizar somente as classes C# de serialização realmente afetadas e auditar impactos das tags preexistentes alteradas em todo o projeto, rastreando dependências, consumidores, enums e testes. Não use para criar a serialização completa de um DFe novo.
---

# Atualizar serialização a partir de XSD alterado

## Objetivo

Atualizar classes existentes de serialização/desserialização após mudanças em XSDs, com rastreabilidade entre a alteração do schema, o caminho XML afetado e a classe C# correspondente.

O nome de uma tag não é evidência suficiente de impacto. Só altere uma classe quando a relação semântica e estrutural estiver demonstrada pelo grafo dos schemas e pelo mapeamento XML da classe.

## Entradas e portão obrigatório

Obtenha estas entradas:

1. um ou mais arquivos XSD alterados;
2. uma ou mais pastas que contenham as classes de serialização a revisar;
3. opcionalmente, a pasta-raiz que contém o conjunto de schemas relacionado;
4. opcionalmente, a revisão-base, arquivos anteriores ou outra fonte para identificar o que mudou.

As pastas das classes de serialização são obrigatórias e nunca devem ser inferidas apenas pelo nome do DFe, namespace, tag ou caminho dos XSDs. Se não forem fornecidas, pergunte ao usuário e não prossiga com análise, edição, build ou testes até receber a resposta.

Se nenhum XSD alterado for informado, peça os arquivos. Se os XSDs forem informados, mas a pasta-raiz dos schemas não for:

- para um arquivo, use inicialmente a pasta que o contém;
- para vários arquivos, use o ancestral comum mais próximo que contenha todos eles;
- amplie para uma pasta ancestral somente quando imports/includes resolvidos ou a organização local demonstrarem que o conjunto relacionado está acima;
- informe a pasta inferida antes de executar alterações;
- se houver mais de uma raiz plausível com impacto material diferente, peça confirmação.

Não trate caminhos mostrados em pedidos anteriores ou exemplos desta skill como valores fixos. Use sempre as entradas da invocação atual.

## Fontes e limites

- O XSD é o contrato principal da estrutura XML.
- O diff real do XSD define o ponto de partida; não suponha que o arquivo inteiro seja novo.
- Use Git para obter a versão anterior quando a mudança estiver pendente ou a revisão-base estiver identificável. Se não houver diff recuperável, peça ao usuário a versão anterior ou a revisão-base antes de modificar classes.
- Use classes existentes, testes, XMLs reais e documentação técnica para confirmar o padrão do projeto e a intenção semântica.
- Faça a auditoria de impacto em todo o checkout para componentes preexistentes alterados, mesmo que as pastas de classes informadas sejam mais estreitas. As pastas informadas limitam onde revisar e alterar classes de serialização; não limitam a busca por consumidores que podem quebrar.
- Não altere os XSDs recebidos, serviços, configurações ou outras áreas, salvo pedido explícito ou necessidade indispensável demonstrada.
- Preserve `netstandard2.0`, C# 7.3, API pública e compatibilidade `INTEROP`/COM.

## Investigação obrigatória antes de codificar

### 1. Identificar o diff estrutural

Compare cada XSD alterado com sua versão-base usando análise XML, não apenas diff textual. Classifique separadamente cada mudança como:

- **inserção**: componente sem correspondente na versão-base;
- **alteração**: componente preexistente cujo contrato mudou;
- **remoção**: componente preexistente que deixou de existir;
- **movimentação ou renomeação provável**: somente quando estrutura, contexto, documentação ou histórico fornecerem evidência; não deduza apenas por similaridade nominal.

Uma alteração pode envolver nome, tipo, posição, ancestral, namespace, cardinalidade, obrigatoriedade, domínio, formato ou restrição. Registre mudanças em:

- elementos e atributos;
- tipos simples e complexos;
- `xs:sequence`, `xs:choice`, `xs:all` e grupos;
- `minOccurs`, `maxOccurs`, `use`, defaults e valores fixos;
- tipo-base, extensão, restrição, union e list;
- facets como `xs:enumeration`, pattern, tamanho, intervalo e casas decimais;
- `nillable`, `abstract`, `mixed`, `form`, `elementFormDefault`, `attributeFormDefault`, `block`, `final` e qualificação de namespace;
- `substitutionGroup`, elementos referenciados, wildcards `xs:any`/`xs:anyAttribute` e `processContents`;
- namespaces, imports, includes e redefinições;
- tipos, grupos e `attributeGroup` globais;
- raízes ou mensagens que passaram a alcançar o componente alterado.

Desconsidere diferenças puramente de formatação, ordem de declarações globais sem efeito e alterações de comentário/documentação que não mudem o contrato. Não conclua impacto em C# somente porque uma linha ou um nome aparece no diff.

Não trate remoção seguida de inserção como renomeação automaticamente. Essa associação errada pode fazer a IA modificar a classe ou o grupo incorreto.

### 2. Construir o grafo de schemas

Antes de pesquisar classes, inventarie recursivamente os `.xsd` da pasta-raiz e resolva, em ambos os sentidos:

- `xs:include`, `xs:import` e `xs:redefine` por `schemaLocation` e namespace;
- referências QName por `type`, `base`, `ref`, `group` e `attributeGroup`;
- tipos anônimos e tipos globais;
- extensão e restrição de tipos;
- `substitutionGroup`, tipos abstratos e tipos derivados usados de forma polimórfica;
- elementos raiz, tipos ou grupos que consomem direta ou transitivamente cada componente alterado;
- versões paralelas do leiaute que reutilizam nomes, mas não o mesmo componente.

Siga as dependências até os schemas raiz/mensagens consumíveis. Resolva caminhos reais e namespaces; não associe arquivos apenas por nomes parecidos. Quando viável, carregue e compile cada fechamento relevante em um `XmlSchemaSet` sem acesso à internet para detectar referências não resolvidas, colisões de tipos e combinações inválidas. Não misture no mesmo fechamento versões mutuamente exclusivas apenas para fazê-las compilar juntas.

Registre dependências ausentes ou ambíguas e não avance sobre o ramo afetado enquanto não forem esclarecidas. Um XSD compilado isoladamente não prova que a mensagem raiz ou a versão consumida pelo projeto está correta.

### 3. Traçar o impacto XML

Para cada componente realmente alterado, determine:

```text
XSD alterado e componente:
Mudança estrutural:
Caminho de dependência até o schema raiz:
Namespace e versão:
Caminho XML completo esperado:
Cardinalidade e ordem:
Classes/pastas candidatas informadas pelo usuário:
Classe, propriedade e atributos XML correspondentes:
Evidência de correspondência:
Ação necessária ou motivo para nenhuma alteração:
```

A evidência deve combinar, conforme aplicável:

- raiz e namespace de `[XmlRoot]`;
- cadeia de propriedades e classes que reproduz os grupos ancestrais;
- `[XmlElement]`, `[XmlAttribute]`, `[XmlArray]`, `[XmlArrayItem]` ou `[XmlText]`;
- tipo do grupo, posição na sequência, choice e cardinalidade;
- versão/schema usado pela mensagem;
- testes ou XMLs que percorrem o mesmo caminho.

Para alteração de facet que não muda o tipo C#, registre explicitamente `sem mudança estrutural na classe` e continue a auditoria de validação, massas XML e consumidores. Não force uma mudança de propriedade apenas para refletir pattern, comprimento ou intervalo que o projeto deixa a cargo do schema.

### Proibição crítica de busca nominal

Nunca altere uma classe só porque uma busca textual encontrou uma propriedade, classe ou tag com o mesmo nome do XSD.

Buscas por nome servem apenas para levantar candidatos. Antes de editar, comprove que o candidato pertence ao mesmo:

- documento ou mensagem raiz;
- namespace e versão aplicáveis;
- grupo/tipo do XSD;
- caminho de ancestrais no XML;
- papel semântico;
- cardinalidade e alternativa de `xs:choice`, quando houver.

Tags homônimas em grupos como emitente, destinatário, endereço, totais, impostos, protocolos, eventos ou documentos distintos não são intercambiáveis. Se a correspondência não puder ser provada, não altere a classe; registre a dúvida e peça esclarecimento quando ela impedir o trabalho.

## Auditoria de impacto no projeto para tags alteradas

Esta auditoria é obrigatória para toda **alteração, remoção, movimentação ou renomeação comprovada de componente preexistente**. Ela não se aplica ao relatório especial de regressão para uma tag puramente inserida, embora inserções continuem exigindo implementação e testes normais.

Para cada componente preexistente alterado, percorra todo o checkout e procure consumidores da classe, propriedade e contrato XML antigos e novos. Combine busca de símbolos C# com contexto estrutural; uma pesquisa pela string da tag, sozinha, não basta nem para confirmar uso nem para descartá-lo.

Revise, conforme existirem:

- atribuições, leituras, object initializers, construtores, métodos auxiliares, extensões, clones e cópias;
- propriedades `...Field`, `...Specified`, `ShouldSerialize...`, helpers `Add...`/`Get...` e contadores INTEROP;
- casts, comparações, switches, parsing, formatação e conversões de enums ou tipos primitivos;
- serviços que leem dados do XML em `DefinirConfiguracao`, assinatura, identificação, chave, ambiente, versão, modelo ou roteamento;
- validators manuais, `ValidateTag`, regras de conteúdo e resolução centralizada;
- builders, parsers, mappers, transportes, conversores TXT/XML, importadores, exportadores e integrações UniNFe;
- reflection, nomes de propriedades em strings, `nameof`, expressões, binding, COM/ProgId e consumidores que não geram referência estática fácil de localizar;
- configurações de schema/versão, recursos embutidos, caminhos de XSD e entradas de `.csproj` quando nome, pasta, include/import ou versão mudou;
- XMLs em `Resources`, exemplos e massas de testes que contenham o caminho afetado;
- documentação executável, snippets ou exemplos compiláveis somente quando façam parte do build ou representem risco concreto que deva ser relatado.

Classifique o risco de cada consumidor:

```text
Componente preexistente alterado:
Mudança anterior -> nova:
Consumidor e localização:
Forma de dependência: símbolo, XML, reflection/string, configuração ou massa de teste
Falha provável: compilação, serialização, desserialização, validação, runtime, COM ou compatibilidade
Ação tomada:
Teste que comprova a correção:
Risco residual ou ação externa:
```

Atualize consumidores internos diretamente afetados quando isso for consequência necessária da mudança solicitada e puder ser feito com segurança. Se a correção exigir decisão de API pública, mudança incompatível, alteração fora do checkout ou ampliação material de escopo, não improvise: preserve o que for possível, registre o risco e peça direção.

O build encontra referências estáticas quebradas, mas não substitui esta auditoria. Reflection, strings, XMLs, configurações, COM e mudanças sem erro de compilação precisam de conferência própria.

## Decisão de escopo

Somente depois do grafo e da matriz de impacto:

- classifique cada pasta de serialização fornecida como afetada ou não afetada;
- explique por que pastas aparentemente relacionadas não recebem mudança;
- se a alteração alcançar um tipo compartilhado por múltiplas raízes, revise todos os consumidores transitivos dentro das pastas autorizadas;
- identifique se a mesma classe atende múltiplas versões de schema e preserve as versões ainda suportadas; não corrija a versão nova quebrando silenciosamente a anterior;
- não replique a mudança em versões, DFe ou classes homônimas que não dependam do componente alterado;
- não remova membro apenas por não aparecer no XSD examinado: confirme que ele não pertence a outro schema, versão, extensão ou choice suportado pela mesma classe.

Não comece a editar enquanto o caminho XSD raiz -> componente alterado -> caminho XML -> classe/propriedade não estiver claro.

## Implementação C#

Siga o padrão já existente na própria pasta e nas classes estruturalmente equivalentes do mesmo DFe:

- preserve nomes públicos, summaries XML, namespace em bloco, atributos de serialização, `#if INTEROP`, `ProgId`, `ComVisible` e helpers de listas;
- preserve o nome e o case exatos de elementos e atributos do XSD;
- mantenha a ordem de `xs:sequence`; use `Order` somente se o arquivo/padrão relacionado já o exigir;
- modele `xs:choice` pelo padrão compatível já usado no contexto, sem criar wrappers inexistentes;
- preserve pares de `XmlChoiceIdentifier`, arrays/listas paralelos e membros do enum identificador quando o padrão existente os usar;
- trate `nillable` e ausência como conceitos distintos; não confunda elemento ausente com `xsi:nil="true"`;
- preserve namespaces em elementos e atributos qualificados, inclusive quando `form` ou defaults do schema mudarem;
- reveja `[XmlInclude]`, alternativas `[XmlElement(typeof(...))]` e tipos derivados quando houver herança, tipo abstrato ou `substitutionGroup`;
- preserve grupos aninhados e não achate propriedades;
- use `List<T>` ou array conforme o padrão local para `maxOccurs > 1`;
- trate `minOccurs="0"` e atributos opcionais sem serializar defaults indevidos, usando nullable, `ShouldSerialize...`, `...Specified` ou propriedade auxiliar conforme o padrão local;
- use `DateTime`, `DateTimeOffset`, tipos numéricos ou propriedades `...Field` quando o formato XML exigir conversão, respeitando o padrão local;
- preserve como `string` códigos com zeros à esquerda, chaves, documentos e formatos cujo texto faça parte do contrato;
- não invente validações manuais a partir de facets se o projeto normalmente apenas as valida pelo schema;
- não faça refatorações estéticas ou mudanças fora da matriz de impacto.

Antes de criar classe, propriedade ou helper, procure implementação semanticamente equivalente. Reutilize somente quando namespace, estrutura e significado forem compatíveis.

## Regra obrigatória para domínios fechados e enums

Para toda mudança que introduza ou altere `xs:enumeration`, lista fechada de códigos ou outro domínio finito:

1. localize `source/.NET Standard/Unimake.Business.DFe/Servicos/Enums/Enums.cs` no checkout;
2. procure primeiro enum já existente com a mesma semântica e representação XML;
3. confirme compatibilidade de todos os valores, inclusive valores legados, sentinelas, aliases e formato serializado;
4. reutilize o enum somente se a compatibilidade for real, não apenas porque alguns códigos coincidem;
5. se não existir enum compatível, crie-o em `Enums.cs`, seguindo nomenclatura, summaries, valores e atributos usados no arquivo;
6. use o enum na propriedade correspondente, com propriedade auxiliar `...Field`, nullable, sentinela ou `ShouldSerialize...` quando necessário ao contrato XML;
7. nunca crie enum dentro da classe de serialização ou em arquivo novo;
8. nunca duplique enum equivalente.

Não converta automaticamente uma propriedade pública existente para enum sem avaliar compatibilidade de API/COM e o padrão das classes relacionadas. Quando houver risco de quebra, preserve a API pública por uma propriedade auxiliar compatível ou reporte o conflito antes de fazer mudança incompatível.

## Testes obrigatórios

Localize os testes já relacionados ao DFe, schemas raiz, classes e caminhos XML afetados antes de editar as expectativas. Não localize testes somente pelo nome da tag: siga o tipo raiz desserializado, recursos XML usados, validação de schema, namespace do teste, trait e chamadas às classes/propriedades impactadas.

Sempre que for viável, execute primeiro os testes relacionados no estado encontrado para registrar a linha de base. Depois da implementação, execute novamente todos os testes relacionados identificados, além dos testes novos ou alterados. Se algum quebrar:

- determine se a falha revela erro na classe, consumidor, massa XML, schema selecionado ou expectativa obsoleta;
- corrija a causa compatível com o novo contrato;
- não remova teste, não enfraqueça asserção e não atualize expectativa cegamente apenas para obter sucesso;
- preserve teste de versão anterior quando ela continuar suportada;
- registre falhas preexistentes separadamente das introduzidas pela mudança.

Crie regressão mínima quando a cobertura existente não provar o comportamento novo.

Os testes devem comprovar a mudança estrutural, e não apenas que a classe compila. Conforme o caso, cubra:

- desserialização do novo elemento, atributo, enum, choice ou cardinalidade;
- serialização no nome, namespace, nível e ordem corretos;
- omissão de campo opcional quando não informado;
- múltiplas ocorrências para listas;
- round-trip com XML realista e validação contra o XSD raiz correto;
- consumidores transitivos distintos quando um tipo compartilhado foi alterado;
- ausência de mudança em versão ou raiz não afetada, quando houver risco de falso positivo.

Coloque testes na subpasta por responsabilidade, namespace e `[Trait("DFe", "...")]` coerentes com o primeiro diretório. Atualize recursos e `.csproj` somente quando necessário.

Execute os testes relacionados identificados, incluindo os preexistentes, e evite grupos amplos quando um filtro por classe ou método cobrir o fechamento afetado. Para xUnit v3, compile o projeto de testes e execute a DLL diretamente por classe/método quando esse for o padrão aceito no checkout. Sempre que executar testes unitários da DLL, execute também os testes correspondentes do projeto `C:\projetos\github\UniNFe\source\UniNFe.Test\UniNFe.Test.csproj` em `Debug`, com filtro equivalente quando existir.

Compile também o projeto principal, quando possível:

```powershell
dotnet build "source\.NET Standard\Unimake.Business.DFe\Unimake.Business.DFe.csproj" --no-restore
```

Não rode toda a suíte por padrão.

## Revisão final

Antes de concluir, confirme:

- todos os XSDs alterados foram comparados com uma base real;
- inserções foram separadas de alterações, remoções, movimentações e renomeações comprovadas;
- imports/includes e referências diretas e reversas foram seguidos;
- os fechamentos de schemas relevantes foram compilados separadamente quando viável;
- raízes e versões consumidoras foram identificadas;
- cada edição C# possui uma linha de evidência na matriz de impacto;
- nenhuma edição foi motivada somente por coincidência de nome;
- cada componente preexistente alterado teve consumidores pesquisados em todo o checkout;
- impactos em serviços, validators, conversores, reflection/strings, configurações, COM, recursos e massas XML foram corrigidos ou relatados;
- todas as pastas fornecidas foram classificadas como afetadas ou não afetadas;
- elementos, atributos, ordem, choice e cardinalidade refletem o schema;
- enums foram reutilizados ou centralizados em `Enums.cs` com compatibilidade comprovada;
- API pública, C# 7.3, `netstandard2.0` e INTEROP foram preservados;
- testes focados cobrem o comportamento alterado;
- testes preexistentes relacionados foram localizados, executados e corrigidos pela causa quando quebraram;
- compatibilidade com versões de schema ainda suportadas foi preservada ou teve risco relatado;
- arquivos não relacionados e alterações pré-existentes do usuário foram preservados.

## Relatório de saída

Entregue um relatório objetivo contendo:

```text
XSDs alterados e base comparada:
Pasta-raiz de schemas (informada ou inferida):
Grafo/dependências relevantes:
Componentes e caminhos XML afetados:
Classificação das mudanças: inseridas, alteradas, removidas ou movidas/renomeadas:
Pastas de classes analisadas:
Classes alteradas e evidência de impacto:
Pastas/classes analisadas sem alteração e motivo:
Impactos no projeto causados por tags preexistentes alteradas:
Consumidores corrigidos:
Riscos de compilação, runtime, serialização, validação, COM ou compatibilidade:
Enums reutilizados ou criados:
Testes preexistentes relacionados, resultado inicial e correções:
Testes criados/alterados e resultado final:
Build e testes do UniNFe:
Pendências, ambiguidades ou limitações:
```

Não apresente como afetada uma classe cuja correspondência estrutural não tenha sido comprovada. No relatório especial de impacto, não misture tags puramente inseridas com tags preexistentes alteradas; isso deve deixar claro quais riscos surgem de quebra de contrato existente.
