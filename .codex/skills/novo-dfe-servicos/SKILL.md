---
name: novo-dfe-servicos
description: Use quando Codex precisar implementar ou revisar serviços de consumo de webservices/APIs para um novo documento fiscal eletrônico na DLL Unimake.DFe, a partir do nome da subpasta do documento e da pasta de documentação oficial com PDFs/arquivos técnicos, usando classes XML já existentes, padrões NFCom/NFe/CTe, configurações por UF e testes reais com Executar().
---

# Novo DF-e - Serviços

## Objetivo

Implementar os serviços de consumo de webservices/APIs para um novo documento fiscal eletrônico na DLL `Unimake.DFe`, usando as classes de serialização/desserialização já existentes e seguindo exatamente o padrão atual do projeto.

Esta skill deve ser usada depois da implementação das classes XML do documento.

A entrada obrigatória do usuário deve conter:

1. o nome da subpasta/documento;
2. o caminho da pasta onde ficam PDFs, manuais, WSDLs, URLs, exemplos XML, notas técnicas, tabelas e demais arquivos técnicos da documentação do novo documento.

Exemplos:

```text
Documento: DCe
Documentação: C:\docs\DCe

Documento: NFGas
Documentação: C:\docs\NFGas

Documento: NFCom
Documentação: C:\docs\NFCom
```

Com `{Documento}` informado, trabalhar principalmente em:

```text
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Servicos\{Documento}
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Servicos\Config\{Documento}
C:\projetos\github\Unimake.DFe\source\Unimake.DFe.Test\{Documento}
```

Se o nome da subpasta/documento ou a pasta de documentação não forem informados, solicite somente as informações faltantes antes de alterar arquivos.

## Exemplo de uso

Exemplo de mensagem do usuário para acionar a skill:

```text
Use a skill novo-dfe-servicos.

Documento: DCe
Documentação: C:\projetos\docs\DCe
```

## Princípio central

Não invente um pipeline novo.

Implemente os serviços do novo documento como uma extensão natural dos padrões já usados em `NFCom`, `NFe` e `CTe`.

Os testes não devem apenas instanciar classes ou simular chamadas. Sempre que o padrão do projeto e a infraestrutura de testes permitirem, cada teste de serviço deve:

1. criar o objeto XML de envio usando as classes em `Xml\{Documento}`;
2. criar o serviço passando o objeto XML, e não XML bruto em string;
3. chamar `Executar()`;
4. validar retorno, XML enviado, XML retornado, status, exceções e comportamento esperado conforme padrão existente.

## Pré-condições obrigatórias

Antes de implementar serviços, valide:

1. A pasta de documentação informada existe e pode ser lida.
2. A pasta abaixo existe:

```text
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Xml\{Documento}
```

3. Existem classes XML suficientes para os serviços que serão implementados.
4. Existem classes de retorno correspondentes aos serviços.
5. Existem exemplos, configs ou documentação técnica suficientes para identificar endpoints, versões, métodos e mensagens.

Se as classes XML ainda não existirem ou estiverem incompletas, pare e informe que a skill de serialização/desserialização do documento deve ser executada primeiro.

Não implemente classes XML dentro desta skill, exceto pequenos ajustes indispensáveis para compilar os serviços e desde que sigam o padrão já existente.

## Referências obrigatórias

### Serviços

Analise e replique rigorosamente o padrão das pastas:

```text
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Servicos\NFCom
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Servicos\NFe
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Servicos\CTe
```

Use também o serviço de documento fiscal eletrônico mais semelhante ao novo documento, se existir no checkout.

### Configuração

Analise e replique rigorosamente o padrão de:

```text
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Servicos\Config\NFCom
```

Use essa pasta como fonte de verdade para:

- nomes dos arquivos por UF;
- estrutura das tags XML;
- organização por ambiente;
- nomes de serviços;
- tags de versão;
- padrão de URLs;
- QRCode, quando existir;
- forma como arquivos são incluídos e consumidos pelo projeto.

### Testes

Analise e replique o padrão de:

```text
C:\projetos\github\Unimake.DFe\source\Unimake.DFe.Test\NFCom
```

Use também testes de `NFe`, `CTe` ou do DFe mais semelhante quando forem mais próximos do tipo de serviço implementado.

## Antes de implementar

1. Localize e leia a pasta de documentação informada pelo usuário.
2. Identifique PDFs, notas técnicas, manuais, WSDLs, URLs, exemplos XML, tabelas de serviços, leiautes, ambientes e arquivos auxiliares.
3. Use a documentação informada como fonte principal para endpoints, web actions, versões, métodos SOAP/API, ambientes, UFs, mensagens de envio/retorno, certificado e regras de execução.
4. Localize a pasta `Servicos\{Documento}`.
5. Localize a pasta `Servicos\Config\{Documento}`.
6. Localize a pasta `Xml\{Documento}`.
7. Localize a pasta `Unimake.DFe.Test\{Documento}`.
8. Verifique se já existem serviços, configs ou testes parciais.
9. Inventarie todos os serviços possíveis a partir de:
   - classes XML de envio;
   - classes XML de retorno;
   - variantes do mesmo documento, quando existirem, como autorização normal, simplificada, modal específico ou serviço técnico separado;
   - arquivos de configuração existentes;
   - WSDLs/URLs já presentes em configs;
   - exemplos XML;
   - documentação técnica informada na pasta obrigatória;
   - documentação técnica presente no repositório ou informada no contexto da tarefa;
   - padrões de NFCom/NFe/CTe.
10. Inventarie também todos os registros de infraestrutura necessários fora de `Servicos\{Documento}`: enums, configs embutidos, validação central, detecção de tipo de DFe, isoladores e schemas.
11. Implemente todos os serviços aplicáveis identificados.
12. Não implemente apenas `StatusServico` se houver classes/configs para autorização, consulta, recepção de evento, distribuição, inutilização ou outros serviços aplicáveis.

Se houver dúvida real sobre endpoint, método SOAP, versão ou mensagem, pare e solicite o dado faltante. Não adivinhe integração fiscal.

## Documentação obrigatória

A pasta de documentação é obrigatória e deve orientar a implementação dos serviços.

Ao analisá-la:

- procure recursivamente PDFs, notas técnicas, manuais, WSDLs, arquivos `.wsdl`, `.xml`, `.json`, `.txt`, exemplos de requisição/retorno e tabelas de endpoints;
- identifique todos os serviços descritos para o documento;
- identifique ambiente de homologação/produção, UF, web action, URL, método HTTP/SOAP, versão, namespace, envelope, headers, timeout, certificado, assinatura e formato de retorno;
- compare a documentação com os padrões existentes de `NFCom`, `NFe` e `CTe`;
- não implemente endpoint, método ou versão por suposição se a documentação não confirmar;
- quando documentação e padrão do projeto divergirem, preserve o padrão técnico do projeto sem violar a especificação oficial.

Se a documentação estiver incompleta, ilegível, inacessível ou contraditória, pare e peça esclarecimento ou arquivo complementar. Não adivinhe integração fiscal.

## Padrões técnicos do projeto

### Compatibilidade

- O projeto principal é `netstandard2.0` com `LangVersion` 7.3.
- Não use recursos incompatíveis com C# 7.3 no projeto principal: file-scoped namespace, records, nullable reference types, init-only setters, global usings, collection expressions ou APIs indisponíveis em `netstandard2.0`.
- Testes podem usar o padrão já existente do projeto de testes, sem modernização desnecessária.

### Organização

Crie ou ajuste arquivos em:

```text
source\.NET Standard\Unimake.Business.DFe\Servicos\{Documento}
source\.NET Standard\Unimake.Business.DFe\Servicos\Config\{Documento}
source\Unimake.DFe.Test\{Documento}
```

Não altere arquivos fora dessas áreas, exceto quando indispensável para registrar o serviço, incluir enum, ajustar roteamento de configuração ou manter compatibilidade com o padrão existente.

### Namespace e nomes

Siga exatamente o padrão existente para:

- namespace;
- nome de classes;
- nome de arquivos;
- construtores;
- herança;
- propriedades;
- métodos;
- classes `Servico`;
- classes `Request`;
- classes `Result`;
- enums ou tipos auxiliares.

Não crie novo padrão de nomenclatura.

### Herança e pipeline

Utilize as mesmas classes base, interfaces e pipeline de execução já existentes.

Siga o padrão para:

- montagem da requisição;
- serialização do XML de envio;
- assinatura, quando aplicável;
- envelope SOAP/HTTP;
- headers;
- versão do serviço;
- seleção de ambiente;
- seleção de UF;
- certificado digital;
- timeout;
- envio;
- leitura do retorno;
- desserialização;
- tratamento de rejeição;
- tratamento de erro técnico;
- exceções.

Não crie abstrações novas se o projeto já possui estrutura equivalente.

Quando criar a `ServicoBase` de um novo documento, defina `Configuracoes.TipoDFe = TipoDFe.{Documento}` antes de chamar `Configuracoes.Load(GetType().Name)`, seguindo o padrão de documentos recentes. O consumidor do serviço não deve precisar informar manualmente o tipo do DFe para que a configuração seja localizada.

Use a validação centralizada existente do projeto quando o documento equivalente usa esse fluxo, normalmente por `ValidarXMLCentralizado`, em vez de criar validações paralelas dentro do serviço.

### Registros obrigatórios de infraestrutura

Ao implementar serviço para um documento novo, a alteração normalmente não fica restrita à pasta `Servicos\{Documento}`. Verifique e ajuste, quando aplicável:

- `Servicos\Enums\Enums.cs`: valores de `Servico`, `TipoDFe`, `XMLDoc` e enums auxiliares exigidos por schema ou configuração.
- `Servicos\Config\{Documento}`: `Config.xml`, arquivos por UF, arquivo base de autorizador virtual e overrides por UF.
- `source\.NET Standard\Unimake.Business.DFe\Unimake.Business.DFe.csproj`: inclusão explícita dos XMLs de configuração como `EmbeddedResource` quando o projeto exigir.
- `Servicos\Config\ValidacaoConfig.xml`: bloco do documento com roots, schemas, namespace, tags de assinatura e eventos.
- `Utility\XMLUtility.cs`: detecção do documento por root, modelo, evento ou chave, quando validação/serviço depender disso.
- `Xml\Validar\ValidarEstruturaXML.cs`: roots aceitas, normalização do XML, assinatura centralizada e ajustes pós-assinatura específicos do documento.
- `Xml\Validar\Extractors\IsoladorFactory.cs` e isolador do documento: necessário quando eventos usam `detEvento` com schemas específicos.
- `.csproj` de schemas: confirme que os XSDs usados por `ValidacaoConfig.xml` estão embutidos.

Se algum desses pontos não for necessário, apenas siga adiante. Se for necessário e ficar faltando, o serviço costuma compilar mas falhar em tempo de execução.

### Integração com XML

Use obrigatoriamente as classes em:

```text
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Xml\{Documento}
```

Os serviços devem receber ou manipular objetos XML tipados, conforme o padrão do projeto.

Não use XML bruto em string como atalho para evitar a modelagem correta do serviço.

Não duplique classes XML dentro da pasta de serviços.

Antes de criar um serviço, confira se existem classes XML de envio e de retorno para cada root envolvida. Para autorizações com variantes, crie uma classe de serviço por variante quando a documentação ou o usuário indicar raízes distintas, por exemplo autorização normal, modal específico ou modelo técnico separado.

Se existir retorno tipado mas não existir wrapper de distribuição/processamento, como um `Proc...`, não invente esse wrapper dentro desta skill. Implemente o `Result` com o retorno existente e registre a ausência do wrapper no relatório final, salvo quando o schema e a classe já existirem.

Quando testes de serviço revelarem erro de schema por tipo ou enum, corrija a classe XML ou enum conforme o XSD/manual. Não contorne a validação nem force XML bruto. Variantes do mesmo documento podem ter restrição própria para um campo que parece compartilhável, como modal permitido apenas para uma variante.

### Assinatura, suplemento e ordem XML

Quando o serviço assina XML, valide o XML final que sai do pipeline, não apenas o objeto antes da assinatura.

Se a assinatura quebrar a ordem exigida pelo XSD, implemente ajuste pós-assinatura no serviço seguindo padrões existentes, como `AjustarXMLAposAssinado`. Quando a validação central também assina ou reorganiza uma cópia do XML, replique o tratamento em `ValidarEstruturaXML.cs`.

Não remova grupos suplementares ou QRCode na normalização sem confirmar o XSD. Em alguns documentos o grupo suplementar, como `inf...Supl`, é obrigatório e precisa permanecer antes de `Signature`.

Se QRCode ou grupo suplementar for obrigatório e ainda não houver gerador claro no projeto, preserve um XML de recurso válido para testes ou implemente o gerador somente quando a especificação e o padrão existente forem suficientes.

### Certificado digital

Reutilize o mecanismo existente de certificado digital.

Não crie nova implementação para carregar, assinar, enviar ou validar certificado.

### Tratamento de retorno

Mapeie o retorno conforme o padrão existente:

- sucesso;
- rejeição;
- erro técnico;
- falha de comunicação;
- XML de retorno inválido;
- exceções do certificado;
- exceções de configuração.

Não crie novo formato de exceção se já existir equivalente no projeto.

### Documentação

Todas as classes, propriedades e métodos públicos criados devem conter `summary`, seguindo o estilo das classes existentes.

## Configuração por UF

### Regra crítica

A pasta abaixo deve existir ao final da implementação:

```text
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Servicos\Config\{Documento}
```

Crie um arquivo de configuração para cada UF seguindo exatamente o conjunto de UFs e nomes de arquivos usados em:

```text
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Servicos\Config\NFCom
```

### Arquivo base

Se existir:

```text
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Servicos\Config\{Documento}\PR.XML
```

use `PR.XML` como arquivo base.

O conteúdo dos demais arquivos de UF deve ser idêntico ao conteúdo de `PR.XML`, salvo se a documentação oficial ou padrão existente indicar diferença obrigatória por UF.

Se `PR.XML` não existir, use como base o primeiro XML válido já existente em `Config\{Documento}`.

Se nenhum XML de configuração existir para `{Documento}`, crie a estrutura a partir do padrão de `Config\NFCom` e dos endpoints disponíveis no contexto da tarefa. Se os endpoints não estiverem disponíveis, pare e peça os WSDLs/URLs faltantes.

Quando houver autorizador virtual ou arquivo base oficial equivalente, use esse arquivo como base de herança, por exemplo `SVRS.xml`. Os arquivos de UF com endpoints próprios devem usar `<Heranca>ArquivoBase.xml</Heranca>` e sobrescrever apenas URLs ou tags específicas, mantendo a estrutura completa do serviço no arquivo base.

`Config.xml` é a configuração geral do documento, com conteúdo como `WebContentType`, envelope, versão SOAP, `SchemaVersao` e `TargetNS`. Ele não substitui os arquivos por UF.

### Restrições de configuração

- Não altere nomes das tags XML.
- Não altere a estrutura das tags.
- Altere somente o conteúdo das tags.
- Preserve nomes de serviços, ambientes, versões e atributos conforme padrão do projeto.
- Não remova serviços existentes do arquivo base.
- Não deixe UF faltando quando `NFCom` possuir arquivo equivalente.
- Se alguma UF for propositalmente não aplicável ao novo documento, registre a justificativa no relatório final.
- Se a documentação tiver erro material evidente em URL ou extensão, compare com produção/homologação, WSDLs equivalentes e padrão do mesmo autorizador. Corrija apenas quando a evidência for forte e registre no relatório final.

### Inclusão no projeto

Verifique como os arquivos de configuração são tratados no projeto.

Inclua os novos arquivos no `.csproj` somente se o padrão atual exigir inclusão explícita.

Não altere o `.csproj` se o SDK/padrão do projeto já incluir os arquivos automaticamente.

No projeto atual, confirme sempre o `.csproj`: arquivos em `Servicos\Config\{Documento}` frequentemente precisam de `None Remove` e `EmbeddedResource` explícitos para `Configuracao.Load(...)` encontrá-los em tempo de execução.

## Serviços a implementar

Implemente todos os serviços aplicáveis ao `{Documento}` identificados por XMLs, configs, WSDLs e padrões existentes.

Serviços comuns que devem ser considerados quando existirem classes/configuração:

- `StatusServico`;
- `Autorizacao`;
- `RetAutorizacao` ou consulta de recibo, quando aplicável;
- `Consulta`;
- `RecepcaoEvento`;
- `Distribuicao`;
- `Inutilizacao`;
- outros serviços específicos do documento.

Para cada serviço aplicável, implemente:

- classe principal do serviço;
- request, quando o padrão exigir;
- result, quando o padrão exigir;
- construtores equivalentes aos documentos de referência;
- uso da classe XML de envio;
- uso da classe XML de retorno;
- integração com configuração;
- método de execução conforme pipeline existente;
- teste unitário/integrado correspondente.

Não deixe serviço com XML/config existente sem implementação e sem justificativa.

Para serviços de autorização:

- leia o XML de envio para definir `Configuracoes.Servico`, `CodigoUF`, `Modelo`, `TipoEmissao`, `TipoAmbiente` e `SchemaVersao`;
- use a classe de retorno própria do documento;
- use wrapper `Proc...` somente quando ele já existir por schema/classe;
- mantenha uma classe separada por variante quando houver raízes distintas;
- preserve blocos `#if INTEROP`, `ClassInterface`, `ProgId` e `ComVisible` em classes públicas quando o padrão exigir.

Para recepção de evento:

- confira se `ValidacaoConfig.xml` mapeia cada evento e seus schemas específicos;
- confira se existe isolador para validar `detEvento` sem perder a estrutura do evento;
- teste pelo menos um evento realista com o tipo de evento mais representativo.

## Testes obrigatórios

Crie ou ajuste:

```text
C:\projetos\github\Unimake.DFe\source\Unimake.DFe.Test\{Documento}\ServicosTest.cs
```

Siga o padrão de `NFCom`, `NFe`, `CTe` e do DFe mais semelhante.

### Regra principal dos testes

Todos os métodos de teste criados para serviços devem seguir o padrão do método `StatusServico`, quando este existir no próprio `ServicosTest.cs`.

Cada teste deve, sempre que aplicável:

1. criar explicitamente o objeto XML de envio;
2. preencher as propriedades mínimas necessárias no objeto;
3. criar o serviço passando esse objeto tipado;
4. chamar `Executar()`;
5. obter o retorno real do serviço ou o retorno produzido pelo pipeline padrão;
6. validar que o retorno foi desserializado na classe correta;
7. validar que o XML de envio foi serializado corretamente;
8. validar que não houve quebra no pipeline de configuração/certificado/transporte.

Não crie testes que apenas instanciam o serviço sem chamar `Executar()`.

Não crie testes que enviam XML bruto quando já existe classe XML correspondente.

Não deixe os demais métodos em padrão inferior ao `StatusServico`.

Quando usar XML de recurso para autorização ou evento, remova assinaturas vazias ou placeholders do objeto antes de executar o serviço, se o padrão do teste exigir assinatura real. Preserve, porém, grupos suplementares ou QRCode quando o XSD os exigir.

### Objeto XML em todos os serviços

Para cada serviço testado, monte o objeto XML correspondente.

Exemplos conceituais:

- para status, crie o objeto de consulta de status;
- para autorização, crie o objeto do documento/autorização;
- para consulta, crie o objeto de consulta;
- para recepção de evento, crie o objeto de evento;
- para distribuição, crie o objeto de distribuição;
- para inutilização, crie o objeto de inutilização.

O objetivo é testar também as classes XML usadas pelo serviço.

### Execução real do pipeline

Quando o padrão do projeto permitir, chame `Executar()` para consumir o serviço real ou o pipeline configurado de teste.

Isso é obrigatório para validar:

- leitura da configuração;
- seleção do endpoint;
- montagem do envelope;
- certificado;
- serialização do XML de envio;
- transporte;
- leitura do retorno;
- desserialização;
- tratamento de erro/rejeição.

Não substitua `Executar()` por mock se o padrão existente do documento equivalente consome o serviço real.

Use mock apenas quando o padrão existente para aquele tipo de serviço usar mock ou quando a chamada real for tecnicamente impossível no ambiente de teste. Nesse caso, registre a justificativa no relatório final.

### Certificado e ambiente

Use o mesmo padrão de certificado, empresa, UF e ambiente dos testes existentes.

Não crie caminho fixo novo de certificado se já houver infraestrutura de teste.

Não inclua certificado real no repositório.

Se o serviço exigir certificado e o ambiente de teste não possuir certificado disponível, mantenha o teste no padrão do projeto para esse cenário e registre a limitação no relatório final.

### Traits e filtros

Use traits equivalentes ao padrão local, preferencialmente:

```csharp
[Trait("DFe", "{Documento}")]
```

e, quando útil:

```csharp
[Trait("Servico", "{NomeDoServico}")]
```

Se a implementação de serviços exigir ajuste em classe XML, enum, recurso XML ou normalização de assinatura, rode também os testes de serialização/desserialização do documento além dos testes de serviço.

## Execução de testes

Não rode toda a suíte por padrão.

Execute somente os testes criados ou alterados, filtrando por classe, método ou trait do documento. Exemplos:

```powershell
dotnet test source\Unimake.DFe.Test\Unimake.DFe.Test.csproj --no-restore --filter "FullyQualifiedName~Unimake.DFe.Test.{Documento}.ServicosTest"

dotnet test source\Unimake.DFe.Test\Unimake.DFe.Test.csproj --no-restore --filter "DFe={Documento}"
```

Quando classes XML ou recursos do documento forem alterados durante a implementação dos serviços, rode também:

```powershell
dotnet test source\Unimake.DFe.Test\Unimake.DFe.Test.csproj --no-restore --filter "FullyQualifiedName~Unimake.DFe.Test.{Documento}.SerializacaoDesserializacaoTest"
```

Rode todos os testes apenas se o usuário solicitar ou se a alteração atingir infraestrutura compartilhada do projeto.

## Diagnóstico rápido de falhas comuns

Use estes sintomas para ir direto ao ponto:

- `Não foi possível encontrar a configuração para o tipo de DFe`: falta `TipoDFe`, `ValidacaoConfig.xml`, detecção em `XMLUtility.cs` ou `Configuracoes.TipoDFe` antes de `Load`.
- Configuração não encontrada em runtime apesar do XML existir: falta `EmbeddedResource` no `.csproj` ou nome/caminho do recurso diverge do padrão.
- `Não existe um isolador de XML configurado`: falta isolador do documento ou registro em `IsoladorFactory.cs`.
- Schema reclama que `Signature` apareceu antes de grupo suplementar: falta ajuste de ordem pós-assinatura no serviço e possivelmente na validação central.
- Schema reclama de enum ou valor fixo de variante: tipo compartilhado está amplo ou errado para o XSD dessa variante; crie enum/tipo específico.
- Evento falha apenas no `detEvento`: falta mapeamento do evento/schema em `ValidacaoConfig.xml` ou isolador do documento.
- Serviço compila mas `Configuracao.Load(GetType().Name)` falha: confira nome da classe de serviço, tag correspondente no XML de config e `Servico` enum.

## Build

Quando possível, compile o projeto principal:

```powershell
dotnet build "source\.NET Standard\Unimake.Business.DFe\Unimake.Business.DFe.csproj" --no-restore
```

Se o build falhar por dependência/restauração ausente, informe isso no resultado e não tente baixar pacotes sem aprovação.

## Restrições

- Não implementar serialização/desserialização completa nesta skill.
- Não criar novo pipeline de serviço.
- Não criar nova infraestrutura de certificado.
- Não criar novo formato de configuração.
- Não alterar estrutura das tags XML dos arquivos de configuração.
- Não deixar de criar arquivos de configuração por UF quando houver arquivo base.
- Não criar apenas `PR.XML` quando o padrão exige uma configuração para cada UF.
- Não criar testes que apenas instanciam serviços sem chamar `Executar()`.
- Não criar testes de serviço usando XML bruto quando existe objeto XML tipado.
- Não deixar métodos de teste em padrão inferior ao método `StatusServico`.
- Não duplicar classes XML, enums, helpers, requests ou results já existentes.
- Não atualizar framework, pacotes, versão de linguagem ou dependências.
- Não fazer refatoração ampla junto da implementação.
- Não executar toda a suíte de testes se for possível executar apenas os testes criados/alterados.
- Não ignorar serviço aplicável sem registrar motivo no relatório final.
- Não implementar endpoint, WSDL, URL, web action, versão ou método por suposição quando a documentação não confirmar.

## Checklist antes de finalizar

- [ ] A entrada `{Documento}` foi usada para namespace, pasta, configs e testes.
- [ ] A pasta de documentação informada foi analisada.
- [ ] PDFs/manuais/WSDLs/URLs/exemplos XML relevantes foram considerados quando disponíveis.
- [ ] As classes XML em `Xml\{Documento}` foram localizadas e usadas.
- [ ] Serviços existentes em NFCom/NFe/CTe foram analisados.
- [ ] Configuração de NFCom foi analisada como padrão.
- [ ] Todos os serviços aplicáveis foram inventariados.
- [ ] Todos os serviços aplicáveis foram implementados ou tiveram bloqueio/justificativa registrado.
- [ ] As classes de serviço seguem o padrão de herança e pipeline existente.
- [ ] Requests/results seguem o padrão do projeto.
- [ ] Certificado digital foi reutilizado conforme padrão existente.
- [ ] Tratamento de retorno e exceções segue o padrão atual.
- [ ] `Servico`, `TipoDFe`, `XMLDoc` e enums auxiliares foram registrados quando necessários.
- [ ] A `ServicoBase` do documento define `Configuracoes.TipoDFe` antes de carregar a configuração.
- [ ] Arquivos de configuração foram criados para todas as UFs exigidas pelo padrão de NFCom.
- [ ] O conteúdo dos arquivos por UF foi copiado do arquivo base do documento, especialmente `PR.XML` quando existir.
- [ ] Arquivo base ou autorizador virtual foi usado com herança quando aplicável.
- [ ] Nenhuma tag XML dos arquivos de configuração foi renomeada.
- [ ] Nenhuma estrutura XML de configuração foi alterada indevidamente.
- [ ] Os WSDLs/URLs foram aplicados corretamente quando disponíveis.
- [ ] XMLs de configuração foram incluídos como `EmbeddedResource` quando o projeto exigiu.
- [ ] `ValidacaoConfig.xml` foi atualizado para roots, schemas, namespace, assinatura e eventos.
- [ ] `XMLUtility.cs` e `ValidarEstruturaXML.cs` foram atualizados quando o documento exigiu detecção ou normalização própria.
- [ ] Isolador e `IsoladorFactory.cs` foram atualizados quando eventos ou validação específica exigiram.
- [ ] Ordem final do XML assinado foi validada contra o XSD quando houve assinatura.
- [ ] Grupos suplementares/QRCode obrigatórios foram preservados ou gerados corretamente.
- [ ] `ServicosTest.cs` foi criado ou ajustado.
- [ ] Todos os testes de serviço montam objeto XML tipado.
- [ ] Todos os testes de serviço chamam `Executar()` quando aplicável.
- [ ] Os testes seguem o padrão do método `StatusServico`.
- [ ] Apenas os testes criados/alterados foram executados.
- [ ] Testes de serialização/desserialização foram executados quando classes XML, enums, recursos XML ou normalização foram alterados.
- [ ] Build/testes executados foram registrados no relatório final.
- [ ] Código compila sem warnings relevantes ou a limitação foi registrada.

## Saída esperada

Ao finalizar, responda somente com um relatório objetivo:

```text
Documento implementado:
- {Documento}

Pasta de documentação:
- ...

Arquivos criados/modificados:
- ...

Serviços implementados:
- ...

Serviços não implementados e motivo:
- ...

Arquivos de configuração criados:
- ...

Arquivo base usado para configurações por UF:
- ...

WSDLs/URLs aplicados:
- ...

Arquivos de documentação consultados:
- ...

Estruturas reutilizadas:
- ...

Testes criados/alterados:
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
