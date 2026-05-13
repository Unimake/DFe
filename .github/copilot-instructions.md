# Instruções para GitHub Copilot

Este repositório contém a biblioteca `Unimake.DFe`, usada para emissão, consulta, validação, assinatura e consumo de serviços de documentos fiscais eletrônicos brasileiros, como NFe, NFCe, CTe, MDFe, NFSe, GNRE, EFDReinf, eSocial, NFCom, NF3e, DCe, DARE, CCG e SNCM.

## Arquitetura do projeto

- A solução principal está em `source/Unimake.DFe.sln`.
- A biblioteca principal fica em `source/.NET Standard/Unimake.Business.DFe` e compila para `netstandard2.0` com `LangVersion` 7.3.
- O projeto `source/.NET Framework/Unimake.Security.Platform` mantém compatibilidade com `net472`.
- Os testes ficam em `source/Unimake.DFe.Test`, usam xUnit v3 e miram `net8.0`.
- Exemplos em `Exemplos/` atendem várias linguagens e não devem dirigir a arquitetura da biblioteca principal.

## Organização por domínio fiscal

- Preserve a separação por tipo de DFe. Quando implementar algo de NFe, CTe, NFSe etc., mantenha arquivos no respectivo diretório em `Xml/`, `Servicos/`, `Servicos/Config/`, `Validator/` e `Unimake.DFe.Test/`.
- Classes de XML ficam em `Unimake.Business.DFe.Xml.<DFe>`.
- Classes de serviço ficam em `Unimake.Business.DFe.Servicos.<DFe>`.
- Configurações de endpoints, schemas, ações SOAP/API e serviços ficam em XMLs embutidos sob `Servicos/Config/<DFe>`.
- Schemas XSD ficam em `Xml/Schemas/<DFe>` e devem ser marcados como `EmbeddedResource` quando usados em validação.

## Compatibilidade e linguagem

- No projeto principal, escreva C# compatível com C# 7.3. Não use recursos modernos como nullable reference types, records, init-only setters, file-scoped namespaces, global usings, collection expressions ou pattern matching recente.
- Preserve suporte a `INTEROP`/COM quando mexer em classes públicas usadas por outras linguagens:
  - manter blocos `#if INTEROP`;
  - aplicar `[ClassInterface(ClassInterfaceType.AutoDual)]`, `[ProgId(...)]` e `[ComVisible(true)]` quando seguir o padrão existente;
  - ocultar membros inadequados para COM com `[ComVisible(false)]`.
- Não remova construtores, propriedades públicas ou nomes de classes sem considerar quebra de compatibilidade NuGet/COM.
- Evite trocar dependências centrais sem necessidade. A biblioteca usa `Newtonsoft.Json`, `System.Xml`, `XmlDocument`, `XmlSerializer`, `HttpClient` e pacotes Unimake auxiliares.

## Padrões de XML

- Classes serializáveis devem herdar de `XMLBase` quando representarem XML de DFe.
- Use atributos de serialização XML explicitamente: `[XmlRoot]`, `[XmlElement]`, `[XmlAttribute]`, `[XmlIgnore]`, `[XmlText]`, conforme o schema oficial.
- Preserve nomes, ordem lógica e tipos conforme o XSD/manual fiscal. Não traduza nomes de tags.
- Quando uma tag usa enum internamente mas serializa como número/string, siga o padrão de propriedade auxiliar com `XmlIgnore` na propriedade principal e propriedade `...Field` para serialização.
- Para serializar/desserializar, use os utilitários existentes: `GerarXML()`, `LerXML<T>()`, `LoadFromFile(...)` e `XMLUtility.Deserializar<T>(...)`.
- Não monte XML fiscal por concatenação de strings. Use objetos, `XmlDocument`, `XDocument` ou utilitários existentes.
- Preserve namespaces oficiais em `[XmlRoot(..., Namespace = "...")]` e na lista de namespaces de `XMLBase`.

## Padrões de serviços

- Serviços devem herdar da `ServicoBase` específica do DFe quando existir, por exemplo `Servicos.NFe.ServicoBase`, e da base comum somente quando o DFe ainda não tiver base própria.
- O fluxo padrão é: receber XML/objeto e `Configuracao`, chamar `Inicializar(...)`, definir configurações, assinar quando necessário, validar schema/conteúdo e executar transporte.
- Em `DefinirConfiguracao()`, preencha `Configuracoes.Servico`, `CodigoUF`, `TipoAmbiente`, `SchemaVersao`, `TipoDFe` e demais campos a partir do XML quando o serviço exigir.
- Não duplique lógica de transporte SOAP/API. Reuse `ConsumirServico`, `Builders`, `Parsers`, `Transport` e mappers de compatibilidade existentes.
- Para APIs municipais ou provedores NFSe, prefira estender builders/parsers/configurações existentes antes de criar caminhos paralelos.
- Retornos tipados devem expor propriedade `Result` desserializando `RetornoWSXML` com `XMLUtility.Deserializar<T>()` e retornando objeto com erro amigável quando não houver retorno.
- Exceções de serviço devem seguir o padrão existente com `ThrowHelper.Instance.Throw(...)` e exceções específicas (`ValidarXMLException`, `CertificadoDigitalException`, `ValidatorDFeException`) quando aplicável.

## Configurações, schemas e recursos embutidos

- Ao adicionar novo serviço, versão de schema, município ou provedor:
  - inclua/atualize o XML em `Servicos/Config/<DFe>`;
  - inclua o XSD em `Xml/Schemas/<DFe>`;
  - ajuste o `.csproj` para embutir recursos necessários como `EmbeddedResource`;
  - confirme que `Configuracao.Load(...)` consegue localizar serviço, versão, namespace e schema.
- Não altere nomes de recursos embutidos sem revisar chamadas que montam o caminho por namespace.
- Mantenha compatibilidade com produção e homologação, SOAP e API, certificado digital, assinatura, GZip e autenticação municipal conforme o padrão já existente.

## Validações

- Validação de schema deve continuar em `ValidarSchema`.
- Validações manuais de conteúdo devem ficar em `Validator/<DFe>` herdando de `XmlValidatorBase` e registrando regras com `ValidateTag(...)`.
- Mensagens de validação devem ser claras para o desenvolvedor e citar tag, grupo e valor informado quando possível.
- Use `Warnings` apenas para avisos que não interrompem o fluxo; erros impeditivos devem lançar `ValidatorDFeException` ou exceção específica.

## Testes obrigatórios para novas implementações

- Para novo XML/classe de serialização, adicione ou atualize teste de serialização/desserialização em `source/Unimake.DFe.Test/<DFe>`.
- Para novo serviço, adicione teste no diretório do DFe correspondente, seguindo nomes como `StatusServicoTest`, `ConsultaProtocoloTest`, `AutorizacaoTest`, `RecepcaoEventoTest`.
- Para novo schema ou provedor, adicione XML realista em `Resources` e valide geração, leitura e comparação de `InnerText`.
- Marque testes com `[Trait("DFe", "<DFe>")]`.
- Use caminhos relativos no padrão existente, como `@"..\..\..\NFe\Resources\arquivo.xml"`.
- Para bug fixes, prefira criar teste em `BugFixes` ou no DFe afetado, com recurso XML mínimo que reproduza o problema.
- Ao implementar algo novo ou adaptar comportamento existente, execute somente os testes novos ou alterados. Não rode toda a suíte por padrão, pois ela é grande e demorada.
- Se precisar validar regressão de um DFe específico, filtre pelos testes do DFe ou pela classe/método afetado. Rode todos os testes apenas quando a mudança atingir infraestrutura compartilhada, serialização base, assinatura, transporte, validação global ou quando isso for solicitado explicitamente.

## Estilo de código

- Siga o estilo existente: namespaces em bloco, chaves em nova linha, `var` quando o tipo for óbvio, regiões apenas onde o arquivo já usa esse padrão.
- Escreva comentários XML (`/// <summary>`) em APIs públicas; o build trata `CS1591` como erro no projeto principal.
- Mantenha mensagens e documentação em português quando o código existente estiver em português.
- Não faça refatorações amplas junto com uma correção funcional. Alterações devem ser pequenas, rastreáveis e alinhadas ao DFe afetado.
- Evite alterar arquivos gerados, pacotes em `source/Unimake.DFe/packages`, binários, `Compilacao`, `bin` ou `obj`.

## Checklist antes de concluir uma mudança

- A alteração preserva `netstandard2.0` e C# 7.3 no projeto principal.
- Classes públicas continuam documentadas e compatíveis com INTEROP quando necessário.
- XML gerado respeita nomes, namespaces, atributos e ordem esperada pelo schema.
- Configuração, schema e recursos embutidos foram atualizados juntos.
- Testes xUnit foram adicionados ou ajustados com recursos XML representativos.
- Build recomendado: `dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore`.
- Teste recomendado: `dotnet test source/Unimake.DFe.Test/Unimake.DFe.Test.csproj --no-restore --filter "<classe ou metodo alterado>"`.
