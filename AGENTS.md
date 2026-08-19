# Instruções para Codex

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

## Resolução centralizada de NFSe

- Use `ValidarEstruturaXML.DefinirVersaoNFSe(...)` como fonte única para identificar a versão de NFSe e `ValidarEstruturaXML.DefinirTipoServicoNFSe(...)` como fonte única para identificar o serviço; não replique switches por padrão ou município nos consumidores.
- Em `ValidarConfig.xml`, exceções de `TipoServico` podem usar `codMunicipio`, `tipoAmbiente` ou ambos. A precedência obrigatória é: município e ambiente, somente município, somente ambiente e, por fim, o valor padrão de `TipoServico`.
- Os overloads sem `TipoAmbiente` preservam produção como padrão por compatibilidade. Quem conhece o ambiente deve chamar o overload explícito.
- Ao ativar município, padrão, versão ou serviço NFSe, confira em conjunto `Servicos/Config/Config.xml`, `Xml/Validar/ValidarConfig.xml` e os resolvedores centrais, executando os testes focados de versão e serviço.

## Validações

- Validação de schema deve continuar em `ValidarSchema`.
- Validações manuais de conteúdo devem ficar em `Validator/<DFe>` herdando de `XmlValidatorBase` e registrando regras com `ValidateTag(...)`.
- Mensagens de validação devem ser claras para o desenvolvedor e citar tag, grupo e valor informado quando possível.
- Use `Warnings` apenas para avisos que não interrompem o fluxo; erros impeditivos devem lançar `ValidatorDFeException` ou exceção específica.

## Diagnóstico de disponibilidade dos DFe

- Use a skill `manutencao-disponibilidade-dfe` ao alterar `Utility/Disponibilidade`, a coleta em `Servicos/ServicoBase.cs`, o cache de status ou a classificação de conectividade usada pelo diagnóstico.
- A telemetria deve permanecer passiva e nunca repetir uma operação fiscal real, criar arquivos ou aumentar perceptivelmente o tempo da autorização.
- Nunca enviar XML sintético como teste de disponibilidade. A única sonda fiscal explícita permitida é `StatusServico`, protegida por cache e bloqueio de consumo indevido.
- Em XML com vários `cStat`, usar o primeiro em ordem documental. O retorno principal do serviço/lote prevalece sobre códigos posteriores em `protNFe`, `protCTe` e `protMDFe`.
- Quando não houver `cStat` principal, localizar o primeiro em estruturas internas como `infInut`, `infCons` e `infEvento`, sempre conferindo o XSD do serviço e da versão.
- Tratar `108`, `109` e `999` como indisponibilidade fiscal; `656` e `678` como consumo indevido; qualquer outro `cStat > 0` como prova de processamento pela aplicação fiscal.
- Não atribuir DNS, conexão, TLS, proxy, certificado, configuração ou timeout isolado à SEFAZ.
- Nunca armazenar XML fiscal, certificado, senha, token, credencial ou mensagem não sanitizada no diagnóstico.
- Validar alterações com testes determinísticos sem internet, incluindo precedência entre status principal e protocolo, desempenho do caminho fiscal, build principal e testes do UniNFe em `Debug`.

## Testes obrigatórios para novas implementações

- Para novo XML/classe de serialização, adicione ou atualize teste de serialização/desserialização em `source/Unimake.DFe.Test/<DFe>/Serializacao`.
- Para novo serviço, adicione teste em `source/Unimake.DFe.Test/<DFe>/Servicos`, seguindo nomes como `StatusServicoTest`, `ConsultaProtocoloTest`, `AutorizacaoTest`, `RecepcaoEventoTest`.
- Para novo schema ou provedor, adicione XML realista em `Resources` e valide geração, leitura e comparação de `InnerText`.
- Marque testes com `[Trait("DFe", "<DFe>")]`.
- Use caminhos relativos no padrão existente, como `@"..\..\..\NFe\Resources\arquivo.xml"`.
- Para bug fixes, prefira criar teste em `BugFixes` ou no DFe afetado, com recurso XML mínimo que reproduza o problema.
- Ao implementar algo novo ou adaptar comportamento existente, execute somente os testes novos ou alterados. Não rode toda a suíte por padrão, pois ela é grande e demorada.
- Se precisar validar regressão de um DFe específico, filtre pelos testes do DFe ou pela classe/método afetado. Rode todos os testes apenas quando a mudança atingir infraestrutura compartilhada, serialização base, assinatura, transporte, validação global ou quando isso for solicitado explicitamente.
- Sempre que executar testes unitários da DLL, execute também os testes unitários do projeto `C:\projetos\github\UniNFe\source\UniNFe.Test\UniNFe.Test.csproj` em `Debug`. Nessa configuração, os projetos do UniNFe usam `ProjectReference` para este checkout e validam a DLL recém-alterada; aplique no UniNFe um filtro correspondente ao escopo testado na DLL quando houver uma suíte focada equivalente.

## Massas TXT de regressão e comparação antes/depois

- Sempre que o usuário fornecer um TXT real para reproduzir conversão de NFe/NFCe ou comparar o conversor legado do UniNFe com a DLL, anonimize a massa antes de copiá-la para qualquer repositório ou incluí-la em testes.
- Aplique a anonimização tanto em `source/Unimake.DFe.Test/.../Resources/Txt` quanto na massa equivalente do repositório `C:\projetos\github\UniNFe`, mantendo as duas cópias sincronizadas.
- Remova ou substitua nome ou razão social identificável, CPF, CNPJ de pessoa ou empresário individual quando não for indispensável ao cenário, inscrição estadual, e-mail, telefone, endereço, CEP, nome de vendedor, número de pedido e dados repetidos em campos livres, como `Z`, `Z04`, `ZD` e observações.
- Use valores claramente sintéticos, como `CLIENTE TESTE`, `EMPRESA TESTE`, `RUA EXEMPLO` e domínios `example.com`. Preserve somente os campos fiscais indispensáveis ao cenário; se um identificador compuser a chave de acesso ou afetar o dígito verificador, mantenha a consistência ou recalcule a chave e atualize as expectativas.
- Antes de concluir, faça uma varredura em todos os TXT adicionados ou alterados e nas demais massas usadas pela mesma suíte, procurando dados esquecidos, especialmente nos segmentos `E`, `E02`, `E03`, `E05`, `F`, `G`, `G02a`, `X03`, `X04`, `Z`, `Z04` e `ZD`.
- Para comparações “antes × depois”, execute o mesmo TXT anonimizado no conversor legado do UniNFe e na DLL, compare os XMLs e acrescente uma asserção explícita para o comportamento corrigido. Atualize hashes somente após revisar o diff da massa anonimizada.
- Adicione ou mantenha um teste preventivo que falhe se dados identificáveis conhecidos reaparecerem nas massas TXT.

## Organização dos testes unitários

- Mantenha os testes em `source/Unimake.DFe.Test` separados primeiro pelo documento fiscal eletrônico ou integração: `BPe`, `CCG`, `CIOT`, `CTe`, `CTeOS`, `CTeSimp`, `DARE`, `DCe`, `EFDReinf`, `ESocial`, `GNRE`, `MDFe`, `NF3e`, `NFCe`, `NFCom`, `NFe`, `NFGas`, `NFSe`, `SNCM`, `EBoleto`, `PIX`, `UMessenger`.
- Dentro de cada grupo, use subpastas por responsabilidade:
  - `Serializacao` para serialização, desserialização, round-trip e leitura/gravação de XML.
  - `Servicos` para consumo de serviços, status, autorização, consulta, recepção de eventos, inutilização, distribuição e bases auxiliares usadas só por testes de serviço.
  - `Validacao` para testes de validators, schema, regras manuais e validações específicas do DFe.
  - `BugFixes` para regressões de bugs, com recursos relacionados em `Resources\BugFixes` quando forem do mesmo DFe.
  - `Utilitarios` para helpers ou testes auxiliares que pertencem a um DFe específico.
  - `Parsing` para parsers de retorno/API, especialmente em integrações como `EBoleto`, `PIX` e `UMessenger`.
- Não deixe arquivos `.cs` diretamente na raiz de um grupo de DFe/integração; a exceção é `Infraestrutura`, que concentra suporte global do projeto de testes (`PropConfig`, `AssemblyInfo`, `GlobalUsings`).
- O namespace deve acompanhar o caminho físico. Exemplo: `source/Unimake.DFe.Test/NFe/Servicos/StatusServicoTest.cs` deve usar `namespace Unimake.DFe.Test.NFe.Servicos`.
- O valor de `[Trait("DFe", "...")]` deve ser igual ao primeiro diretório do teste. Exemplo: testes em `DARE\...` usam `[Trait("DFe", "DARE")]`, nunca o trait de outro DFe.
- Testes utilitários que não pertencem a um DFe ficam em `Utility`, usando subpastas como `Cache`, `Certificados`, `Chaves`, `Conversao`, `Rede`, `Xml` e `Validacao`, com `[Trait("Utility", "...")]` quando aplicável.
- Recursos XML devem ficar no grupo do DFe em `Resources`, ou em `Utility\Validacao\XMLteste` quando forem massa de validação compartilhada. Ao mover recursos, atualize também `Unimake.DFe.Test.csproj` (`EmbeddedResource`, `None Remove`, `None Update`) e qualquer caminho usado pelos testes.
- Bases de teste compartilhadas por um único grupo devem ficar perto dos consumidores. Exemplo: `PIXTestBase`, `EBoletoTestBase` e `UMessengerTestBase` ficam em `Servicos` porque são usadas pelos testes de serviço.
- Antes de concluir reorganizações de teste, faça uma varredura por arquivos soltos, namespaces antigos, `Trait("DFe", ...)` divergente do diretório e caminhos antigos no `.csproj`.
- Para GNRE, evite rodar o filtro inteiro `DFe=GNRE` por padrão; rode apenas um método representativo, pois alguns testes se multiplicam bastante.
- Para NFSe, evite rodar o filtro inteiro `DFe=NFSe` por padrão; prefira filtros por `FullyQualifiedName` de poucos métodos ou classes, pois o grupo pode gerar milhares de casos.

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
- Para xUnit v3 neste projeto, compile `Unimake.DFe.Test.csproj` e execute a DLL gerada diretamente com `dotnet "source/Unimake.DFe.Test/bin/Debug/net8.0/Unimake.DFe.Test.dll" -class "<namespace.classe>"`; não use o alvo VSTest legado quando o SDK o rejeitar.
