# Instruções para Codex

Este repositório contém a biblioteca `Unimake.DFe`, usada para emissão, consulta, validação, assinatura e consumo de serviços de documentos fiscais eletrônicos brasileiros, como NFe, NFCe, CTe, MDFe, NFSe, GNRE, EFDReinf, eSocial, NFCom, NF3e, DCe, DARE, CCG e SNCM.

## Arquitetura do projeto

- A solução principal está em `source/Unimake.DFe.sln`.
- A biblioteca principal fica em `source/.NET Standard/Unimake.Business.DFe` e compila para `netstandard2.0` com `LangVersion` 7.3.
- O projeto `source/.NET Framework/Unimake.Security.Platform` mantém compatibilidade com `net472`.
- Os testes ficam em `source/Unimake.DFe.Test`, usam xUnit v3 e miram `net8.0`.
- Exemplos em `Exemplos/` atendem várias linguagens e não devem dirigir a arquitetura da biblioteca principal.

## Empacotamento NuGet e integração com o UniNFe

- A automação oficial fica em `source/packDFeOffline.bat`, `source/packDFeNuget.bat` e `source/Packaging/Pack-DFe.ps1`; não monte pacotes manualmente nem crie outro fluxo paralelo.
- O pacote `Unimake.DFe` deve conter somente `lib/netstandard2.0`, com `Unimake.Business.DFe.dll`, PDB, XML e `GeradorCIOTShared.dll`. Não duplique os mesmos binários em `lib/net472`; consumidores .NET Framework 4.7.2 ou superiores usam os assets `netstandard2.0`.
- A DLL `GeradorCIOTShared.dll` acompanha o mesmo pacote e deve ser validada contra `source/.NET Standard/Unimake.Business.DFe/ThirdParty/GeradorCIOTShared/GeradorCIOTShared.dll`. Não crie pacote ou referência externa separada para ela.
- Use `source/packDFeOffline.bat` para gerar versões imutáveis no feed local `C:\projetos\NuGetOffline`. A fonte `Unimake Offline` é configuração do usuário e não deve ser gravada em `NuGet.Config` versionado.
- Use `source/packDFeNuget.bat` somente para uma publicação explicitamente autorizada. A publicação exige branch `main`, árvore limpa, sincronismo com `origin/main` e `NUGET_API_KEY` no ambiente; nunca grave ou exiba chaves em scripts, arquivos ou argumentos de linha de comando.
- Release notes vêm dos commits elegíveis do GitHub no intervalo informado. Preserve a descrição institucional em UTF-8 e as validações de versão, dependências, conteúdo e hash do CIOT.
- O modo `-DryRun` é o caminho padrão para validar o fluxo oficial sem publicar nem persistir o pacote ou versões rastreadas.
- Debug, Beta e Release do UniNFe consomem a DLL exclusivamente pelo pacote NuGet. Nunca instrua a inclusão deste checkout na solução do UniNFe nem recrie `ProjectReference` para a biblioteca.
- Para validar uma alteração da DLL no UniNFe antes da publicação oficial, gere um novo pacote offline, atualize localmente o UniNFe para essa versão exata, execute os builds/testes relevantes e não faça commit da versão no UniNFe enquanto ela não existir no nuget.org.

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

## Proxy e transportes HTTP

- `Configuracao.HasProxy` é a decisão explícita dos consumidores de alto nível. Quando `false`, mappers e transportes criados a partir de `Configuracao` devem forçar conexão direta; `HttpWebRequest` e handlers não podem herdar silenciosamente o proxy do sistema.
- `ProxyAutoDetect = true` usa o proxy do sistema sem exigir `ProxyServer`/`ProxyPort`. No modo manual, valide servidor/URI HTTP(S) sem caminho/query/fragmento e porta entre 1 e 65535.
- Credenciais explícitas de proxy exigem usuário e senha em conjunto. Não use `DefaultCredentials`/`UseDefaultCredentials` como fallback para os fluxos originados de `Configuracao`.
- Aplique a mesma política em SOAP, API com `HttpClientHandler`, WinHTTP, preparação TLS e autenticações auxiliares que criem request próprio, como eFrete, eBoleto, PIX e uMessenger. Ao adicionar transporte ou token/login, procure antes um mapper/executor compartilhado e inclua teste offline.
- Preserve compatibilidade das APIs de baixo nível (`WSSoap`, `APIConfig` e overloads públicos existentes): defaults históricos podem continuar selecionando proxy do sistema, mas os mappers de `Configuracao` devem carregar uma flag explícita até o request final.
- Nunca registre senha/usuário de proxy. A identidade usada no diagnóstico deve incluir modo, endpoint, porta e credenciais somente por derivação irreversível; mudança de endpoint não pode reutilizar cache de outro proxy.

## Manutenção do CIOT

- Use a skill `manutencao-ciot` ao implementar, corrigir ou revisar XML, XSD, serviços, provedores, autenticação, transporte, retornos, INTEROP, testes ou integração UniNFe do CIOT via ANTT/eFrete.
- A ANTT é o baseline: ausência de `<ProvedorCIOT>` continua selecionando ANTT e nenhuma regra exclusiva da eFrete pode alterar seu endpoint, payload, schema ou retorno.
- Diferenças eFrete devem permanecer no provedor e ser normalizadas na DLL. Em especial, sucessos precisam chegar aos consumidores no contrato CIOT existente, incluindo `Codigo=110` e mensagens estruturadas quando aplicável; não transfira essa compatibilização para o UniNFe.

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

- Use a skill [manutencao-disponibilidade-dfe](.codex/skills/manutencao-disponibilidade-dfe/SKILL.md) para diagnóstico, observações, reconhecimento de respostas, autenticações auxiliares, transporte instrumentado, cache ou classificação. O escopo inclui todos os documentos, integrações e utilitários remotos, não somente SEFAZ/cStat.
- Leia o contrato em [.agents/instructions/disponibilidade-dfe.instructions.md](.agents/instructions/disponibilidade-dfe.instructions.md). Ele aponta as APIs, implementações, testes e evidências sem exigir leitura de todo o inventário.
- O consumidor do diagnóstico é o PRO (`C:\projetos\UniNFe.Desktop`). O usuário decidiu retirar a rotina do UniNFe legado; não planejar sua migração nem exigir seus testes para este recurso. Isso não elimina a integração fiscal do legado em outros domínios nem comprova que a remoção já ocorreu.
- A API recomendada é `CanalObservacoesDisponibilidade`/`SessaoObservacaoDisponibilidade`, envolvendo construção/autenticação e execução; concluir com `Concluir(xml, http)` ou `Falhar(ex, http)`. A DLL interpreta respostas; persistência, agregação temporal e apresentação pertencem ao PRO.
- Para verificação manual, usar `VerificarStatusServico()` e capacidades da DLL. Não introduzir listas fixas no consumidor nem usar `HistoricoLegado`/APIs obsoletas como prova de cobertura ou fallback. A supressão do histórico duplicado e a proteção de consumo indevido devem continuar funcionando, inclusive com fila cheia.
- Observação passiva não gera tráfego, arquivos ou repetição de negócio. Sonda fiscal explícita somente `StatusServico`, com cache e bloqueio; sem status oficial ainda há observação passiva. Não usar documentos fictícios nem sondas automáticas no caminho crítico.
- Interpretar cada contrato: `cdResposta`, GNRE, CIOT e NFS-e não herdam a política cStat. Nas famílias que usam cStat, preservar o primeiro em ordem documental, inclusive grupos internos quando não houver principal; 108/109/999 indicam indisponibilidade, 656/678 consumo indevido e outro positivo comprova processamento, não autorização. SOAP Fault/HTTP200 isolado não comprova negócio.
- Vincular respostas sem cStat ao serviço/operação antes de ler o código: enum, tipo de Result ou prefixo de namespace isolados não bastam. Conferir namespace completo e caminho qualificado; não inferir a operação do próprio retorno nem confundir versão de entrada com versão de resposta. Testar contratos cruzados e preservar contratos realmente compartilhados, conforme a instrução específica.
- Separar transporte, autenticação, preparação e aplicação. Não atribuir DNS, conexão, TLS, proxy, certificado ou timeout isolado ao órgão remoto. Desconhecido fica inconclusivo; verificação sem estratégia oficial fica não aplicável, sem excluir a operação da observação passiva.
- Não guardar XML, corpo remoto, certificado/PIN, credenciais, token ou exceção bruta. `Endpoint` expõe somente esquema/host/porta; cache e agrupamento preservam identidade derivada interna, sem depender do caminho público. Verificação de envelope PDF não equivale a validação integral ou assinatura.
- Validação: testes offline dos contratos/executores afetados, preservação do retorno/exceção, ausência de requests extras, sanitização e consumo indevido. Seguir o runner xUnit v3 e os filtros da skill; testar o PRO somente no escopo autorizado e com o pacote novo efetivamente restaurado. Confira versão, ProductVersion e base fiscal: pacote anterior do PRO pode conter patches de diagnóstico sobre uma base diferente do HEAD. Não misturar atualização fiscal independente.

## Testes obrigatórios para novas implementações

- Para novo XML/classe de serialização, adicione ou atualize teste de serialização/desserialização em `source/Unimake.DFe.Test/<DFe>/Serializacao`.
- Para novo serviço, adicione teste em `source/Unimake.DFe.Test/<DFe>/Servicos`, seguindo nomes como `StatusServicoTest`, `ConsultaProtocoloTest`, `AutorizacaoTest`, `RecepcaoEventoTest`.
- Para novo schema ou provedor, adicione XML realista em `Resources` e valide geração, leitura e comparação de `InnerText`.
- Marque testes com `[Trait("DFe", "<DFe>")]`.
- Use caminhos relativos no padrão existente, como `@"..\..\..\NFe\Resources\arquivo.xml"`.
- Para bug fixes, prefira criar teste em `BugFixes` ou no DFe afetado, com recurso XML mínimo que reproduza o problema.
- Ao implementar algo novo ou adaptar comportamento existente, execute somente os testes novos ou alterados. Não rode toda a suíte por padrão, pois ela é grande e demorada.
- Se precisar validar regressão de um DFe específico, filtre pelos testes do DFe ou pela classe/método afetado. Rode todos os testes apenas quando a mudança atingir infraestrutura compartilhada, serialização base, assinatura, transporte, validação global ou quando isso for solicitado explicitamente.
- A suíte integral contém casos que exigem certificado A1 e serviços externos. Para transporte compartilhado, execute-a somente em ambiente preparado; sem esses recursos, não enfraqueça validação/certificado nem trate a falha ambiental como regressão. Registre a limitação e mantenha verdes os testes determinísticos do transporte, o build principal e o `UniNFe.Test` Debug.
- Sempre que executar testes unitários da DLL e a mudança afetar a integração com o UniNFe, gere primeiro um pacote offline desta DLL e atualize localmente `C:\projetos\github\UniNFe` para essa versão. Depois execute os testes do projeto `source/UniNFe.Test/UniNFe.Test.csproj` e os builds relevantes; aplique um filtro correspondente ao escopo testado na DLL quando houver uma suíte focada equivalente. O UniNFe não usa mais `ProjectReference` para este checkout.
- Exceção para diagnóstico de disponibilidade: o consumidor é o PRO, conforme a seção específica acima. Não atualizar/testar o legado para validar esse recurso. Se a tarefa limitar o PRO a consulta, executar os testes offline da DLL e registrar a validação do consumidor como pendente; não editar referências nem apresentar testes do pacote anterior como prova do código novo.

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
- Evite alterar arquivos gerados, pacotes em `source/Unimake.DFe/packages`, binários, `Compilacao`, `bin` ou `obj`. A exceção é a substituição controlada de `source/Unimake.DFe/Unimake.DFe.nupkg` pelo fluxo oficial de publicação, quando explicitamente autorizada.

## Checklist antes de concluir uma mudança

- A alteração preserva `netstandard2.0` e C# 7.3 no projeto principal.
- Classes públicas continuam documentadas e compatíveis com INTEROP quando necessário.
- XML gerado respeita nomes, namespaces, atributos e ordem esperada pelo schema.
- Configuração, schema e recursos embutidos foram atualizados juntos.
- Testes xUnit foram adicionados ou ajustados com recursos XML representativos.
- Build recomendado: `dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore`.
- Quando a mudança afetar empacotamento ou consumo pelo UniNFe, valide `source/Packaging/Pack-DFe.ps1` em `-DryRun` e confirme que o pacote contém apenas os assets `netstandard2.0`, incluindo `GeradorCIOTShared.dll`.
- Para xUnit v3 neste projeto, compile `Unimake.DFe.Test.csproj` e execute a DLL gerada diretamente com `dotnet "source/Unimake.DFe.Test/bin/Debug/net8.0/Unimake.DFe.Test.dll" -class "<namespace.classe>"`; não use o alvo VSTest legado quando o SDK o rejeitar.

## Planejamento NF-e ABI (DevPlanner)

- Para executar NF-e ABI, leia `docsplan/nfabi/CODEX-START-HERE.md` e use somente a etapa explicitamente autorizada.
- Em toda etapa `002+`, releia `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi`, compare hashes e pare se a fonte mudou.
- Somente o DEV aprova etapas. Não inicie a sucessora após entregar.
- Produção permanece sem endpoint; não invente URL nem copie homologação para produção.
