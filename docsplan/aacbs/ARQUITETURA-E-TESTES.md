# AACBS — arquitetura e estratégia de testes

Estado: arquitetura da AACBS-000 e contratos da [especificação da AACBS-001](contratos/ESPECIFICACAO.md) aprovados. A implementação XML/XSD da AACBS-002 foi entregue para revisão; seus resultados estão no [dossiê da etapa](evidencias/AACBS-002/tentativa-0001/ENTREGA.md). Este guia distingue os testes já executados nessa entrega dos fluxos HTTP/reais ainda planejados. Nenhum token foi obtido.

Revisão documental: **24/11/2026**. Atualiza o inventário publicado em 13/11/2026, a preparação dos testes reais incrementais e o runner `net10.0`. As fontes e divergências detalhadas ficam em [FONTES-E-REQUISITOS.md](FONTES-E-REQUISITOS.md) e [MATRIZ-APIS.md](MATRIZ-APIS.md).

## 1. Decisões de escopo e contrato

- Sigla proposta: **AACBS — Apuração Assistida da CBS**. Diretórios e namespaces de código usam `AACBS`; os documentos do plano ficam em `docsplan/aacbs`.
- O escopo atual é a CBS documentada pela Receita Federal. IBS permanece para uma evolução com documentação própria; não presumir que utilize os mesmos endpoints ou contratos.
- O desenvolvedor fornece XML, ou usa as classes públicas para gerar esse XML, e recebe XML e `Result` tipado. A DLL traduz o contrato necessário para HTTP/API internamente.
- Haverá um serviço público por endpoint/operação documentada. Serviços individualizados de solicitação, consulta da situação e obtenção do resultado permitem que a aplicação controle tickets, espera, retomada e persistência.
- O inventário atual contém **oito endpoints de negócio**, mais `Autenticar` e `BaixarArquivoRetorno`, totalizando **dez serviços planejados**: quatro aberturas de consulta assíncrona, situação, link de download e dois geradores síncronos de DARF.
- `GerarDarfAdquirente` consome `POST /rad/{cnpj14}` com entrada JSON em array de um ou vários itens; `GerarDarfContribuinte` consome `POST /pcont/{cnpj14}` com entrada JSON objeto. Ambos entregam a resposta da chamada síncrona em XML/`Result`, sem passar artificialmente pelo fluxo de tíquete e download das consultas.
- Uma execução do serviço não inicia um fluxo de polling, não espera a conclusão remota e não baixa resultados de forma oculta. A aplicação escolhe a próxima operação a executar.
- O domínio terá uma base própria, sem seleção de provedores: o conjunto em escopo é da Receita Federal.
- Dados e códigos retornados pela API devem conservar seu significado e sua representação contratual no XML. Nenhuma regra de sucesso, namespace, código ou normalização de CIOT é transferida para a AACBS.
- A etapa AACBS-001 especificará o contrato API/XML por campo, as raízes, o namespace XML Unimake, a versão do leiaute local e a correspondência entre respostas e serviços, antes da criação de modelos e XSDs.

## 2. O que foi comprovado no CIOT

Os caminhos abaixo são relativos à raiz do repositório. A biblioteca está em `source/.NET Standard/Unimake.Business.DFe`; os testes estão em `source/Unimake.DFe.Test`.

| Papel | Referência comprovada | Uso na AACBS |
|---|---|---|
| Serviço público por operação | `Servicos/CIOT/ConsultarCIOTGerado.cs`, `GravarMotorista.cs` | Reproduzir `Envio`, `Result`, construtores por objeto e string XML, construtor vazio e entrada INTEROP. |
| Inicialização, envio e resultado tipado | `Servicos/CIOT/ServicoBase.cs`, métodos `InicializarServico`, `ObterEnvio` e `ObterResult` | Criar equivalentes próprios, pequenos e comuns às operações AACBS. |
| Configuração fiscal | `Servicos/CIOT/ServicoBase.cs`, `DefinirConfiguracao` | Definir `TipoDFe`, `Servico`, configuração nacional, ambiente e versão de schema do domínio novo. |
| XML para payload API | `Servicos/CIOT/Provedores/ANTT/ProvedorANTT.cs`, `CriarHttpContent`; `Provedores/EFrete/EFreteMapper.cs`, `CriarJson` | Usar como exemplos de separação de conversão. Implementar mapeamento AACBS conforme o contrato RFB, sem herdar regras CIOT. |
| Retorno API para XML | `Provedores/EFrete/EFreteMapper.cs`, `NormalizarRetorno`; `Servicos/CIOT/ServicoBase.cs`, `CriarXMLRetornoTipado` | Produzir o retorno XML do serviço AACBS e conferir os dados até `Result`. |
| Validação de envio | `Servicos/CIOT/ServicoBase.cs`, `XmlValidar` | Usar `ValidarSchema` e validação de conteúdo do domínio antes do request de negócio. |
| Validação de retorno | `Servicos/CIOT/ServicoBase.cs`, `ValidarSchemaRetornoDoProvedor` | Validar todos os contratos XML de retorno AACBS previstos, incluindo respostas de erro modeladas. O CIOT não prova que já existe cobertura integral de XSD de retorno. |
| Autenticação auxiliar | `Provedores/EFrete/EFreteAuthenticator.cs` | Referência de uso do transporte comum e de propagação de proxy; a autenticação RFB será especificada com a documentação oficial. |
| Configurações embutidas | `Servicos/Config/CIOT/Config.xml`, `AN.xml`, `EF.xml` | Criar configuração própria da AACBS, sem `ProvedorCIOT`, fábrica de provedores ou arquivo por provedor. |
| Gravação de XML | `Servicos/CIOT/ServicoBase.cs`, `GravarXmlDistribuicao` | Disponibilizar gravação pelo padrão da biblioteca, usando o XML final do serviço. |
| Modelos e COM | `Xml/CIOT/CadastrosEFrete.cs`, `ConsultarFrotaTransportador.cs` | Seguir atributos XML, documentação pública e métodos COM para listas; conservar tipos e opcionais conforme o contrato novo. |
| Teste do fluxo sem rede | `CIOT/Servicos/EFreteFluxoExecucaoTest.cs` | Exercitar `Executar()` com executor controlado, validar requisições, XML e `Result`. |
| Teste HTTP local | `CIOT/Servicos/EFreteTransporteHttpTest.cs` | Usar quando necessário para comprovar comportamento do transporte real que o executor substituído não observa. |
| Testes de modelos/XSD | `CIOT/Serializacao/EFreteCadastrosTest.cs`, `CIOT/Validacao/EFreteSchemaTest.cs` | Round-trip, comparação estrutural, exemplos positivos e negativos, catálogo e recursos embutidos. |

## 3. Componentes centrais e dependências

| Componente | Comportamento verificado | Trabalho esperado |
|---|---|---|
| `Servicos/Enums/Enums.cs` | Contém os enums públicos `TipoDFe` e `Servico`, com valores numéricos explícitos. | Acrescentar o domínio e as operações, sem renumerar valores existentes. Auditar impacto INTEROP. |
| `Servicos/Configuracao.cs` | O namespace do recurso deriva de `TipoDFe.ToString()`. Há `Load(nomeTagServico)` e `Load(nomeTagServico, arquivoConfiguracao)`. | Carregar os recursos AACBS pelo mecanismo existente. Parâmetros e cabeçalhos específicos só serão adicionados quando justificados pelo contrato. |
| `Servicos/Config/Config.xml` | Catálogo geral de configurações, inclusive configuração nacional. | Conferir a resolução em conjunto com os novos arquivos. Uma alteração no catálogo geral não é presumida: o arquivo nacional existente ou o overload de arquivo direto podem ser suficientes. |
| `Xml/XMLBase.cs` | O construtor obtém o namespace do atributo `[XmlRoot]` por reflexão e o inclui nos namespaces de serialização. | Herdar a base e fornecer atributos corretos. Não alterar a base somente para cadastrar o domínio. |
| `Xml/Validar/ValidarConfig.xml` | Catálogo de raiz, versão, schema e namespace. | Registrar todos os XMLs previstos de envio e retorno, com versão local explícita. |
| `Xml/Validar/ValidarEstruturaXML.cs` | Detecta o domínio pela raiz, obtém a versão e chama o catálogo/validação. | Acrescentar as raízes necessárias e testar identificação, versão e serviço, inclusive conflitos de raiz e namespace. |
| `Xml/Validar/ResolvedorServicoValidacao.cs` | Resolve o catálogo por domínio/raiz/versão; a filtragem de provedores é uma exceção do CIOT. | Usar a resolução comum. Não criar filtragem de provedores AACBS. |
| `Xml/Validar/RegistroNormalizacaoXML.cs` | Mantém as raízes que exigem round-trip tipado antes do envio. | Avaliar a necessidade; registrar apenas se o contrato exigir essa normalização, sem incluir transformações fiscais indevidas. |
| `Xml/Validar/ValidarSchema.cs` | Extrai XSDs embutidos e resolve `include`/`import` recursivamente. | Embutir os XSDs e comprovar resolução dos tipos compartilhados. |
| `Validator/ValidatorFactory.cs` | Descobre validadores públicos que implementam o contrato da biblioteca. | Acrescentar validadores AACBS em `Validator/AACBS`, com regras por `ValidateTag(...)` e mensagens úteis. |
| `Servicos/ServicoBase.cs` | Inicializa, valida e executa a API por `ConfiguracaoApiConfigMapper` e `ConsumirAPI`; expõe XML, texto, corpo bruto e status HTTP. | Herdar o fluxo existente e especializar somente o que o contrato AACBS exigir. |
| `ConsumirServico/Compatibility/ConfiguracaoApiConfigMapper.cs` | Propaga URI, método, headers, certificado, conteúdo e política explícita de proxy. | Reutilizar também nas chamadas auxiliares aplicáveis; testar que os parâmetros chegam ao request final. |
| `ConsumirServico/Compatibility/ApiConfigTransportRequestMapper.cs` | Mapeia `APIConfig` e certificado para o contrato de transporte. | Preservar a separação entre contrato de domínio e transporte. |
| `ConsumirServico/Transport/ApiTransportExecutor.cs` | Executa HTTP e configura handler, certificado, timeout e proxy. | Reusar. Alterações compartilhadas só com necessidade demonstrada e regressões determinísticas. |
| `ConsumirServico/Transport/ApiTransportExecutorFactory.cs` | Possui `Override(...)` com escopo por `AsyncLocal`. | Usar nos testes de serviços para capturar requisições e fornecer respostas sem rede externa. |
| `Unimake.Business.DFe.csproj` | Controla os recursos embutidos; biblioteca usa `netstandard2.0` e C# 7.3. | Incluir configurações e XSDs. Manter `CS1591`, compatibilidade de linguagem e INTEROP. |
| `source/Unimake.DFe.Test/Unimake.DFe.Test.csproj` | O projeto de testes agora mira `net10.0` e mantém xUnit v3. | Exigir SDK/runtime compatíveis e executar a DLL em `bin/Debug/net10.0`; não migrar o target da biblioteca principal. |

O catálogo e os resolvedores centrais serão alterados de forma restrita. O novo domínio não justifica uma refatoração geral da validação ou do transporte.

## 4. Desenho proposto

```text
Aplicação / desenvolvedor
  XML de solicitação, situação ou resultado
       |
       v
Classes Xml.AACBS + validação XSD/conteúdo
       |
       v
Serviços individualizados Servicos.AACBS
       |
       v
Mapeamento AACBS: path / query / headers / body
       |
       v
Configuracao -> APIConfig -> transporte HTTP existente
       |
       v
Resposta HTTP/API -> interpretação AACBS -> XML tipado validado
       |
       v
RetornoWSXML / RetornoWSString / Result / gravação XML
```

Estrutura prevista, sujeita ao detalhamento da AACBS-001:

```text
source/.NET Standard/Unimake.Business.DFe/
  Xml/AACBS/                  modelos públicos de envio e retorno
  Xml/Schemas/AACBS/          XSDs do contrato XML Unimake
  Servicos/AACBS/             base, serviços e conversores específicos
  Servicos/Config/AACBS/      configuração e endpoints embutidos
  Validator/AACBS/            regras de conteúdo não expressas por XSD
source/Unimake.DFe.Test/AACBS/
  Serializacao/
  Validacao/
  Servicos/
  Parsing/                   quando houver conversores com testes próprios
  Resources/                 massas sintéticas de XML e respostas API
```

Cada serviço deve expor XML/objeto de envio e retorno tipado coerente com o endpoint. As entradas necessárias à API continuam presentes no XML público, incluindo parâmetros que a DLL transportará em path, query ou header. Credenciais de autenticação, certificado e segredos ficam na configuração apropriada; não são adicionados ao XML fiscal por conveniência de implementação.

A consulta de situação e o download não dependem de memória de uma solicitação anterior na mesma instância da DLL. O contrato XML contém os identificadores e o contexto necessários à operação; assim a aplicação consegue persistir o ticket e retomar após reiniciar.

Os dois serviços de DARF têm contratos próprios de envio e retorno síncronos. A conversão deve respeitar a diferença entre array RAD de cardinalidade 1:N e objeto PCONT, inclusive para RAD com apenas um item. O CNPJ completo de 14 posições dessas rotas não pode ser substituído pelo CNPJ base utilizado nas aberturas de consulta.

Para arquivos de resultado com formatos diferentes, a **família de consulta é contexto explícito**: débitos, créditos, pagamentos ou recolhimentos. A interpretação não deve depender apenas de um identificador opaco, de tentativa sequencial de desserialização ou de nomes de campos parcialmente coincidentes. A AACBS-001 decidirá se esse contexto aparece como campo discriminador obrigatório ou por tipos/serviços específicos compatíveis com o inventário oficial.

## 5. Contrato XML e preservação dos dados

Os XSDs serão da representação XML fornecida pela Unimake para a API. Não serão apresentados como schemas XML oficiais da Receita quando a fonte oficial definir HTTP/JSON ou outro formato.

O conjunto atual publica JSON Schemas oficiais **Draft 2020-12** para as quatro consultas e os dois geradores de DARF. As páginas das consultas estão na revisão documental 1.3 de 13/11/2026; as de DARF, na 1.0 da mesma data. Esses schemas qualificam a origem JSON, mas não substituem os XSDs do XML público da DLL. Existem contradições entre HTML e JSON Schema; a AACBS-001 registrará a decisão por campo, com fonte, exemplos e restrições aceitas pelo DEV, antes de gerar modelos ou impor enums e cardinalidades.

A qualificação inclui arquivos e referências dos schemas, versões e hashes, além das diferenças frente ao levantamento anterior. Não adicionar uma dependência de validação Draft 2020-12 à biblioteca principal apenas por haver schema oficial; definir o método de verificação das fixtures e a necessidade de qualquer dependência na AACBS-001, preservando `netstandard2.0` e C# 7.3.

A AACBS-001 deve produzir uma matriz por campo contendo: operação, caminho na API, nome/tag XML, tipo, cardinalidade, obrigatoriedade, limites, enumeração, formato, tratamento de ausência/null, regra de conversão e fonte. Campos cuja definição oficial estiver incompleta ficam identificados como pendência, com método de qualificação; não se inventa uma restrição fiscal.

Regras a observar na implementação:

- Identificadores, CNPJ, chaves, tickets e códigos conservam zeros e conteúdo textual contratual; não converter automaticamente em números.
- Valores financeiros devem ter precisão compatível com a API; não adotar `double` por copiar um modelo CIOT. A decisão entre `decimal`, campos auxiliares e preservação lexical será documentada na AACBS-001.
- Datas e horários conservam o fuso, a precisão e a semântica oficial. Datas sem hora não se tornam instantes locais por inferência.
- A revisão atual usa `niContribuinte` em pagamentos, em lugar de `ni_contribuinte`; `excedente` singular em créditos, em lugar de `excedentes`; datas de pagamentos/recolhimentos têm formato `date`; `niConsumidor` admite 11 ou 14 posições. A matriz por campo resolve divergências remanescentes entre schema e HTML. Testes de contrato não podem congelar os nomes e formatos superados do levantamento de 12/11.
- Ausência, `null`, zero, `false`, string vazia e lista vazia recebem regras próprias quando forem situações distintas na API. Os testes precisam detectar perda desses significados.
- Arrays conservam itens, ordem relevante e cardinalidade de um ou vários elementos. Não depender da conversão JSON/XML genérica para decidir a forma da coleção pública.
- Códigos e mensagens remotos são preservados. Não substituí-los por `110`, `cStat`, autorização de documento ou mensagens CIOT.
- HTTP de aceitação da solicitação, conclusão do processamento e sucesso do conteúdo são estados distintos; `Result` não pode converter aceitação em conclusão.
- Resposta de negócio, problema HTTP documentado, falha de autenticação e exceção de transporte mantêm contratos distintos. Corpo HTML inesperado ou JSON inválido não se torna um retorno de sucesso.
- A gravação utiliza o XML final exposto pelo serviço, com a mesma estrutura que `Result` lê. O teste compara geração, gravação e leitura.
- A versão do XML local é separada da versão da API e da versão do pacote NuGet. Mudanças da fonte exigem análise de impacto no contrato e nos testes antes de atualizar a implementação.

## 6. Transporte, autenticação e execução

A especificação de autenticação e os pré-requisitos reais pertencem ao levantamento oficial e à AACBS-001. O fato de o CIOT aceitar certificado ou credenciais em determinados fluxos não prova a existência das mesmas alternativas na AACBS.

Requisitos da implementação:

1. Endpoints e ambientes ficam nos XMLs embutidos. A revisão atual não publica ambiente de produção restrita, inclusive no download; remover a antiga exceção documental. Não presumir homologação ou produção restrita a partir de nomes de host nem copiar um ambiente para outro. Ambiente sem endpoint oficial produz erro local explícito, sem fallback para produção.
2. Método HTTP, media type, parâmetros, headers e autenticação seguem a operação oficial. Codificar valores de path/query corretamente e testar caracteres reservados.
3. Reutilizar `ConsumirAPI`, mappers e executor existentes para negócio e chamadas auxiliares. A adaptação do domínio deve ocorrer antes/depois do transporte.
4. `HasProxy=false` significa conexão direta; `ProxyAutoDetect=true` permite proxy do sistema; proxy manual e pares de credenciais obedecem à política central. Nenhuma chamada auxiliar pode reintroduzir proxy implícito.
5. Certificado, token, credencial e qualquer estado de autenticação não podem vazar entre contribuintes, ambientes ou instâncias. Chaves de cache, se forem necessárias, exigem identidade adequada e regras de validade explícitas.
6. Não adicionar retry, reenvio de solicitação, polling ou download automático dentro de `Executar()`. Após uma falha ambígua, a aplicação recebe o resultado/exceção para decidir a retomada usando o contrato documentado.
7. Contar todas as chamadas nos testes: cada execução produz somente o request da operação e as chamadas auxiliares de autenticação comprovadamente necessárias. Não consultar status ou validar disponibilidade pela rede de forma oculta.
8. Avaliar limites de tamanho e uso de memória dos resultados oficiais. O transporte atual lê o corpo como string; isso deve ser qualificado antes de supor que atende a qualquer volume de download.

Há duas particularidades verificadas no caminho comum que precisam ser testadas com os contratos reais, sem alterar genericamente por antecipação:

- O parser JSON comum usa um nó `temp` ao converter JSON para XML. A representação pública AACBS exige conversão própria e tipada para não depender dessa forma intermediária.
- `ConsumirAPI` verifica conteúdo XML com `InnerText` não vazio. Se houver resposta bem-sucedida vazia documentada, o suporte precisa ser implementado e delimitado ao contrato afetado, com regressão dos fluxos existentes.

### Proteção comprovada antes de credenciais reais

A revisão do código em 24/11 identificou dois caminhos que o plano precisa tratar antes da primeira autenticação real:

- `Servicos/ServicoBase.cs`, na inicialização, chama `Trace.WriteLine(ConteudoXML?.InnerXml, "Unimake.DFe")`. O XML público de download pode conter a URL assinada; registrar esse XML sem tratamento divulga o segredo temporário.
- `ConsumirServico/ConsumirAPI.cs`, ao falhar no tratamento do retorno, usa `throw new Exception(responseContent)`. Uma resposta de autenticação ou um conteúdo incompatível pode chegar integralmente à mensagem da exceção.

Na AACBS-003, implementar a proteção delimitada necessária no caminho efetivamente usado pela AACBS e comprovar seu comportamento **com dados sintéticos antes de carregar credenciais reais**. O XML/`Result` legítimo continua disponível ao consumidor; a proteção incide em trace, mensagens de erro, logs e evidências, sem eliminar informação contratual do retorno público por conveniência.

Os testes devem instalar um `TraceListener` controlado e usar sentinelas distintas para `clientSecret`, token, URL assinada com query e conteúdo fiscal. Exercitar construção do serviço, sucesso e falhas de parsing/autenticação/download; verificar mensagens de exceções e suas internas, trace e saída do teste. Nenhuma sentinela pode aparecer nesses canais. A falha deve conservar tipo/categoria, HTTP quando disponível e correlação sanitizada suficiente para diagnóstico. Não apenas evitar a impressão de um segredo no teste enquanto o serviço continua produzindo a mensagem sensível.

### Arquivo local e suporte dos testes reais

O suporte dos testes importa a credencial **não versionada** `.local/aacbs/credenciais.local.clixml`, com ClientSecret cifrado por DPAPI Windows, e lê `.local/aacbs/configuracao.local.json` somente para CNPJ, ambiente e limites, conforme [ACESSO-LOCAL.md](ACESSO-LOCAL.md). O preparador pede ClientId/ClientSecret em prompt; o roteiro recupera o segredo somente em memória, no mesmo usuário/computador, sem imprimi-lo ou passá-lo em argumentos. A DLL recebe `Configuracao` preenchida pelo consumidor: não procura esses arquivos e não ganha polling ou persistência de estado. Não presumir que uma restrição de ACL foi comprovada.

O manual primário [Receita Integra 1.4](https://cgibs.gov.br/upload/arquivos/202609/05164748-08-manual-de-integracao-tecnica-receita-integra-v-1-4.pdf), de 17/07/2026, exige cache e limita a emissão a dois tokens/hora, com validade de uma hora. O suporte de testes deve compartilhar cache cifrado e sincronizado entre processos por identidade/ambiente; reiniciar o runner ou criar outra instância não autoriza novo login. Esse limite não substitui a cota de negócio. O cache da DLL e a persistência operacional do roteiro são camadas distintas.

Antes de criar conteúdo real, confirmar a regra de ignore e que o arquivo não está rastreado. O arquivo não é fixture, `EmbeddedResource`, conteúdo copiado para `bin` ou parte de pacote. O loader tem testes sintéticos para ausência, JSON inválido, campos obrigatórios e ambiente sem suporte. Ele informa o nome do campo inválido sem devolver valor, JSON ou objeto de configuração na mensagem.

Testes reais desabilitados ficam explicitamente ignorados, sem tráfego. Com execução habilitada, configuração inválida ou pré-requisito indisponível deve ser reportado como falha/pendência apropriada, sem declarar que o teste passou. A habilitação local não substitui o gate da etapa nem autoriza criar DARF ou abrir novas consultas fora do cenário definido.

O harness conserva tíquetes, tentativas, cotas e dados mínimos de retomada em estado privado sob `.local/aacbs`, separado das credenciais. Uma nova execução pode autenticar e acompanhar um tíquete existente sem repetir sua abertura. Abertura de consulta e geração de DARF são ações explícitas do cenário; não podem ocorrer em inicializador compartilhado ou rotina de preparação. Timeout de POST sem resposta cria um resultado incerto e não autoriza reenvio automático. URLs assinadas e tokens ficam protegidos e fora das evidências; não transformar seu armazenamento em requisito da DLL.

## 7. Disponibilidade passiva

O transporte existente informa respostas HTTP e participa da observação passiva quando usada pelo consumidor. A AACBS deve conservar retorno e exceção da operação e não gerar requests de diagnóstico adicionais.

Não existe neste plano um serviço `StatusServico` AACBS inventado. Consultar a situação de uma solicitação de processamento não equivale a uma sonda oficial de disponibilidade da API.

O reconhecimento de transporte e de autenticação deve continuar funcionando sem inferir autorização ou processamento completo de um HTTP bem-sucedido. Quando não houver estratégia oficial ou interpretação específica comprovada, preservar a classificação inconclusiva/não aplicável correspondente da infraestrutura, sem excluir a operação da observação passiva.

Se uma etapa precisar alterar reconhecedores, sessões, telemetria ou classificação de disponibilidade, deverá usar a skill `manutencao-disponibilidade-dfe` e ler `.agents/instructions/disponibilidade-dfe.instructions.md`. A inclusão do domínio não autoriza por si só manutenção no PRO nem no UniNFe legado.

## 8. Matriz de testes obrigatórios

| Camada | Casos relevantes | Critério observável |
|---|---|---|
| Serialização XML | Exemplos mínimos/completos, round-trip, namespaces, ordem, opcionais, coleções e atributos | XML equivalente ao contrato aprovado e leitura sem perda dos valores significativos. |
| Tipos e conversões | Valores financeiros nos limites, zeros à esquerda, datas/fusos, enumerações, ausência/null/zero/false | Nenhuma conversão silenciosa que altere o significado dos dados. |
| XSD | Todos os envios e retornos, tipos compartilhados, campo obrigatório ausente, formato/enum/cardinalidade inválidos | Exemplos válidos aceitos e violações representativas rejeitadas pelo `ValidarSchema`. |
| Origem JSON oficial | Fixtures contra os JSON Schemas Draft 2020-12, nomes/formatos atualizados e divergências resolvidas com HTML | Contrato XML rastreável ao JSON qualificado; XSD Unimake e schema JSON oficial cumprem papéis distintos. |
| Validação central | Identificação do domínio, versão, raiz, serviço e namespace, envios e retornos cadastrados | `ValidarConfig.xml` e os resolvedores localizam o recurso correto; XML de outra operação não recebe schema por coincidência. |
| Regras de conteúdo | Dependências entre campos, intervalos e combinações oficialmente documentadas | Mensagem cita tag/grupo/valor relevante; não há request de negócio para entrada inválida. |
| XML para API | Corpo JSON estrutural, path/query/headers, media type, presença/ausência e encoding | Request coincide com a matriz API/XML; metadados locais não vazam para a API. |
| DARFs síncronos | RAD com um e vários itens, array vazio inválido, PCONT objeto, CNPJ completo, respostas/erros oficiais | RAD conserva array 1:N; PCONT conserva objeto; resposta XML/`Result` completa sem tíquete ou polling inventado. |
| API para XML | Sucesso, aceitação, processamento, conclusão, erros documentados, listas vazias/um/vários itens | XML final validado por XSD e `Result` reproduz o contrato remoto correto. |
| Download por família | Mesmos identificadores opacos em contextos distintos, conteúdo incompatível com a família e campos variantes | Seleção usa o contexto explícito aprovado; não interpreta a resposta por tentativa ou semelhança de nomes. |
| Serviço completo | Construção por XML e por objeto, `Executar()` com transporte controlado, retorno e exceções | Captura do request final e asserções sobre `RetornoWSXML`, `RetornoWSString`, status e `Result`. |
| Contagem de requests | Operação única, autenticação quando necessária, erro, timeout e retomada explícita | Quantidade e sequência exatas; nenhum polling, retry, download ou request de disponibilidade oculto. |
| Autenticação | Contratos aceitos, rejeição, expiração e reuso conforme a fonte, certificado requerido/ausente quando aplicável | Credenciais chegam somente ao destino correto; falhas não são atribuídas ao processamento de negócio. |
| Sanitização antes do real | Sentinelas em credencial, token, URL assinada e conteúdo; `TraceListener`, mensagens/inner exceptions e falhas de parsing | Nenhuma sentinela nos canais de diagnóstico; retorno contratual preservado; teste verde antes de ler credenciais reais. |
| Suporte local dos testes | Loader JSON sintético, habilitação desligada, ambiente ausente e retomada com tíquete privado | Sem rede quando desabilitado; configuração inválida não aparenta sucesso; nenhuma leitura automática de arquivo pela DLL. |
| Proxy | Direto, automático e manual; pares de credenciais e endpoints inválidos | Política explícita chega ao request e ao handler de negócio e autenticação. |
| Reutilização | Troca de contribuinte, ambiente, operação e execução após reinício lógico | URI, autenticação, família e resultado anterior não são reaproveitados indevidamente. |
| Gravação | XML final em diretório temporário de teste e posterior leitura | Conteúdo gravado corresponde ao retorno tipado e valida pelo XSD aplicável. |
| INTEROP | API pública, construtores, enums, listas, valores opcionais e métodos COM | Build `INTEROP_Release` e uso compatível com o padrão público existente. |
| Integração real incremental | Autenticação e débitos na 003; outras três consultas e dois DARFs na 004 conforme cenário autorizado e habilitado | Evidência sanitizada por serviço e versão; nenhuma repetição de abertura/geração para recriar evidência; ausência de dados não comprova grupos não exercitados. |

Testes de mapper isolado são necessários, mas não substituem `Executar()` até `Result`. O conjunto deve incluir respostas controladas de erro, corpo inválido e contratos cruzados, preservando as exceções e dados observáveis definidos.

Os testes usarão `[Trait("DFe", "AACBS")]`, namespaces que acompanham as subpastas e recursos sintéticos dentro de `AACBS/Resources`. Dados e segredos reais não serão copiados para massas ou logs versionados. Testes reais ficam separados dos determinísticos, com execução explícita e pré-requisitos externos documentados.

## 9. Distribuição dos testes por etapa

| Etapa | Entrega técnica prevista | Qualidade exigida antes da revisão humana |
|---|---|---|
| AACBS-000 | Plano e investigação | Consistência de escopo, dependências, status e decisões; nenhum build/teste de implementação alegado. |
| AACBS-001 | Qualificação da fonte e especificação por campo, somente documentação | Inventário dos dez serviços, HTML e JSON Schemas oficiais reconciliados, matriz API/XML, variantes, erros, requisitos de acesso e critérios testáveis. |
| AACBS-002 | Modelos XML, XSDs e validadores | Serialização, XSDs positivos/negativos, campos limítrofes, catálogo, recursos e compatibilidade pública. |
| AACBS-003 | Infraestrutura/autenticação e primeiro fluxo completo de débitos | Primeiro testes offline de requests, XML/`Result`, erros, retomada, contagem de chamadas, loader e sanitização; depois autenticação e fluxo real de débitos com acesso habilitado e cenário autorizado. |
| AACBS-004 | Créditos, pagamentos, recolhimentos e dois DARFs; cobertura total | Provar offline os três fluxos, dois serviços síncronos, variantes e contratos cruzados; reais incrementais quando ambiente/cenário estiverem autorizados e habilitados, com pendências reais explícitas se faltarem dados. |
| AACBS-005 | Consolidação real, INTEROP, pacote e entrega | Reutilizar evidências válidas de 003/004, executar apenas validação residual necessária, builds e pacote; exemplos de consumo e limitações, sem repetir aberturas ou geração de DARF só para refazer o dossiê. |

Cada etapa encerra em revisão humana. Os agentes podem trabalhar em tarefas independentes dentro da etapa liberada, mas não iniciam a implementação da etapa sucessora sem aprovação do DEV.

## 10. Comandos e seleção da regressão

Os comandos abaixo são a referência de execução. Os comandos e filtros efetivamente executados na AACBS-002 estão em seu dossiê; os fluxos HTTP e reais permanecem futuros. Confirmar SDK/runtime compatíveis com `net10.0` antes de compilar/executar testes. A biblioteca principal permanece em `netstandard2.0`.

Build principal:

```powershell
dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore
```

Build do projeto de testes:

```powershell
dotnet build "source/Unimake.DFe.Test/Unimake.DFe.Test.csproj" --no-restore
```

Execução pelo runner xUnit v3, substituindo o nome pela classe criada na etapa:

```powershell
dotnet "source/Unimake.DFe.Test/bin/Debug/net10.0/Unimake.DFe.Test.dll" -class "Unimake.DFe.Test.AACBS.Servicos.NomeDaClasseTest"
```

Build INTEROP da biblioteca:

```powershell
dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore -c INTEROP_Release
```

Se for necessária restauração, executar o fluxo normal do projeto e registrar o motivo; não esconder falha de dependência nem substituir o runner xUnit v3 pelo alvo VSTest legado rejeitado pelo SDK.

A seleção da regressão segue o impacto:

- Alteração restrita a AACBS: executar testes novos/alterados e as classes focadas dos componentes centrais efetivamente tocados.
- Cadastro no catálogo ou detecção central: incluir regressões determinísticas representativas do resolvedor e dos domínios próximos, sem varrer NFSe/GNRE indiscriminadamente.
- Mudança em transporte/parser/serialização base: ampliar os testes offline desses componentes e dos fluxos existentes afetados, incluindo CIOT quando seu caminho for tocado.
- Suíte integral: somente quando o impacto justificar e houver ambiente preparado para certificados e serviços externos; falhas ambientais devem ser identificadas, sem afrouxar validação.
- Correção após falha: repetir o teste que falhou e os testes diretamente relacionados. Não repetir conjuntos amplos sem nova alteração ou preocupação concreta.

O fluxo oficial de empacotamento é `source/packDFeOffline.bat` e `source/Packaging/Pack-DFe.ps1`; a validação inicial usa `-DryRun` conforme seus parâmetros vigentes. Não criar empacotamento paralelo, publicar sem autorização ou modificar versões de consumidores nesta etapa.

## 11. Evidências suficientes e limites de integração

Para cada entrega técnica, registrar em sua evidência: arquivos relevantes, contrato coberto, comando e resultado dos testes, quantidade de casos quando disponível, build, achados corrigidos e limitações restantes. Um relatório curto com links para artefatos e logs sanitizados é suficiente; capturas de tela não são prova necessária de testes automatizados.

Separar evidência offline, autenticação real e execução real de cada operação. Registrar somente serviço, ambiente, horário, HTTP/estado, contagem e correlação sanitizada; não anexar arquivo de credenciais, token, URL assinada, XML fiscal real nem exceção bruta. A consolidação na 005 reaproveita resultados anteriores vinculados ao código ainda válido. Se um contrato ou caminho executado mudou, repetir somente a verificação necessária e justificar eventual nova abertura ou geração, respeitando cotas e autorização do cenário.

Uma evidência deve vincular o resultado ao estado do código executado. Em trabalho paralelo, o agente executor identifica a revisão ou o diff testado; se outro agente alterar os arquivos relevantes durante a execução, o resultado não fecha a validação da versão posterior. Builds e testes que compartilham `bin/obj` precisam de um executor coordenado, ou isolamento explícito, para evitar disputa de arquivos e resultados ambíguos.

A integração com UniNFe ou PRO não faz parte do escopo pedido. Não editar outros repositórios nem apresentar testes de um pacote anterior como validação da AACBS. Se uma etapa futura de consumo for autorizada, gerar primeiro pacote offline imutável pelo fluxo oficial, restaurar essa versão exata no consumidor e executar os filtros/builds correspondentes, sem recriar `ProjectReference`.

Este documento registra a arquitetura e a estratégia de testes. A especificação aprovada da AACBS-001 detalha classes e agrupamentos internos, preservando serviços individualizados, contrato XML, contexto explícito de família e aprovação humana por etapa. Somente o dossiê de cada etapa comprova a execução de seus critérios.
