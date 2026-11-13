# AACBS — arquitetura e estratégia de testes

Estado: proposta de arquitetura da etapa AACBS-000; contratos detalhados serão fechados na AACBS-001. Este documento não comprova implementação, compilação, execução de testes nem consumo real.

## 1. Decisões de escopo e contrato

- Sigla proposta: **AACBS — Apuração Assistida da CBS**. Diretórios e namespaces de código usam `AACBS`; os documentos do plano ficam em `docsplan/aacbs`.
- O escopo atual é a CBS documentada pela Receita Federal. IBS permanece para uma evolução com documentação própria; não presumir que utilize os mesmos endpoints ou contratos.
- O desenvolvedor fornece XML, ou usa as classes públicas para gerar esse XML, e recebe XML e `Result` tipado. A DLL traduz o contrato necessário para HTTP/API internamente.
- Haverá um serviço público por endpoint/operação documentada. Serviços individualizados de solicitação, consulta da situação e obtenção do resultado permitem que a aplicação controle tickets, espera, retomada e persistência.
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

Para arquivos de resultado com formatos diferentes, a **família de consulta é contexto explícito**: débitos, créditos, pagamentos ou recolhimentos. A interpretação não deve depender apenas de um identificador opaco, de tentativa sequencial de desserialização ou de nomes de campos parcialmente coincidentes. A AACBS-001 decidirá se esse contexto aparece como campo discriminador obrigatório ou por tipos/serviços específicos compatíveis com o inventário oficial.

## 5. Contrato XML e preservação dos dados

Os XSDs serão da representação XML fornecida pela Unimake para a API. Não serão apresentados como schemas XML oficiais da Receita quando a fonte oficial definir HTTP/JSON ou outro formato.

A AACBS-001 deve produzir uma matriz por campo contendo: operação, caminho na API, nome/tag XML, tipo, cardinalidade, obrigatoriedade, limites, enumeração, formato, tratamento de ausência/null, regra de conversão e fonte. Campos cuja definição oficial estiver incompleta ficam identificados como pendência, com método de qualificação; não se inventa uma restrição fiscal.

Regras a observar na implementação:

- Identificadores, CNPJ, chaves, tickets e códigos conservam zeros e conteúdo textual contratual; não converter automaticamente em números.
- Valores financeiros devem ter precisão compatível com a API; não adotar `double` por copiar um modelo CIOT. A decisão entre `decimal`, campos auxiliares e preservação lexical será documentada na AACBS-001.
- Datas e horários conservam o fuso, a precisão e a semântica oficial. Datas sem hora não se tornam instantes locais por inferência.
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

1. Endpoints e ambientes ficam nos XMLs embutidos. Não presumir homologação ou produção a partir de nomes de host nem copiar um ambiente para outro.
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
| Validação central | Identificação do domínio, versão, raiz, serviço e namespace, envios e retornos cadastrados | `ValidarConfig.xml` e os resolvedores localizam o recurso correto; XML de outra operação não recebe schema por coincidência. |
| Regras de conteúdo | Dependências entre campos, intervalos e combinações oficialmente documentadas | Mensagem cita tag/grupo/valor relevante; não há request de negócio para entrada inválida. |
| XML para API | Corpo JSON estrutural, path/query/headers, media type, presença/ausência e encoding | Request coincide com a matriz API/XML; metadados locais não vazam para a API. |
| API para XML | Sucesso, aceitação, processamento, conclusão, erros documentados, listas vazias/um/vários itens | XML final validado por XSD e `Result` reproduz o contrato remoto correto. |
| Download por família | Mesmos identificadores opacos em contextos distintos, conteúdo incompatível com a família e campos variantes | Seleção usa o contexto explícito aprovado; não interpreta a resposta por tentativa ou semelhança de nomes. |
| Serviço completo | Construção por XML e por objeto, `Executar()` com transporte controlado, retorno e exceções | Captura do request final e asserções sobre `RetornoWSXML`, `RetornoWSString`, status e `Result`. |
| Contagem de requests | Operação única, autenticação quando necessária, erro, timeout e retomada explícita | Quantidade e sequência exatas; nenhum polling, retry, download ou request de disponibilidade oculto. |
| Autenticação | Contratos aceitos, rejeição, expiração e reuso conforme a fonte, certificado requerido/ausente quando aplicável | Credenciais chegam somente ao destino correto; falhas não são atribuídas ao processamento de negócio. |
| Proxy | Direto, automático e manual; pares de credenciais e endpoints inválidos | Política explícita chega ao request e ao handler de negócio e autenticação. |
| Reutilização | Troca de contribuinte, ambiente, operação e execução após reinício lógico | URI, autenticação, família e resultado anterior não são reaproveitados indevidamente. |
| Gravação | XML final em diretório temporário de teste e posterior leitura | Conteúdo gravado corresponde ao retorno tipado e valida pelo XSD aplicável. |
| INTEROP | API pública, construtores, enums, listas, valores opcionais e métodos COM | Build `INTEROP_Release` e uso compatível com o padrão público existente. |
| Integração real | Solicitação, situação e resultado das quatro famílias em ambiente autorizado | Evidência sanitizada do contrato real; ausência de dados no ambiente não comprova mapeamento de campos não exercitados. |

Testes de mapper isolado são necessários, mas não substituem `Executar()` até `Result`. O conjunto deve incluir respostas controladas de erro, corpo inválido e contratos cruzados, preservando as exceções e dados observáveis definidos.

Os testes usarão `[Trait("DFe", "AACBS")]`, namespaces que acompanham as subpastas e recursos sintéticos dentro de `AACBS/Resources`. Dados e segredos reais não serão copiados para massas ou logs versionados. Testes reais ficam separados dos determinísticos, com execução explícita e pré-requisitos externos documentados.

## 9. Distribuição dos testes por etapa

| Etapa | Entrega técnica prevista | Qualidade exigida antes da revisão humana |
|---|---|---|
| AACBS-000 | Plano e investigação | Consistência de escopo, dependências, status e decisões; nenhum build/teste de implementação alegado. |
| AACBS-001 | Qualificação da fonte e especificação por campo, somente documentação | Inventário completo de operações, matriz API/XML, variantes e erros, exemplos de referência, requisitos de acesso e critérios testáveis. |
| AACBS-002 | Modelos XML, XSDs e validadores | Serialização, XSDs positivos/negativos, campos limítrofes, catálogo, recursos e compatibilidade pública. |
| AACBS-003 | Infraestrutura/autenticação e primeiro fluxo completo de débitos | Requests e autenticação, solicitação/situação/resultado individualizados, XML final, `Result`, erros, retomada e contagem de chamadas sem rede externa. |
| AACBS-004 | Créditos, pagamentos e recolhimentos; cobertura total do inventário | Mesma exigência para os três fluxos, variantes e testes cruzados; matriz endpoint × contrato × teste sem lacunas não justificadas. |
| AACBS-005 | Testes reais, INTEROP, pacote e entrega | Evidências reais sanitizadas, builds, validações do fluxo oficial de pacote, exemplos de consumo XML e limitações explicitadas. |

Cada etapa encerra em revisão humana. Os agentes podem trabalhar em tarefas independentes dentro da etapa liberada, mas não iniciam a implementação da etapa sucessora sem aprovação do DEV.

## 10. Comandos e seleção da regressão

Os comandos abaixo são instruções de execução futura. Foram conferidos contra as configurações dos projetos e as instruções do repositório; não foram executados na etapa de planejamento.

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
dotnet "source/Unimake.DFe.Test/bin/Debug/net8.0/Unimake.DFe.Test.dll" -class "Unimake.DFe.Test.AACBS.Servicos.NomeDaClasseTest"
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

Uma evidência deve vincular o resultado ao estado do código executado. Em trabalho paralelo, o agente executor identifica a revisão ou o diff testado; se outro agente alterar os arquivos relevantes durante a execução, o resultado não fecha a validação da versão posterior. Builds e testes que compartilham `bin/obj` precisam de um executor coordenado, ou isolamento explícito, para evitar disputa de arquivos e resultados ambíguos.

A integração com UniNFe ou PRO não faz parte do escopo pedido. Não editar outros repositórios nem apresentar testes de um pacote anterior como validação da AACBS. Se uma etapa futura de consumo for autorizada, gerar primeiro pacote offline imutável pelo fluxo oficial, restaurar essa versão exata no consumidor e executar os filtros/builds correspondentes, sem recriar `ProjectReference`.

Não há evidência de implementação ou testes neste documento: a entrega atual é o planejamento. A especificação da AACBS-001 poderá refinar classes e agrupamentos internos, preservando as decisões de serviços individualizados, contrato XML, contexto explícito de família e aprovação humana por etapa.
