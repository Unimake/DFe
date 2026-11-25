# Uso da Apuração Assistida da CBS por XML

A Unimake.DFe recebe XML de envio e entrega XML de retorno para a Apuração Assistida da CBS (AACBS). Para consultas, a aplicação abre uma solicitação, guarda o tíquete e decide quando consultar a situação, obter o link e baixar o arquivo. Para gerar DARF, a resposta vem no próprio POST. A DLL cuida da autenticação das chamadas à Receita e da conversão entre os XMLs públicos e os formatos da API.

## Configuração

- Use a credencial **Receita Integra** habilitada para a empresa consultada e o ambiente autorizado. Preencha `Configuracao.ClientID` e `Configuracao.ClientSecret` pelos meios protegidos que a aplicação já usa. Esses valores ficam fora dos XMLs. A AACBS usa OAuth Client Credentials sem certificado digital de cliente.
- Para a **Produção Beta** autorizada, use `Configuracao.TipoAmbiente = TipoAmbiente.Producao` (`1` no COM). A DLL seleciona as rotas de Produção Beta embutidas e qualificadas pela documentação da RFB. Para **Produção Restrita (Piloto)** ainda falta uma rota CBS oficialmente confirmada. A DLL rejeita esse ambiente antes da autenticação ou da operação. [Ambientes e acesso](../contratos/ACESSO-E-FLUXO.md)
- Os serviços fiscais obtêm o token Bearer automaticamente e o reutilizam no cache do processo por contexto de autenticação. `Autenticar` é auxiliar para quem precisa obter ou verificar o token explicitamente. Valide `Result.ErroLocal`, `Result.Error` e a presença de `Result.AccessToken`. Um token reutilizado pode vir com `HttpStatusCode = 0`, pois a consulta ao cache não emite HTTP. `ExpiresIn` mantém o prazo da emissão original, sem reiniciar a validade a cada leitura.
- Na API .NET, `Configuracao.AACBSToken` aceita um token válido já obtido e gerenciado pelo consumidor, sem o prefixo `Bearer`, e substitui a autenticação auxiliar das chamadas fiscais. O consumidor controla validade e identidade nesse modo. Em COM, use `ClientID` e `ClientSecret` com a autenticação integrada. O download externo usa somente a URL assinada como destino, sem enviar o token ou as credenciais da Receita.

## Serviços e arquivos de exemplo

As classes de serviço estão em `Unimake.Business.DFe.Servicos.AACBS` e os modelos em `Unimake.Business.DFe.Xml.AACBS`. Cada arquivo da tabela tem a raiz correspondente ao nome sem `.xml`, o namespace `http://www.unimake.com.br/dfe/aacbs` e o atributo `versao="1.00"`. A versão identifica o XML Unimake e é independente da versão HTTP.

| Serviço | XML de envio | XML de retorno |
| --- | --- | --- |
| `Autenticar` | [autenticar.xml](exemplos/autenticar.xml) | [retAutenticar.xml](exemplos/retAutenticar.xml) |
| `ConsultarDebitos` | [consultarDebitos.xml](exemplos/consultarDebitos.xml) | [retConsultarDebitos.xml](exemplos/retConsultarDebitos.xml) |
| `ConsultarCreditos` | [consultarCreditos.xml](exemplos/consultarCreditos.xml) | [retConsultarCreditos.xml](exemplos/retConsultarCreditos.xml) |
| `ConsultarPagamentos` | [consultarPagamentos.xml](exemplos/consultarPagamentos.xml) | [retConsultarPagamentos.xml](exemplos/retConsultarPagamentos.xml) |
| `ConsultarRecolhimentos` | [consultarRecolhimentos.xml](exemplos/consultarRecolhimentos.xml) | [retConsultarRecolhimentos.xml](exemplos/retConsultarRecolhimentos.xml) |
| `ConsultarSituacaoSolicitacao` | [consultarSituacaoSolicitacao.xml](exemplos/consultarSituacaoSolicitacao.xml) | [retConsultarSituacaoSolicitacao.xml](exemplos/retConsultarSituacaoSolicitacao.xml) |
| `ConsultarLinkDownload` | [consultarLinkDownload.xml](exemplos/consultarLinkDownload.xml) | [retConsultarLinkDownload.xml](exemplos/retConsultarLinkDownload.xml) |
| `BaixarArquivoRetorno` | [baixarArquivoRetorno.xml](exemplos/baixarArquivoRetorno.xml) | [retBaixarArquivoRetorno.xml](exemplos/retBaixarArquivoRetorno.xml) |
| `GerarDarfAdquirente` (RAD) | [gerarDarfAdquirente.xml](exemplos/gerarDarfAdquirente.xml) | [retGerarDarfAdquirente.xml](exemplos/retGerarDarfAdquirente.xml) |
| `GerarDarfContribuinte` (PCONT) | [gerarDarfContribuinte.xml](exemplos/gerarDarfContribuinte.xml) | [retGerarDarfContribuinte.xml](exemplos/retGerarDarfContribuinte.xml) |

O envelope `RetBaixarArquivoRetorno` contém a propriedade da família selecionada. O exemplo do envelope contém débitos e os quatro filhos também podem ser usados como documentos XML independentes.

| `tipoConsulta` no XML | `TipoConsultaAACBS` em .NET (valor COM) | Propriedade de `Result` | XML independente |
| --- | --- | --- | --- |
| `DEBITOS` | `Debitos` (1) | `ArquivoDebitos` | [retArquivoDebitos.xml](exemplos/retArquivoDebitos.xml) |
| `CREDITOS` | `Creditos` (2) | `ArquivoCreditos` | [retArquivoCreditos.xml](exemplos/retArquivoCreditos.xml) |
| `PAGAMENTOS` | `Pagamentos` (3) | `ArquivoPagamentos` | [retArquivoPagamentos.xml](exemplos/retArquivoPagamentos.xml) |
| `RECOLHIMENTOS` | `Recolhimentos` (4) | `ArquivoRecolhimentos` | [retArquivoRecolhimentos.xml](exemplos/retArquivoRecolhimentos.xml) |

Os 24 XMLs são massas sintéticas para leitura, validação e serialização offline. Use-os para conhecer a estrutura. Em operação, forneça os dados autorizados e use o tíquete e a URL devolvidos pelo serviço para a própria solicitação. Nunca envie os exemplos à Receita nem use seus tokens ou links fictícios em chamadas. Os arquivos de retorno servem como referência do resultado de cada etapa. [Catálogo XML e XSDs](../contratos/CATALOGO-XML.csv)

## Consultas da abertura ao arquivo final

1. **Abra a consulta** com o serviço da família desejada. O envio usa os oito dígitos da base do CNPJ da matriz, sem máscara e com zeros iniciais. Para acompanhar por situação, omita `urlRetorno` ou deixe-a vazia. Se a aplicação hospedar um webhook, informe uma URL HTTPS. Guarde a tentativa antes do POST. HTTP 201 com `tiqueteSolicitacao` e `tEASegundos` confirma a abertura. Persista o tíquete imediatamente com a família, a base do CNPJ e o contexto de credencial e ambiente. A estimativa pode ser `-1` quando indisponível.
2. **Retome pelo tíquete** com `ConsultarSituacaoSolicitacao` no mesmo contexto. Guarde `PENDENTE` e `EM_PROCESSAMENTO` para acompanhamento posterior. Em `CONCLUIDA`, passe à consulta de link. Em `ERRO` ou `EXPIRADA`, registre o desfecho e trate os campos de erro presentes. `EXPIRADA` pode vir com HTTP 200.
3. **Obtenha o link** com `ConsultarLinkDownload`, usando o mesmo tíquete. Guarde `urlAssinada` e, se fornecida, `urlAssinadaExpiraEm` sob acesso protegido. Preserve a URL inteira, incluindo a query e seus escapes. A FAQ indica disponibilidade de até 48 horas a partir do aviso de pronto. Respeite também a expiração individual quando conhecida. Se o instante do aviso for desconhecido, use apenas a informação efetivamente disponível e a política operacional da aplicação.
4. **Baixe e leia o XML final** chamando `BaixarArquivoRetorno` com a família, a base do CNPJ e o tíquete da abertura, além da URL e da expiração fornecidas. A DLL valida o contexto e entrega o ramo indicado da tabela. Uma apuração vazia pode ser válida, por isso mantenha a família associada ao tíquete mesmo quando nenhum item vier no arquivo. Descarte a URL após o uso ou a expiração. Um download incompleto exige tratar a falha e preservar o tíquete. A possibilidade de renovar ou reutilizar o link depende da Receita.

No XML de retorno do download, `ni` aceita oito ou 14 dígitos em débitos e créditos. Quando a Receita devolve 14, a DLL confere os oito iniciais com a base enviada e preserva o identificador inteiro. A exceção de créditos foi aprovada pelo DEV em 25/11/2026 após o Beta entregar 14 dígitos com base e tíquete correspondentes, embora o schema oficial ainda descreva oito. Pagamentos e recolhimentos exigem oito dígitos no `ni`. As quatro aberturas e a entrada de download continuam usando oito no `cnpj`. [Contrato de consultas](../contratos/CONSULTAS.md) · [Evidência histórica de créditos](../evidencias/AACBS-004/tentativa-0001/PROPOSTA-NI-CREDITOS.md)

## Exemplo .NET com um XML de envio

Inclua as diretivas `using` no arquivo e os métodos na classe da aplicação que já referencia a DLL. Passe o caminho de um XML autorizado com a estrutura de `consultarDebitos.xml` e as credenciais carregadas pelo consumidor. A aplicação chama cada operação no momento devido e fornece a família, a base e o tíquete do seu próprio registro persistido.

```csharp
using System.IO;
using Unimake.Business.DFe.Servicos;
using Modelo = Unimake.Business.DFe.Xml.AACBS;
using Servico = Unimake.Business.DFe.Servicos.AACBS;

// Métodos da aplicação consumidora
static Configuracao CriarConfiguracaoBeta(string clientId, string clientSecret)
{
    return new Configuracao
    {
        TipoAmbiente = TipoAmbiente.Producao,
        ClientID = clientId,
        ClientSecret = clientSecret
    };
}

static Servico.ConsultarDebitos AbrirDebitos(string caminhoXml, Configuracao configuracao)
{
    var servico = new Servico.ConsultarDebitos(File.ReadAllText(caminhoXml), configuracao);
    servico.Executar();
    return servico;
}

static Servico.ConsultarSituacaoSolicitacao ConsultarSituacao(
    string tiquetePersistido, Configuracao configuracao)
{
    var envio = new Modelo.ConsultarSituacaoSolicitacao { TiqueteSolicitacao = tiquetePersistido };
    var servico = new Servico.ConsultarSituacaoSolicitacao(envio, configuracao);
    servico.Executar();
    return servico;
}

static Servico.ConsultarLinkDownload ObterLink(string tiquetePersistido, Configuracao configuracao)
{
    var envio = new Modelo.ConsultarLinkDownload { TiqueteSolicitacao = tiquetePersistido };
    var servico = new Servico.ConsultarLinkDownload(envio, configuracao);
    servico.Executar();
    return servico;
}

static Servico.BaixarArquivoRetorno Baixar(
    TipoConsultaAACBS familia, string cnpjBase, string tiquetePersistido,
    Modelo.RetConsultarLinkDownload link, Configuracao configuracao)
{
    var envio = new Modelo.BaixarArquivoRetorno
    {
        TipoConsulta = familia,
        Cnpj = cnpjBase,
        TiqueteSolicitacao = tiquetePersistido,
        UrlAssinada = link.UrlAssinada,
        UrlAssinadaExpiraEmField = link.UrlAssinadaExpiraEmField
    };
    var servico = new Servico.BaixarArquivoRetorno(envio, configuracao);
    servico.Executar();
    return servico;
}
```

- Após a abertura, leia `abertura.HttpStatusCode` e `abertura.Result`. Se HTTP for 201, `ErroLocal` e `CodigoErro` estiverem ausentes e houver `TiqueteSolicitacao`, persista esse valor. Para outras famílias, troque a classe e o XML da abertura. As quatro classes expõem os mesmos campos de abertura.
- No acompanhamento, compare `situacao.Result.Situacao` com `SituacaoSolicitacaoAACBS.Concluida` depois de verificar HTTP, `ErroLocal` e `CodigoErro`. Chame `ObterLink` somente após a conclusão. Antes de `Baixar`, confira HTTP 200, ausência de erro e presença de `link.Result.UrlAssinada`. `UrlAssinadaExpiraEmField` mantém a forma textual devolvida.
- Em um download de débitos, leia `download.Result.ArquivoDebitos` depois de verificar HTTP 200 e `ErroLocal`. Para as outras famílias use a propriedade indicada na tabela. Para guardar o envelope validado, use `download.RetornoWSString`. Para gravar só o filho em .NET, use `GerarXML()` no modelo e grave o documento com `XmlWriterSettings.NewLineHandling = NewLineHandling.Entitize` para preservar as quebras de linha nos valores. Em COM, obtenha o texto do modelo com `GerarXMLString()`. A DLL também expõe `GravarXmlDistribuicao(pasta, nomeArquivo, conteudoXML)`, que grava em UTF-8 e depende da aplicação para proteção do destino.
- Para ler o arquivo de exemplo sem rede, use `XMLUtility.Deserializar<Modelo.RetArquivoDebitos>(File.ReadAllText(caminhoXml))` com `Unimake.Business.DFe.Utility.XMLUtility`. A leitura de um XML de demonstração não executa o serviço.

## INTEROP/COM

Com a versão INTEROP da DLL registrada, crie o modelo pelo ProgId `Unimake.Business.DFe.Xml.AACBS.<Classe>` e o serviço pelo ProgId `Unimake.Business.DFe.Servicos.AACBS.<Classe>`. Preencha o modelo com os dados da sua aplicação e chame `servico.Executar(modelo, configuracao)`. A sobrecarga COM recebe o modelo XML tipado. Para montar esse modelo a partir de um arquivo XML em VBScript, leia as tags do namespace AACBS com o parser do consumidor e atribua as propriedades correspondentes.

```vbscript
Function AbrirDebitos(cnpjBase, clientId, clientSecret)
    Dim cfg, envio, servico, resultado
    Set cfg = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
    cfg.TipoAmbiente = 1
    cfg.ClientID = clientId
    cfg.ClientSecret = clientSecret

    Set envio = CreateObject("Unimake.Business.DFe.Xml.AACBS.ConsultarDebitos")
    envio.Cnpj = cnpjBase
    envio.UrlRetorno = ""
    Set servico = CreateObject("Unimake.Business.DFe.Servicos.AACBS.ConsultarDebitos")
    servico.Executar envio, cfg

    Set resultado = servico.Result
    If servico.HttpStatusCode <> 201 Or Not (resultado.ErroLocal Is Nothing) Then
        Err.Raise vbObjectError + 201, "AACBS", "Abertura sem confirmação"
    End If
    If Len(resultado.CodigoErro & "") > 0 Or Len(resultado.TiqueteSolicitacao & "") = 0 Then
        Err.Raise vbObjectError + 202, "AACBS", "Abertura sem tíquete"
    End If
    AbrirDebitos = resultado.TiqueteSolicitacao
End Function
```

Trate erros COM na aplicação chamadora e persista o tíquete retornado pela função. Para retomar, crie os modelos `ConsultarSituacaoSolicitacao` e `ConsultarLinkDownload` com `TiqueteSolicitacao` e chame os serviços correspondentes separadamente. A situação COM vale 1 para pendente, 2 para em processamento, 3 para concluída, 4 para erro e 5 para expirada. No modelo `BaixarArquivoRetorno`, preencha `TipoConsulta` com o valor COM da família da tabela, `Cnpj`, `TiqueteSolicitacao`, `UrlAssinada` e `UrlAssinadaExpiraEmField` quando houver expiração. Leia o ramo tipado em `servico.Result` ou todo o envelope em `servico.RetornoWSString`.

As coleções usam os métodos COM dos próprios modelos. Por exemplo, no RAD, crie `Unimake.Business.DFe.Xml.AACBS.SolicitacaoDarfAdquirente` para cada item e use `AddSolicitacoes(item)`, `GetSolicitacoes(indice)` e `GetSolicitacoesCount()`. Os índices começam em zero. Para valores decimais ou datas com fuso, use as propriedades textuais quando o modelo expuser o sufixo `Field`. [Contratos XML](../contratos/ESPECIFICACAO.md)

## Emissão de DARF RAD e PCONT

Antes de qualquer **POST real de teste**, prepare o cenário com o CNPJ completo de 14 dígitos, a opção total ou parcial e o valor, quando parcial. Para RAD, inclua as chaves de DF-e habilitadas. Para PCONT, inclua o período no formato `MM/yyyy`. Apresente os parâmetros e obtenha confirmação específica do DEV para a operação e o cenário antes de emitir.

- **RAD** recebe uma ou mais entradas em `solicitacoes/item`, na ordem informada. A resposta é global e pode trazer um único grupo `darf`. Trate `DARF_GERADO` como o resultado declarado da operação. A resposta não permite correlacionar um DARF ou um desfecho a cada item.
- **PCONT** recebe um período em `pa` e os campos de opção e valor na raiz. O retorno inclui principal, multa, juros e total. Os dois serviços preservam a variante recebida da data, `dataMaximaPagamento` ou `dataVencimento`.
- Para `PAGAMENTO_PARCIAL`, informe `valorDarfParcial` em cada item RAD ou no objeto PCONT. Para `PAGAMENTO_TOTAL`, o campo parcial fica fora do envio à API.

Com um cenário confirmado e um arquivo XML próprio para ele, crie `new Servico.GerarDarfAdquirente(File.ReadAllText(caminhoXml), configuracao)` ou `new Servico.GerarDarfContribuinte(File.ReadAllText(caminhoXml), configuracao)`, execute `Executar()` uma vez e verifique `HttpStatusCode`, `Result.ErroLocal` e `Result.CdRetorno`. Os erros de aplicação reconhecidos podem trazer `Mensagem` e `DadosInvalidos`. Em `DARF_GERADO`, os grupos `Darf` e `InformacoesPagamento` contêm os dados do documento. O fluxo termina na geração e leitura da resposta. PIX, código de barras e `linkDarfPDF` são dados retornados. A DLL não efetua pagamento, não aciona PIX e não baixa o PDF. [Contratos RAD e PCONT](../contratos/DARF.md)

## Erros, retomada e cotas

| Observação | Tratamento |
| --- | --- |
| `Result.ErroLocal.Codigo = "SEM_RETORNO"` | Indica falta de retorno validado da operação. Verifique a exceção da execução e o HTTP conhecido. O código é local. |
| Retorno reconhecido com erro de aplicação | Leia `CodigoErro` e `MensagemErro` nas consultas, `Error` na autenticação, ou `CdRetorno`, `Mensagem` e `DadosInvalidos` nos DARFs. Avalie também HTTP e a situação quando aplicável. Preserve esses dados onde tiverem proteção adequada. |
| Exceção de validação, transporte ou contrato | Capture a exceção que chega do construtor ou de `Executar()`. Se o serviço recebeu resposta mas o corpo era incompatível, a execução pode falhar com `HttpStatusCode` ainda preenchido. HTTP zero indica ausência de status da operação atual, inclusive em falha OAuth anterior ao negócio. |
| HTTP 401, 403 ou 429 | A DLL invalida o token automático usado em um 401 fiscal, sem repetir a chamada. Verifique o contexto de autenticação ou autorização antes de nova tentativa. Em 429, interrompa o envio e respeite a cota aplicável. |
| POST enviado sem confirmação | Mantenha o resultado como ambíguo. Retome pelo tíquete se ele já for conhecido ou encaminhe para conciliação. Uma nova abertura ou emissão automática pode duplicar efeitos e consumir cota. |

A aplicação coordena pausa, limite de acompanhamento, agendamento, persistência de tíquetes e contagem de chamadas. A DLL executa uma operação explícita por vez, sem polling, reabertura ou reenvio automático. A FAQ informa até quatro aberturas por API, dia e credencial, com virada à meia-noite sem fuso definido. O guia informa até 240 minutos de processamento. A contagem local é conservadora e não comprova saldo oficial. Não aplique a cota de abertura aos GETs nem a presuma para DARFs. [Acompanhamento e cotas](../contratos/ACESSO-E-FLUXO.md)

Receita Integra documenta uma hora de validade do token e até duas emissões por hora. O cache automático atende ao processo atual. Quando houver vários processos, cabe ao consumidor coordenar reuso e orçamento por credencial e emissor, inclusive após reinícios. O harness de testes usa cache cifrado DPAPI, sincronização entre processos e registro local das tentativas. Seus limites de execução pertencem ao roteiro de teste. [Roteiro local e limites](../ACESSO-LOCAL.md)

Trate tokens, credenciais, tíquetes, URLs assinadas, campos de DARF e XMLs fiscais como dados protegidos. Restrinja o acesso a arquivos operacionais e mantenha-os fora do controle de versão. Para diagnóstico, registre a operação, a fase, o status HTTP disponível e uma categoria de erro sanitizada. Evite incluir corpo bruto, query, valores de credenciais ou arquivos fiscais em logs e evidências.

## Estado da verificação real em Produção Beta

| Operação | Evidência e condição de retomada |
| --- | --- |
| Autenticação, débitos, situação, link e download | O ciclo real de débitos terminou com XML validado e apuração vazia. A materialização dos grupos com itens foi coberta offline. [Evidência AACBS-003](../evidencias/AACBS-003/tentativa-0001/ENTREGA.md) |
| Créditos | Abertura, situação, link e download responderam no Beta. Após aprovação do NI 8/14, o original cifrado foi processado integralmente pela DLL final na 005, sem nova chamada remota: dois períodos e 27 créditos, NI14 preservado, XSDs e conteúdo válidos. XML cifrado e journal finalizado; todos os demais campos do journal preservados. [Verificação AACBS-005](../evidencias/AACBS-005/tentativa-0001/VERIFICACAO-REAL-CREDITOS.json) |
| Pagamentos e recolhimentos | Os POSTs retornaram HTTP 404 com corpo incompatível e nenhum tíquete confirmado. A causa permanece indeterminada. Qualifique a rota ou habilitação com a Receita antes de retomar e preserve a tentativa ambígua. [Pesquisa dos 404](../evidencias/AACBS-004/tentativa-0001/PESQUISA-404.md) |
| DARF RAD e PCONT | A cobertura disponível é offline. A prova real aguarda parâmetros de cenários habilitados e confirmação específica do DEV antes de cada cenário de emissão. [Evidência AACBS-004](../evidencias/AACBS-004/tentativa-0001/ENTREGA.md) |

[PDCA e registro das aprovações](../PDCA.md) · [Evidências da AACBS-005](../evidencias/AACBS-005/tentativa-0001/ENTREGA.md)
