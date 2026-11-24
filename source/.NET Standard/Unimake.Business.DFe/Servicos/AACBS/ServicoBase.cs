#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Xml;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.AACBS;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.AACBS
{
    /// <summary>Base dos serviços de apuração assistida da CBS.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.AACBS.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase : Servicos.ServicoBase
    {
        private static readonly Encoding Utf8 = new UTF8Encoding(false, true);
        private string emissor;
        private string metodo;
        private string metodoAutenticacao;
        private string nomeOperacao => Operacao.ToString().Substring("AACBS".Length);
        internal string Rota { get; private set; }
        internal Func<DateTimeOffset> RelogioUtc { get; set; } = () => DateTimeOffset.UtcNow;
        internal abstract Servico Operacao { get; }
        internal abstract RetornoAACBSBase ExecutarOperacao();

        internal T LerEnvio<T>() where T : AACBSBase, new() =>
            ConteudoXML == null ? null : XMLUtility.Deserializar<T>(ConteudoXML);

        internal T LerResultado<T>() where T : RetornoAACBSBase, new() =>
            RetornoWSXML == null ? new T { ErroLocal = new ErroLocal() } : XMLUtility.Deserializar<T>(RetornoWSXML);

        internal void InicializarServico(AACBSBase xml, Configuracao configuracao) =>
            Preparar(() => xml.GerarXML(), configuracao);

        internal void InicializarServico(string conteudoXML, Configuracao configuracao) =>
            Preparar(() =>
            {
                var documento = new XmlDocument { PreserveWhitespace = true, XmlResolver = null };
                using (var texto = new StringReader(conteudoXML))
                using (var leitor = XmlReader.Create(texto, new XmlReaderSettings { DtdProcessing = DtdProcessing.Prohibit }))
                {
                    documento.Load(leitor);
                }
                return documento;
            }, configuracao);

        private void Preparar(Func<XmlDocument> ler, Configuracao configuracao)
        {
            LimparConteudoXML();
            LimparRetornoWS();
            Rota = null;
            Configuracoes = configuracao ?? throw new ArgumentNullException(nameof(configuracao));
            ConferirAmbiente();
            try
            {
                var documento = ler();
                var raiz = documento.DocumentElement;
                var esperada = char.ToLowerInvariant(nomeOperacao[0]) + nomeOperacao.Substring(1);
                if (raiz?.LocalName != esperada || raiz.NamespaceURI != AACBSFormat.Namespace)
                    throw new FormatException();

                DefinirConfiguracao();
                base.Inicializar(documento, configuracao);
            }
            catch (Exception)
            {
                LimparConteudoXML();
                Rota = null;
                throw new ValidarXMLException("AACBS " + nomeOperacao + ": configuração ou XML de envio inválido.");
            }
        }

        private void ConferirAmbiente()
        {
            if (Configuracoes?.TipoAmbiente != TipoAmbiente.Producao)
                throw new InvalidOperationException("AACBS: selecione um ambiente com endpoint publicado. Produção Restrita não possui rota confirmada.");
        }

        /// <inheritdoc/>
        protected override void DefinirConfiguracao()
        {
            var oficial = CarregarConfiguracaoEmbutida(Operacao, nomeOperacao);
            metodoAutenticacao = Operacao == Servico.AACBSAutenticar ? oficial.MetodoAPI : null;
            if (oficial.RequestURILoginProducao != null)
                metodoAutenticacao = CarregarConfiguracaoEmbutida(Servico.AACBSAutenticar, "Autenticar").MetodoAPI;

            Configuracoes.TipoDFe = oficial.TipoDFe;
            Configuracoes.Servico = oficial.Servico;
            Configuracoes.CodigoUF = oficial.CodigoUF;
            Configuracoes.SchemaVersao = oficial.SchemaVersao;
            Configuracoes.Nome = oficial.Nome;
            Configuracoes.NomeUF = oficial.NomeUF;
            Configuracoes.PadraoNFSe = oficial.PadraoNFSe;
            Configuracoes.Descricao = oficial.Descricao;
            Configuracoes.TargetNS = oficial.TargetNS;
            Configuracoes.WebContentType = oficial.WebContentType;
            Configuracoes.UsaCertificadoDigital = oficial.UsaCertificadoDigital;
            Configuracoes.RequestURI = null;
            Configuracoes.RequestURIProducao = oficial.RequestURIProducao;
            Configuracoes.RequestURIHomologacao = oficial.RequestURIHomologacao;
            Configuracoes.RequestURILoginHomologacao = oficial.RequestURILoginHomologacao;
            Configuracoes.RequestURILoginProducao = oficial.RequestURILoginProducao;
            Configuracoes.MetodoAPI = oficial.MetodoAPI;
            Configuracoes.DefinirNomeTagServico(nomeOperacao);
            Configuracoes.Definida = true;
            // A mesma Configuracao pode ser usada para construir outro serviço antes de Executar.
            Rota = oficial.RequestURIProducao;
            emissor = Operacao == Servico.AACBSAutenticar ? Rota : oficial.RequestURILoginProducao;
            metodo = oficial.MetodoAPI;
        }

        private Configuracao CarregarConfiguracaoEmbutida(Servico servico, string operacao)
        {
            var oficial = new Configuracao
            {
                BuscarConfiguracaoPastaBase = false,
                TipoDFe = TipoDFe.AACBS,
                CodigoUF = (int)UFBrasil.AN,
                TipoAmbiente = Configuracoes.TipoAmbiente,
                Servico = servico,
                SchemaVersao = "1.00"
            };
            oficial.Load(operacao);
            return oficial;
        }

        private void ValidarDocumento(XmlDocument documento)
        {
            // A validação AACBS não requer nem carrega o certificado de outra integração.
            var configuracao = new Configuracao { TipoDFe = TipoDFe.AACBS, CodigoUF = (int)UFBrasil.AN,
                TipoAmbiente = Configuracoes.TipoAmbiente, Servico = Operacao, SchemaVersao = "1.00" };
            if (!new ValidarEstruturaXML().ValidarServico(documento, configuracao).Validado)
                throw new ValidarXMLException("AACBS " + nomeOperacao + ": XML incompatível com o contrato da operação.");
        }

        /// <inheritdoc/>
        protected override void XmlValidar() => ValidarDocumento(ConteudoXML);

        /// <inheritdoc/>
        protected override void XmlValidarConteudo() { } // ValidarServico aplica também o AACBSValidator.

        /// <summary>Executa uma única operação e disponibiliza somente o retorno validado.</summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            LimparRetornoWS();
            ConferirAmbiente();
            if (Rota == null || ConteudoXML == null)
                throw new InvalidOperationException("AACBS: inicialize o serviço com o XML de envio.");
            XmlValidar();
            var resultado = ExecutarOperacao();
            var documento = GerarRetornoValidado(resultado);
            var texto = XMLUtility.ObterTextoXmlAACBS(documento);
            RetornoWSXML = documento;
            RetornoWSString = texto;
        }

        private XmlDocument GerarRetornoValidado(RetornoAACBSBase retorno, bool oauth = false)
        {
            try
            {
                var documento = retorno.GerarXML();
                ValidarDocumento(documento);
                return documento;
            }
            catch (Exception)
            {
                throw ErroContrato("$", oauth);
            }
        }

        internal string RotaTiquete(string tiquete)
        {
            // A normalização de URI resolveria "." ou ".." como diretório, mudando o endpoint.
            if (string.IsNullOrEmpty(tiquete) || tiquete == "." || tiquete == "..")
                throw new InvalidOperationException("AACBS: tíquete incompatível com um único segmento de rota.");
            try { return Rota.Replace("{tiqueteSolicitacao}", Uri.EscapeDataString(tiquete)); }
            catch (UriFormatException) { throw new InvalidOperationException("AACBS: tíquete não representável na rota."); }
        }

        internal RetAutenticar ObterAutenticacao()
        {
            ValidarDestinoHttps(emissor);
            var id = Configuracoes.ClientID;
            var segredo = Configuracoes.ClientSecret;
            return CacheAutenticacao.Obter(emissor, Configuracoes.TipoAmbiente, id, segredo, RelogioUtc, () =>
            {
                // RFC 6749 §2.3.1: cada componente da credencial passa pelo form-encoding antes da junção.
                var credencial = WebUtility.UrlEncode(id) + ":" + WebUtility.UrlEncode(segredo);
                var basic = "Basic " + Convert.ToBase64String(Utf8.GetBytes(credencial));
                using (var formulario = new FormUrlEncodedContent(new[] {
                    new System.Collections.Generic.KeyValuePair<string, string>("grant_type", "client_credentials") }))
                {
                    var resultado = (RetAutenticar)Enviar(metodoAutenticacao, emissor, formulario, basic, true, RespostaAACBS.Autenticacao);
                    GerarRetornoValidado(resultado, true);
                    if (resultado.Error != null) SessaoObservacaoDisponibilidade.InformarRejeicaoAutenticacao();
                    return resultado;
                }
            });
        }

        internal RetornoAACBSBase EnviarFiscal(string uri, string corpo, Func<JsonElement, HttpStatusCode, RetornoAACBSBase> ler)
        {
            ValidarDestinoHttps(uri);
            var externo = Configuracoes.AACBSToken;
            var autenticacao = externo == null ? ObterAutenticacao() : null;
            var token = externo ?? autenticacao?.AccessToken;
            if (!RespostaAACBS.TokenValido(token))
                throw new InvalidOperationException("AACBS: autenticação sem token Bearer utilizável.");
            using (var conteudo = corpo == null ? null : new StringContent(corpo, Encoding.UTF8, "application/json"))
            {
                try { return Enviar(metodo, uri, conteudo, "Bearer " + token, false, ler); }
                finally
                {
                    if (HttpStatusCode == HttpStatusCode.Unauthorized && autenticacao != null)
                        CacheAutenticacao.Invalidar(autenticacao);
                }
            }
        }

        internal RetornoAACBSBase Baixar(Xml.AACBS.BaixarArquivoRetorno envio)
        {
            if (envio.TipoConsulta != TipoConsultaAACBS.Debitos)
                throw new InvalidOperationException("AACBS: família de arquivo ainda não disponível para download.");
            if (envio.UrlAssinadaExpiraEm <= RelogioUtc())
                throw new InvalidOperationException("AACBS: a URL assinada está expirada.");
            // O HttpClient de netstandard2.0/.NET Framework constrói o destino usando System.Uri.
            // A assinatura pode distinguir inclusive escapes equivalentes, como %41 e A.
            var original = envio.UrlAssinada;
            var inicioAlvo = original.IndexOfAny(new[] { '/', '?' }, "https://".Length);
            if (!Uri.TryCreate(original, UriKind.Absolute, out var destino) ||
                !string.Equals(destino.AbsoluteUri, original, StringComparison.Ordinal) || inicioAlvo < 0 ||
                !string.Equals(destino.PathAndQuery, original.Substring(inicioAlvo), StringComparison.Ordinal))
                throw new InvalidOperationException("AACBS: o transporte não consegue preservar a URL assinada integralmente.");

            return Enviar(metodo, envio.UrlAssinada, null, null, false,
                (objeto, http) => RespostaAACBS.Arquivo(objeto, http, envio));
        }

        private RetornoAACBSBase Enviar(string verbo, string uri, HttpContent conteudo, string autorizacao,
            bool oauth, Func<JsonElement, HttpStatusCode, RetornoAACBSBase> ler)
        {
            if (autorizacao != null)
                ValidarDestinoHttps(uri);

            var requisicao = new TransportRequest
            {
                Method = verbo.ToUpperInvariant(), RequestUri = uri, HttpContent = conteudo, Timeout = Configuracoes.TimeOutWebServiceConnect,
                DisableAutomaticClientCertificateSelection = true, AllowAutoRedirect = false, UseCookies = false, ResponseHeadersRead = true,
                UseProxy = Configuracoes.HasProxy
            };
            try
            {
                if (requisicao.UseProxy)
                    requisicao.Proxy = Proxy.DefinirServidor(Configuracoes.ProxyServer, Configuracoes.ProxyPort,
                        Configuracoes.ProxyAutoDetect, Configuracoes.ProxyUser, Configuracoes.ProxyPassword);
            }
            catch (Exception) { throw new InvalidOperationException("AACBS: configuração de proxy inválida."); }

            if (autorizacao != null)
            {
                requisicao.Headers.Add("Authorization", autorizacao);
                requisicao.Headers.Add("Accept", "application/json");
            }
            SessaoObservacaoDisponibilidade.InformarTransporte(uri, "REST", oauth ? "Autenticacao" : "Servico");
            var duracao = Stopwatch.StartNew();
            TransportResponse resposta;
            try { resposta = ApiTransportExecutorFactory.Create().Execute(requisicao); }
            catch (Exception ex)
            {
                SessaoObservacaoDisponibilidade.InformarFalhaAuxiliar(ex);
                var operacao = oauth ? "Autenticar" : nomeOperacao;
                if (ex is TimeoutException || (ex is OperationCanceledException && ex.InnerException is TimeoutException))
                    throw new TimeoutException("AACBS " + operacao + ": tempo limite na comunicação.");
                if (ex is OperationCanceledException)
                    throw new OperationCanceledException("AACBS " + operacao + ": comunicação cancelada.");
                throw new HttpRequestException("AACBS " + operacao + ": falha de transporte.");
            }

            using (resposta)
            {
                var http = resposta?.HttpResponseMessage?.StatusCode ?? resposta?.StatusCode ?? 0;
                if (http == 0) throw new HttpRequestException("AACBS: transporte sem resposta HTTP.");
                if (!oauth || Operacao == Servico.AACBSAutenticar) HttpStatusCode = http;
                SessaoObservacaoDisponibilidade.InformarRespostaHttp(http);
                try
                {
                    var texto = LerCorpo(resposta, requisicao.Timeout - duracao.ElapsedMilliseconds);
                    // No arquivo, o mapper percorre todos os objetos e indica o caminho qualificado de chaves repetidas.
                    var opcoes = new JsonDocumentOptions { AllowDuplicateProperties = Operacao == Servico.AACBSBaixarArquivoRetorno };
                    using (var json = JsonDocument.Parse(texto, opcoes))
                        return ler(json.RootElement, http);
                }
                catch (CampoInvalidoException ex) { throw ErroContrato(ex.Caminho, oauth); }
                catch (OperationCanceledException) { throw new TimeoutException("AACBS: tempo limite durante a leitura da resposta."); }
                catch (IOException) { throw new HttpRequestException("AACBS: leitura da resposta interrompida."); }
                catch (Exception) { throw ErroContrato("$", oauth); }
            }
        }

        private static void ValidarDestinoHttps(string uri)
        {
            // IsWellFormedOriginalString rejeita alguns tíquetes válidos já codificados por EscapeDataString.
            if (!Uri.TryCreate(uri, UriKind.Absolute, out var destino) ||
                destino.Scheme != Uri.UriSchemeHttps ||
                destino.GetLeftPart(UriPartial.Authority).IndexOf('@') >= 0 || destino.Fragment.Length != 0)
                throw new InvalidOperationException("AACBS: destino de requisição autenticada inválido.");
        }

        private static string LerCorpo(TransportResponse resposta, long restante)
        {
            if (restante <= 0) throw new OperationCanceledException();
            var conteudo = resposta.HttpResponseMessage?.Content;
            var stream = conteudo?.ReadAsStreamAsync().GetAwaiter().GetResult() ?? resposta.ContentStream;
            if (stream == null) return resposta.Content ?? string.Empty;
            using (var prazo = new CancellationTokenSource((int)restante))
            using (prazo.Token.Register(stream.Dispose))
            using (var buffer = new MemoryStream())
            {
                var bloco = new byte[81920];
                try
                {
                    int lidos;
                    while ((lidos = stream.ReadAsync(bloco, 0, bloco.Length, prazo.Token).GetAwaiter().GetResult()) > 0)
                        buffer.Write(bloco, 0, lidos);
                    prazo.Token.ThrowIfCancellationRequested();
                }
                catch (Exception) when (prazo.IsCancellationRequested) { throw new OperationCanceledException(); }
                if (conteudo?.Headers.ContentLength is long esperado && buffer.Length != esperado)
                    throw new IOException("AACBS: resposta incompleta.");
                return Utf8.GetString(buffer.GetBuffer(), 0, (int)buffer.Length);
            }
        }

        private FormatException ErroContrato(string caminho, bool oauth = false)
        {
            var operacao = oauth ? "Autenticar" : nomeOperacao;
            if (Operacao == Servico.AACBSBaixarArquivoRetorno) operacao += " (DEBITOS)";
            return new FormatException("AACBS " + operacao + ": resposta incompatível com o contrato em " + caminho + ".");
        }

        /// <summary>Grava o conteúdo XML solicitado pelo consumidor em UTF-8.</summary>
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML) =>
            File.WriteAllText(Path.Combine(pasta, nomeArquivo), conteudoXML, new UTF8Encoding(false));
    }
}
