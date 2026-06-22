#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.ConsumirServico.Compatibility;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Security;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.UMessenger
{
    /// <summary>
    /// Classe base para os serviços do uMessenger
    /// </summary>
#if INTEROP
    [ComVisible(false)]
#endif
    public abstract class ServicoBase : Servicos.ServicoBase
    {
        #region Protected Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        protected ServicoBase() : base() { }

        #endregion Protected Constructors

        #region Protected Methods

        /// <summary>
        /// Definir configurações
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            Configuracoes.CodigoUF = (int)UFBrasil.AN;
            Configuracoes.SchemaVersao = "1.00";
            Configuracoes.UsaCertificadoDigital = false;
            Configuracoes.Load(GetType().Name);
            Configuracoes.Definida = true;
        }

        /// <summary>
        /// Adquire token Bearer e, opcionalmente, acrescenta o nome da instância às URLs de requisição.
        /// </summary>
        protected void ConfigureAuth(string instanceName = null)
        {
            var token = UMessengerTokenCache.GetOrAcquireToken(
                Configuracoes.AppId,
                Configuracoes.Secret,
                Configuracoes.TipoAmbiente != TipoAmbiente.Producao,
                Configuracoes.RequestURILoginProducao,
                Configuracoes.RequestURILoginHomologacao);

            Configuracoes.MunicipioToken = "Bearer " + token;

            if (!string.IsNullOrWhiteSpace(instanceName))
            {
                var encoded = Uri.EscapeDataString(instanceName.Trim());
                Configuracoes.RequestURIProducao = Configuracoes.RequestURIProducao.TrimEnd('/') + "/" + encoded;
                Configuracoes.RequestURIHomologacao = Configuracoes.RequestURIHomologacao.TrimEnd('/') + "/" + encoded;
            }
        }

        /// <summary>
        /// Validar o XML contra o schema XSD
        /// </summary>
        protected override void XmlValidar()
        {
            XmlValidarConteudo();

            if (!string.IsNullOrWhiteSpace(Configuracoes.SchemaArquivo))
            {
                var validar = new ValidarSchema();
                validar.Validar(ConteudoXML, Configuracoes.TipoDFe.ToString() + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

                if (!validar.Success)
                {
                    throw new ValidarXMLException(validar.ErrorMessage);
                }
            }
        }

        /// <summary>
        /// Validações manuais de conteúdo das tags do XML
        /// </summary>
        protected override void XmlValidarConteudo() { }

        /// <summary>
        /// Verificar assinatura (não aplicável ao uMessenger)
        /// </summary>
        protected override void VerificarAssinarXML(string tagAssinatura, string tagAtributoID) { }

        /// <summary>
        /// Inicializar o serviço
        /// </summary>
        protected override void Inicializar(XmlDocument conteudoXML, Configuracao configuracao)
        {
            Configuracoes = configuracao ?? throw new ArgumentNullException(nameof(configuracao));
            ConteudoXML = conteudoXML ?? throw new ArgumentNullException(nameof(conteudoXML));

            if (!Configuracoes.Definida)
            {
                DefinirConfiguracao();
            }

            System.Diagnostics.Trace.WriteLine(ConteudoXML?.InnerXml, "Unimake.DFe");

            XmlValidar();
        }

        /// <summary>
        /// Executa a requisição HTTP e retorna o body bruto sem passar pelo pipeline de parsing XML.
        /// Útil para endpoints que retornam JSON arrays ou respostas não parseáveis como XML.
        /// </summary>
        protected string ExecutarRaw()
        {
            var apiConfig = new ConfiguracaoApiConfigMapper().MapExplicitEnvironment(Configuracoes);
            var request = new ApiConfigTransportRequestMapper().Map(apiConfig, null);

            using (var transportResponse = new ApiTransportExecutor().Execute(request))
            using (var httpResponse = transportResponse.HttpResponseMessage)
            {
                var content = httpResponse.Content.ReadAsStringAsync().GetAwaiter().GetResult();

                if (!httpResponse.IsSuccessStatusCode)
                {
                    throw new Exception($"HTTP {(int)httpResponse.StatusCode}: {content}");
                }

                apiConfig.Dispose();
                return content;
            }
        }

        #endregion Protected Methods

        #region Public Methods

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            var apiConfig = new ConfiguracaoApiConfigMapper().MapExplicitEnvironment(Configuracoes);

            var consumirAPI = new ConsumirAPI();
            consumirAPI.ExecutarServico(apiConfig, Configuracoes.CertificadoDigital);

            RetornoWSString = consumirAPI.RetornoServicoString;
            RetornoWSRawString = consumirAPI.RetornoServicoRawString;
            RetornoWSXML = consumirAPI.RetornoServicoXML;
            RetornoWSStream = consumirAPI.RetornoServicoStream;

            apiConfig.Dispose();
            consumirAPI.Dispose();
        }

        /// <inheritdoc />
#if INTEROP
        [ComVisible(false)]
#endif
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            StreamWriter streamWriter = null;

            try
            {
                streamWriter = File.CreateText(Path.Combine(pasta, nomeArquivo));
                streamWriter.Write(conteudoXML);
            }
            finally
            {
                streamWriter?.Close();
            }
        }

        #endregion Public Methods
    }
}
