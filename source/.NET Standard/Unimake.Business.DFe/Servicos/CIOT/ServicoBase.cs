#if INTEROP
using System.Runtime.InteropServices;
#endif
using Newtonsoft.Json;
using System;
using System.IO;
using System.Net.Http;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Classe base para os serviços do CIOT
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase<TEnvio, TRetorno> : Servicos.ServicoBase
        where TEnvio : XMLBase, new()
        where TRetorno : XMLBase, new()
    {
        private TEnvio envio;

        /// <summary>
        /// Objeto do XML de envio
        /// </summary>
        public TEnvio Envio
        {
            get => envio ?? (envio = new TEnvio().LerXML<TEnvio>(ConteudoXML));
            protected set => envio = value;
        }

        /// <summary>
        /// Serviço executado
        /// </summary>
        protected abstract Servico ServicoCIOT { get; }

        /// <summary>
        /// Resultado do serviço
        /// </summary>
        public TRetorno Result
        {
            get
            {
                if (RetornoWSXML?.DocumentElement != null)
                {
                    NormalizarRetorno();
                    return new TRetorno().LerXML<TRetorno>(RetornoWSXML);
                }

                return new TRetorno();
            }
        }

        /// <summary>
        /// Construtor
        /// </summary>
        protected ServicoBase() : base() { }

        /// <summary>
        /// Definir configurações
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                Configuracoes.TipoDFe = TipoDFe.CIOT;
                Configuracoes.Servico = ServicoCIOT;
                Configuracoes.CodigoUF = (int)UFBrasil.AN;
                Configuracoes.SchemaVersao = "1.00";
            }

            base.DefinirConfiguracao();
        }

        /// <summary>
        /// Criar o conteúdo JSON para consumo da API ANTT
        /// </summary>
        protected override HttpContent CriarHttpContentPadrao()
        {
            var json = JsonConvert.SerializeObject(Envio, new JsonSerializerSettings
            {
                NullValueHandling = NullValueHandling.Ignore
            });

            return new StringContent(json, Encoding.UTF8, Configuracoes.WebContentType);
        }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            XmlValidarConteudo();

            if (!string.IsNullOrWhiteSpace(Configuracoes.SchemaArquivo))
            {
                var validar = new ValidarSchema();
                validar.Validar(ConteudoXML, Configuracoes.TipoDFe + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

                if (!validar.Success)
                {
                    throw new ValidarXMLException(validar.ErrorMessage);
                }
            }
        }

        /// <summary>
        /// Validar conteúdo do XML
        /// </summary>
        protected override void XmlValidarConteudo() { }

        /// <inheritdoc />
        public override void Executar()
        {
            base.Executar();
            NormalizarRetorno();
        }

        /// <summary>
        /// Inicializar serviço
        /// </summary>
        protected void InicializarServico(TEnvio xml, Configuracao configuracao)
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(xml?.GerarXML() ?? throw new ArgumentNullException(nameof(xml)), configuracao);
        }

        /// <summary>
        /// Inicializar serviço
        /// </summary>
        protected void InicializarServico(string conteudoXML, Configuracao configuracao)
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            Inicializar(doc, configuracao);
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

        private XmlDocument CriarXMLRetornoTipado()
        {
            var doc = new XmlDocument();
            var rootName = typeof(TRetorno).Name;

            if (RetornoWSXML.DocumentElement.Name == rootName)
            {
                doc.LoadXml(RetornoWSXML.OuterXml);
                return doc;
            }

            var root = doc.CreateElement(rootName, "http://www.antt.gov.br/ciot");
            doc.AppendChild(root);

            if (RetornoWSXML.DocumentElement.LocalName == "temp" && RetornoWSXML.DocumentElement["error"] != null)
            {
                root.AppendChild(CopiarNodeComNamespace(doc, RetornoWSXML.DocumentElement, "http://www.antt.gov.br/ciot"));
                return doc;
            }

            foreach (XmlNode child in RetornoWSXML.DocumentElement.ChildNodes)
            {
                root.AppendChild(CopiarNodeComNamespace(doc, child, "http://www.antt.gov.br/ciot"));
            }

            return doc;
        }

        private void NormalizarRetorno()
        {
            if (RetornoWSXML?.DocumentElement == null)
            {
                return;
            }

            RetornoWSXML = CriarXMLRetornoTipado();
            RetornoWSString = RetornoWSXML.OuterXml;
        }

        private static XmlNode CopiarNodeComNamespace(XmlDocument doc, XmlNode origem, string ns)
        {
            if (origem.NodeType == XmlNodeType.Element)
            {
                var elemento = doc.CreateElement(origem.LocalName, ns);

                foreach (XmlAttribute atributo in origem.Attributes)
                {
                    elemento.SetAttribute(atributo.Name, atributo.Value);
                }

                foreach (XmlNode filho in origem.ChildNodes)
                {
                    elemento.AppendChild(CopiarNodeComNamespace(doc, filho, ns));
                }

                return elemento;
            }

            return doc.ImportNode(origem, true);
        }
    }
}
