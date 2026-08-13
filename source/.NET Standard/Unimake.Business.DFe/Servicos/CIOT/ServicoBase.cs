#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.IO;
using System.Net.Http;
using System.Xml;
using Unimake.Business.DFe.Servicos.CIOT.Provedores;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Classe base para os serviços do CIOT.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase : Servicos.ServicoBase
    {
        private IProvedorCIOT _provedor;

        /// <summary>
        /// Serviço executado.
        /// </summary>
        protected abstract Servico ServicoCIOT { get; }

        /// <summary>
        /// Nome da tag raiz do XML de retorno.
        /// </summary>
        protected abstract string NomeRootRetorno { get; }

        /// <summary>
        /// Objeto do XML de envio.
        /// </summary>
        protected abstract XMLBase XmlEnvio { get; }

        private IProvedorCIOT Provedor => _provedor ?? (_provedor = ProvedorCIOTFactory.Criar(Configuracoes.ProvedorCIOT));

        /// <summary>
        /// Construtor.
        /// </summary>
        protected ServicoBase() : base() { }

        /// <summary>
        /// Obter o XML de envio tipado.
        /// </summary>
        protected TEnvio ObterEnvio<TEnvio>(ref TEnvio envio)
            where TEnvio : XMLBase, new() => envio ?? (envio = new TEnvio().LerXML<TEnvio>(ConteudoXML));

        /// <summary>
        /// Obter o resultado tipado do serviço.
        /// </summary>
        protected TRetorno ObterResult<TRetorno>()
            where TRetorno : XMLBase, new()
        {
            if (RetornoWSXML?.DocumentElement != null)
            {
                NormalizarRetorno();
                return new TRetorno().LerXML<TRetorno>(RetornoWSXML);
            }
            return new TRetorno();
        }

        /// <summary>
        /// Define as configurações comuns e delega as particularidades ao provedor selecionado.
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
            Provedor.Configurar(Configuracoes, GetType().Name, ServicoCIOT);
        }

        /// <summary>
        /// Cria o conteúdo HTTP por meio do provedor selecionado.
        /// </summary>
        protected override HttpContent CriarHttpContentPadrao() => Provedor.CriarHttpContent(XmlEnvio, ServicoCIOT, Configuracoes);

        /// <summary>
        /// Valida o conteúdo segundo as regras do provedor e, quando aplicável, pelo schema oficial.
        /// </summary>
        protected override void XmlValidar()
        {
            var schemaEspecifico = Provedor.ObterSchemaArquivo(ServicoCIOT);
            if (!string.IsNullOrWhiteSpace(schemaEspecifico))
            {
                var validador = new ValidarSchema();
                validador.Validar(ConteudoXML, schemaEspecifico, CIOTNamespace.PortalANTT);
                if (!validador.Success)
                {
                    throw new ValidarXMLException(validador.ErrorMessage);
                }
            }

            Provedor.Validar(XmlEnvio, ServicoCIOT, Configuracoes);
            if (!Provedor.UsaValidacaoSchema) return;

            if (!string.IsNullOrWhiteSpace(schemaEspecifico)) return;

            XmlValidarConteudo();
            var resultado = ValidarXMLCentralizado();
            if (!resultado.Validado) throw new ValidarXMLException(resultado.MensagemRetorno);
        }

        /// <summary>
        /// Validar conteúdo do XML.
        /// </summary>
        protected override void XmlValidarConteudo() { }

        /// <inheritdoc />
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            Provedor.PrepararExecucao(Configuracoes);
            if (Provedor.RecriaConteudoAposPrepararExecucao)
            {
                Configuracoes.HttpContent = CriarHttpContentPadrao();
            }

            base.Executar();
            var retornoNormalizado = Provedor.NormalizarRetorno(RetornoWSRawString, ServicoCIOT);
            if (retornoNormalizado != null)
            {
                RetornoWSXML = retornoNormalizado;
                RetornoWSString = retornoNormalizado.OuterXml;
            }
            NormalizarRetorno();
        }

        /// <summary>
        /// Inicializar serviço.
        /// </summary>
        protected void InicializarServico<TEnvio>(TEnvio xml, Configuracao configuracao)
            where TEnvio : XMLBase, new()
        {
            if (configuracao is null) throw new ArgumentNullException(nameof(configuracao));
            var documento = xml?.GerarXML() ?? throw new ArgumentNullException(nameof(xml));
            AplicarProvedorDoXML(documento, configuracao);
            Inicializar(documento, configuracao);
            AtualizarHttpContentAposInicializacao();
        }

        /// <summary>
        /// Inicializar serviço.
        /// </summary>
        protected void InicializarServico(string conteudoXML, Configuracao configuracao)
        {
            if (configuracao is null) throw new ArgumentNullException(nameof(configuracao));
            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);
            AplicarProvedorDoXML(doc, configuracao);
            Inicializar(doc, configuracao);
            AtualizarHttpContentAposInicializacao();
        }

        private void AplicarProvedorDoXML(XmlDocument documento, Configuracao configuracao)
        {
            var provedor = ProvedorCIOT.ANTT;
            XmlNode tagProvedor = null;
            if (documento?.DocumentElement != null)
            {
                foreach (XmlNode node in documento.DocumentElement.ChildNodes)
                {
                    if (node.NodeType == XmlNodeType.Element && node.LocalName == "ProvedorCIOT")
                    {
                        tagProvedor = node;
                        break;
                    }
                }
            }

            if (tagProvedor != null)
            {
                if (tagProvedor.InnerText == nameof(ProvedorCIOT.EFrete))
                {
                    provedor = ProvedorCIOT.EFrete;
                }
                else if (tagProvedor.InnerText != nameof(ProvedorCIOT.ANTT))
                {
                    throw new ValidarXMLException("A tag ProvedorCIOT deve conter ANTT ou EFrete.");
                }
            }

            if (configuracao.ProvedorCIOT != provedor)
            {
                configuracao.Definida = false;
                configuracao.RequestURI = null;
            }
            configuracao.ProvedorCIOT = provedor;
            _provedor = null;
        }

        /// <summary>
        /// Atualiza o conteúdo HTTP depois que a configuração do serviço CIOT foi carregada.
        /// </summary>
        protected virtual void AtualizarHttpContentAposInicializacao()
        {
            if (Configuracoes.RequestURI != null &&
                (!string.Equals(Configuracoes.MetodoAPI, "get", StringComparison.OrdinalIgnoreCase) || Provedor.EnviaConteudoEmRequisicaoGet))
            {
                Configuracoes.HttpContent = CriarHttpContentPadrao();
            }
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

        /// <summary>
        /// Criar XML de retorno tipado.
        /// </summary>
        protected virtual XmlDocument CriarXMLRetornoTipado()
        {
            var doc = new XmlDocument();
            if (RetornoWSXML.DocumentElement.Name == NomeRootRetorno)
            {
                doc.LoadXml(RetornoWSXML.OuterXml);
                return doc;
            }

            var root = doc.CreateElement(NomeRootRetorno, "http://www.antt.gov.br/ciot");
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

        /// <summary>
        /// Normalizar XML de retorno.
        /// </summary>
        protected void NormalizarRetorno()
        {
            if (RetornoWSXML?.DocumentElement == null) return;
            RetornoWSXML = CriarXMLRetornoTipado();
            RetornoWSString = RetornoWSXML.OuterXml;
        }

        private static XmlNode CopiarNodeComNamespace(XmlDocument doc, XmlNode origem, string ns)
        {
            if (origem.NodeType != XmlNodeType.Element) return doc.ImportNode(origem, true);
            var elemento = doc.CreateElement(origem.LocalName, ns);
            foreach (XmlAttribute atributo in origem.Attributes) elemento.SetAttribute(atributo.Name, atributo.Value);
            foreach (XmlNode filho in origem.ChildNodes) elemento.AppendChild(CopiarNodeComNamespace(doc, filho, ns));
            return elemento;
        }
    }
}
