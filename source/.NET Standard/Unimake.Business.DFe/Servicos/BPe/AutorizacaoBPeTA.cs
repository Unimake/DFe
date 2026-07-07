#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.BPe;
using Unimake.Exceptions;
using BPeTAXml = Unimake.Business.DFe.Xml.BPeTA.BPeTA;

namespace Unimake.Business.DFe.Servicos.BPe
{
    /// <summary>
    /// Enviar o XML do BPe TA para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPeTA")]
    [ComVisible(true)]
#endif
    public class AutorizacaoBPeTA : ServicoBase, IInteropService<BPeTAXml>
    {
        private BPeTAXml _BPeTA;

        /// <summary>
        /// Objeto XML do BPe TA
        /// </summary>
        public BPeTAXml BPeTA
        {
            get => _BPeTA ?? (_BPeTA = new BPeTAXml().LerXML<BPeTAXml>(ConteudoXML));
            protected set => _BPeTA = value;
        }

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                Configuracoes.TipoDFe = TipoDFe.BPe;
                Configuracoes.Servico = Servico.BPeTAAutorizacao;
                DefinirConfiguracaoPorInfBPe("BPeTA");

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Efetuar ajustes no XML do BPe TA logo depois de assinado
        /// </summary>
        protected override void AjustarXMLAposAssinado()
        {
            PosicionarAssinaturaDepoisDoSuplemento();
            base.AjustarXMLAposAssinado();
        }

        private void PosicionarAssinaturaDepoisDoSuplemento()
        {
            var raiz = ConteudoXML.DocumentElement;
            if (raiz == null)
            {
                return;
            }

            var infBPeSupl = raiz.GetElementsByTagName("infBPeSupl").Count > 0 ? raiz.GetElementsByTagName("infBPeSupl")[0] : null;
            var signature = raiz.GetElementsByTagName("Signature", "http://www.w3.org/2000/09/xmldsig#").Count > 0 ? raiz.GetElementsByTagName("Signature", "http://www.w3.org/2000/09/xmldsig#")[0] : null;

            if (infBPeSupl != null && signature != null && infBPeSupl.NextSibling != signature)
            {
                raiz.RemoveChild(signature);
                raiz.InsertAfter(signature, infBPeSupl);
            }
        }

        private void DefinirConfiguracaoPorInfBPe(string tagDocumento)
        {
            if (ConteudoXML.GetElementsByTagName(tagDocumento).Count <= 0)
            {
                throw new Exception("A tag obrigatória <" + tagDocumento + "> não foi localizada no XML.");
            }

            var tagInfBPe = ConteudoXML.GetElementsByTagName("infBPe").Count > 0 ? (XmlElement)ConteudoXML.GetElementsByTagName("infBPe")[0] : null;

            if (tagInfBPe == null)
            {
                throw new Exception("A tag obrigatória <infBPe>, do grupo de tag <" + tagDocumento + ">, não foi localizada no XML.");
            }

            if (tagInfBPe.GetAttribute("versao").Length > 0)
            {
                Configuracoes.SchemaVersao = tagInfBPe.GetAttribute("versao");
            }
            else
            {
                throw new Exception("O atributo obrigatório \"versao\" da tag <infBPe>, do grupo de tag <" + tagDocumento + ">, não foi localizado no XML.");
            }

            if (tagInfBPe.GetElementsByTagName("ide").Count <= 0)
            {
                throw new Exception("A tag obrigatória <ide>, do grupo de tag <" + tagDocumento + "><infBPe>, não foi localizada no XML.");
            }

            var tagIde = (XmlElement)tagInfBPe.GetElementsByTagName("ide")[0];

            if (tagIde.GetElementsByTagName("cUF").Count <= 0)
            {
                throw new Exception("A tag obrigatória <cUF>, do grupo de tag <" + tagDocumento + "><infBPe><ide>, não foi localizada no XML.");
            }
            else if (tagIde.GetElementsByTagName("mod").Count <= 0)
            {
                throw new Exception("A tag obrigatória <mod>, do grupo de tag <" + tagDocumento + "><infBPe><ide>, não foi localizada no XML.");
            }
            else if (tagIde.GetElementsByTagName("tpEmis").Count <= 0)
            {
                throw new Exception("A tag obrigatória <tpEmis>, do grupo de tag <" + tagDocumento + "><infBPe><ide>, não foi localizada no XML.");
            }
            else if (tagIde.GetElementsByTagName("tpAmb").Count <= 0)
            {
                throw new Exception("A tag obrigatória <tpAmb>, do grupo de tag <" + tagDocumento + "><infBPe><ide>, não foi localizada no XML.");
            }

            Configuracoes.CodigoUF = Convert.ToInt32(tagIde.GetElementsByTagName("cUF")[0].InnerText);
            Configuracoes.Modelo = (ModeloDFe)Convert.ToInt32(tagIde.GetElementsByTagName("mod")[0].InnerText);
            Configuracoes.TipoEmissao = (TipoEmissao)Convert.ToInt32(tagIde.GetElementsByTagName("tpEmis")[0].InnerText);
            Configuracoes.TipoAmbiente = (TipoAmbiente)Convert.ToInt32(tagIde.GetElementsByTagName("tpAmb")[0].InnerText);
        }

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetBPe Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetBPe>(RetornoWSString);
                }

                return new RetBPe
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public AutorizacaoBPeTA() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="bpeTA">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public AutorizacaoBPeTA(BPeTAXml bpeTA, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(bpeTA?.GerarXML() ?? throw new ArgumentNullException(nameof(bpeTA)), configuracao);

            BPeTA = BPeTA.LerXML<BPeTAXml>(ConteudoXML);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public AutorizacaoBPeTA(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            Inicializar(doc, configuracao);

            BPeTA = BPeTA.LerXML<BPeTAXml>(ConteudoXML);
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço
        /// </summary>
        /// <param name="bpeTA">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        public void Executar(BPeTAXml bpeTA, Configuracao configuracao)
        {
            try
            {
                Inicializar(bpeTA?.GerarXML() ?? throw new ArgumentNullException(nameof(bpeTA)), configuracao);
                Executar();
            }
            catch (ValidarXMLException ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
            catch (CertificadoDigitalException ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

#endif

        /// <inheritdoc />
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            try
            {
                throw new Exception("Não existe XML de distribuição implementado para autorização do BPe TA.");
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }
    }
}
