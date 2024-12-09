#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NF3e;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NF3e
{
    /// <summary>
    /// Enviar o XML de consulta recibo do lote de NFe para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NF3e.RetAutorizacaoSinc")]
    [ComVisible(true)]
#endif
    public class RetAutorizacaoSinc : ServicoBase, IInteropService<Unimake.Business.DFe.Xml.NF3e.RetConsReciNF3e>
    {
        #region Private Methods

        ///// <summary>
        ///// Mudar o conteúdo da tag xMotivo caso a nota tenha sido rejeitada por problemas nos itens/produtos da nota. Assim vamos retornar na xMotivo algumas informações a mais para facilitar o entendimento para o usuário.
        ///// </summary>
        //private void MudarConteudoTagRetornoXMotivo()
        //{
        //    if (EnviNFe != null)
        //    {
        //        try
        //        {
        //            var alterouXMotivo = false;

        //            var retConsReciNF3eList = RetornoWSXML.GetElementsByTagName("retConsReciNF3e");
        //            foreach (var retConsReciNF3eNode in retConsReciNF3eList)
        //            {
        //                var retConsReciNF3eElement = (XmlElement)retConsReciNF3eNode;

        //                var protNFeList = retConsReciNF3eElement.GetElementsByTagName("protNFe");
        //                foreach (var protNFeNode in protNFeList)
        //                {
        //                    var protNFeElement = (XmlElement)protNFeNode;

        //                    if (protNFeElement.GetElementsByTagName("xMotivo") != null)
        //                    {
        //                        var xMotivo = protNFeElement.GetElementsByTagName("xMotivo")[0].InnerText;

        //                        if (xMotivo.Contains("[nItem:"))
        //                        {
        //                            var nItem = Convert.ToInt32(xMotivo.Substring(xMotivo.IndexOf("[nItem:") + 7).Substring(0, xMotivo.Substring(xMotivo.IndexOf("[nItem:") + 7).Length - 1));
        //                            protNFeElement.GetElementsByTagName("xMotivo")[0].InnerText = xMotivo + "[cProd:" + EnviNFe.NFe[0].InfNFe[0].Det[nItem - 1].Prod.CProd + "][xProd:" + EnviNFe.NFe[0].InfNFe[0].Det[nItem - 1].Prod.XProd + "]";
        //                            alterouXMotivo = true;
        //                        }
        //                    }
        //                }
        //            }

        //            if (alterouXMotivo)
        //            {
        //                RetornoWSString = RetornoWSXML.OuterXml;
        //            }
        //        }
        //        catch { }
        //    }
        //}

        #endregion

        #region Protected Fields

        ///// <summary>
        ///// Objeto do XML da NFe/NFCe
        ///// É importante carregar o conteúdo deste objeto, com isso temos como resgatar informações da NFe na hora de gerar o XML com rejeição, caso ocorra, para o ERP, pois fazemos pequenas mudanças na xMotivo para facilitar o entendimento para o usuário da rejeição.
        ///// </summary>
        //public EnviNFe EnviNFe { get; set; }

        #endregion

        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ConsReciNF3e();
            xml = xml.LerXML<ConsReciNF3e>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.NF3eConsultaRecibo;
                Configuracoes.TipoAmbiente = xml.TpAmb;
                Configuracoes.SchemaVersao = xml.Versao;
                Configuracoes.CodigoUF = Convert.ToInt32(xml.NRec.Substring(0, 2));
                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetConsReciNF3e Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetConsReciNF3e>(RetornoWSXML);
                }

                return new RetConsReciNF3e
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public RetAutorizacaoSinc() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="ConsReciNF3e">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public RetAutorizacaoSinc(ConsReciNF3e ConsReciNF3e, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(ConsReciNF3e?.GerarXML() ?? throw new ArgumentNullException(nameof(ConsReciNF3e)), configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public RetAutorizacaoSinc(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            Inicializar(doc, configuracao);
        }


        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            base.Executar();
            //MudarConteudoTagRetornoXMotivo();
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="ConsReciNF3e">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar(ConsReciNF3e ConsReciNF3e, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(ConsReciNF3e?.GerarXML() ?? throw new ArgumentNullException(nameof(ConsReciNF3e)), configuracao);
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

        /// <summary>
        /// Grava o XML de Distribuição em uma pasta definida - (Para este serviço não tem XML de distribuição).
        /// </summary>
        /// <param name="pasta">Pasta onde é para ser gravado do XML</param>
        /// <param name="nomeArquivo">Nome para o arquivo XML</param>
        /// <param name="conteudoXML">Conteúdo do XML</param>
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            try
            {
                throw new Exception("Não existe XML de distribuição para consulta do recibo de lote.");
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <exception cref="NotImplementedException"></exception>
        protected override void XmlValidar()
        {
            //throw new NotImplementedException();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <exception cref="NotImplementedException"></exception>
        protected override void XmlValidarConteudo()
        {
            throw new NotImplementedException();
        }

#if INTEROP
        /// <summary>
        /// 
        /// </summary>
        /// <param name="interopType"></param>
        /// <param name="configuracao"></param>
        /// <exception cref="NotImplementedException"></exception>
        public void Executar(RetConsReciNF3e interopType, Configuracao configuracao)
        {
            base.Executar();
        }

#endif

        #endregion Public Methods
    }
}