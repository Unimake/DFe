#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFe
{
    /// <summary>
    /// Enviar o XML de consulta recibo do lote de NFe para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFe.RetAutorizacao")]
    [ComVisible(true)]
#endif
    public class RetAutorizacao : ServicoBase, IInteropService<ConsReciNFe>
    {
        #region Private Methods

        /// <summary>
        /// Mudar o conteúdo da tag xMotivo caso a nota tenha sido rejeitada por problemas nos itens/produtos da nota. Assim vamos retornar na xMotivo algumas informações a mais para facilitar o entendimento para o usuário.
        /// </summary>
        private void MudarConteudoTagRetornoXMotivo()
        {
            if (EnviNFe != null)
            {
                try
                {
                    var alterouXMotivo = false;

                    var retConsReciNFeList = RetornoWSXML.GetElementsByTagName("retConsReciNFe");
                    foreach (var retConsReciNFeNode in retConsReciNFeList)
                    {
                        var retConsReciNFeElement = (XmlElement)retConsReciNFeNode;

                        var protNFeList = retConsReciNFeElement.GetElementsByTagName("protNFe");
                        foreach (var protNFeNode in protNFeList)
                        {
                            var protNFeElement = (XmlElement)protNFeNode;

                            if (protNFeElement.GetElementsByTagName("xMotivo") != null)
                            {
                                var xMotivo = protNFeElement.GetElementsByTagName("xMotivo")[0].InnerText;

                                if (xMotivo.Contains("[nItem:"))
                                {
                                    var nItem = Convert.ToInt32((xMotivo.Substring(xMotivo.IndexOf("[nItem:") + 7)).Substring(0, (xMotivo.Substring(xMotivo.IndexOf("[nItem:") + 7)).Length - 1));
                                    protNFeElement.GetElementsByTagName("xMotivo")[0].InnerText = xMotivo + "[cProd:" + EnviNFe.NFe[0].InfNFe[0].Det[nItem - 1].Prod.CProd + "][xProd:" + EnviNFe.NFe[0].InfNFe[0].Det[nItem - 1].Prod.XProd + "]";
                                    alterouXMotivo = true;
                                }
                            }
                        }
                    }

                    if (alterouXMotivo)
                    {
                        RetornoWSString = RetornoWSXML.OuterXml;
                    }
                }
                catch { }
            }
        }

        #endregion

        #region Protected Fields

        /// <summary>
        /// Objeto do XML da NFe/NFCe
        /// É importante carregar o conteúdo deste objeto, com isso temos como resgatar informações da NFe na hora de gerar o XML com rejeição, caso ocorra, para o ERP, pois fazemos pequenas mudanças na xMotivo para facilitar o entendimento para o usuário da rejeição.
        /// </summary>
        public EnviNFe EnviNFe { get; set; }

        #endregion

        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ConsReciNFe();
            xml = xml.LerXML<ConsReciNFe>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.NFeConsultaRecibo;
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
        public RetConsReciNFe Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetConsReciNFe>(RetornoWSXML);
                }

                return new RetConsReciNFe
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
        public RetAutorizacao() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="consReciNFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public RetAutorizacao(ConsReciNFe consReciNFe, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(consReciNFe?.GerarXML() ?? throw new ArgumentNullException(nameof(consReciNFe)), configuracao);
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

            MudarConteudoTagRetornoXMotivo();
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="consReciNFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar(ConsReciNFe consReciNFe, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(consReciNFe?.GerarXML() ?? throw new ArgumentNullException(nameof(consReciNFe)), configuracao);
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

        #endregion Public Methods
    }
}