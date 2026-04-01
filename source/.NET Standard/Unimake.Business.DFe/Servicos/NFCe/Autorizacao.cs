#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFCe
{
    /// <summary>
    /// Enviar o XML de NFCe para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFCe.Autorizacao")]
    [ComVisible(true)]
#endif
    public class Autorizacao : NFe.Autorizacao
    {
        #region Protected Methods

        /// <summary>
        /// Efetuar um ajuste no XML da NFCe logo depois de assinado
        /// </summary>
        protected override void AjustarXMLAposAssinado()
        {
            QrCodeXmlHelper.MontarQrCodeNFCe(ConteudoXML, Configuracoes);
            base.AjustarXMLAposAssinado();
        }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            var validar = new ValidarSchema();
            validar.Validar(ConteudoXML, TipoDFe.NFe.ToString() + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

            if (!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }

        #endregion Protected Methods

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="enviNFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public Autorizacao(EnviNFe enviNFe, Configuracao configuracao) : base(enviNFe, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public Autorizacao(string conteudoXML, Configuracao configuracao) : base(conteudoXML, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public Autorizacao() : base() { }

        #endregion Public Constructors
    }
}