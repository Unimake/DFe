#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
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
        #region Private Methods

        /// <summary>
        /// Definir as propriedades do QRCode e Link da consulta manual da NFCe
        /// </summary>
        private void MontarQrCode()
        {
            for (var i = 0; i < EnviNFe.NFe.Count; i++)
            {
                EnviNFe = new EnviNFe().LerXML<EnviNFe>(ConteudoXML);

                if (EnviNFe.NFe[i].InfNFeSupl == null)
                {
                    if (string.IsNullOrWhiteSpace(Configuracoes.CSC))
                    {
                        throw new Exception("Para montagem do QRCode é necessário informar o conteúdo da propriedade \"Configuracao.CSC\"");
                    }

                    if (Configuracoes.CSCIDToken <= 0)
                    {
                        throw new Exception("Para montagem do QRCode é necessário informar o conteúdo da propriedade \"Configuracao.CSCIDToken\"");
                    }

                    EnviNFe.NFe[i].InfNFeSupl = new InfNFeSupl();

                    var urlQrCode = (Configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? Configuracoes.UrlQrCodeHomologacao : Configuracoes.UrlQrCodeProducao);
                    var urlChave = (Configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? Configuracoes.UrlChaveHomologacao : Configuracoes.UrlChaveProducao);
                    string paramLinkQRCode;

                    if (EnviNFe.NFe[i].InfNFe[0].Ide.TpEmis == TipoEmissao.ContingenciaOffLine)
                    {
                        paramLinkQRCode = EnviNFe.NFe[i].InfNFe[0].Chave + "|" +
                            "2" + "|" +
                            ((int)EnviNFe.NFe[i].InfNFe[0].Ide.TpAmb).ToString() + "|" +
                            EnviNFe.NFe[i].InfNFe[0].Ide.DhEmi.ToString("dd") + "|" +
                            EnviNFe.NFe[i].InfNFe[0].Total.ICMSTot.VNFField.Trim() + "|" +
                            Converter.ToHexadecimal(EnviNFe.NFe[i].Signature.SignedInfo.Reference.DigestValue.ToString()) + "|" +
                            Configuracoes.CSCIDToken.ToString();
                    }
                    else
                    {
                        paramLinkQRCode = EnviNFe.NFe[i].InfNFe[0].Chave + "|" +
                            "2" + "|" +
                            ((int)EnviNFe.NFe[i].InfNFe[0].Ide.TpAmb).ToString() + "|" +
                            Configuracoes.CSCIDToken.ToString();
                    }

                    var hashQRCode = Converter.ToSHA1HashData(paramLinkQRCode.Trim() + Configuracoes.CSC, true);

                    EnviNFe.NFe[i].InfNFeSupl.QrCode = urlQrCode + "?p=" + paramLinkQRCode.Trim() + "|" + hashQRCode.Trim();
                    EnviNFe.NFe[i].InfNFeSupl.UrlChave = urlChave;
                }

                //Atualizar a propriedade do XML da NFCe novamente com o conteúdo atual já a tag de QRCode e link de consulta
                ConteudoXML = EnviNFe.GerarXML();
            }
        }

        #endregion Private Methods

        #region Protected Methods

        /// <summary>
        /// Efetuar um ajuste no XML da NFCe logo depois de assinado
        /// </summary>
        protected override void AjustarXMLAposAssinado()
        {
            MontarQrCode();
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
        public Autorizacao() : base() { }

        #endregion Public Constructors
    }
}