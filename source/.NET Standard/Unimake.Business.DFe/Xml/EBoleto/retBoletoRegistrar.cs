#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.EBoleto
{
    /// <summary>
    /// Retorno do registro de boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.retBoletoRegistrar")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("BoletoRegistrarResponse", IsNullable = false)]
    public class retBoletoRegistrar : retEBoletoRetornoBasico
    {
        /// <summary>
        /// Código de barras numérico
        /// </summary>
        [XmlElement]
        public string CodigoBarraNumerico { get; set; }

        /// <summary>
        /// Número do boleto no banco
        /// </summary>
        [XmlElement]
        public string NumeroNoBanco { get; set; }

        /// <summary>
        /// Linha digitável
        /// </summary>
        [XmlElement]
        public string LinhaDigitavel { get; set; }

        /// <summary>
        /// Indica se o PDF foi gerado com sucesso
        /// </summary>
        [XmlElement]
        public bool PdfContentSuccess { get; set; }

        /// <summary>
        /// Mensagem referente à geração do PDF
        /// </summary>
        [XmlElement]
        public string PdfContentMessage { get; set; }

        /// <summary>
        /// Conteúdo do PDF em base64
        /// </summary>
        [XmlElement]
        public string PdfContentBase64 { get; set; }

        /// <summary>
        /// Caminho do PDF gerado
        /// </summary>
        [XmlElement]
        public string PdfPath { get; set; }

        /// <summary>
        /// Conteúdo do QRCode
        /// </summary>
        [XmlElement]
        public retBoletoRegistrarQRCodeContent QRCodeContent { get; set; }

        /// <summary>
        /// Versão da DLL
        /// </summary>
        [XmlElement]
        public string DLLVersao { get; set; }
    }

    /// <summary>
    /// Conteúdo do QRCode retornado no registro do boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.retBoletoRegistrarQRCodeContent")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class retBoletoRegistrarQRCodeContent
    {
        /// <summary>
        /// Caminho ou conteúdo da imagem do QRCode
        /// </summary>
        [XmlElement]
        public string Image { get; set; }

        /// <summary>
        /// Indica se o QRCode foi gerado com sucesso
        /// </summary>
        [XmlElement]
        public bool Success { get; set; }

        /// <summary>
        /// Texto do QRCode
        /// </summary>
        [XmlElement]
        public string Text { get; set; }
    }
}
