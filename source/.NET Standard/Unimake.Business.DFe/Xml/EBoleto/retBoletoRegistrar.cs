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
        /// Identificador de rastreamento do erro retornado pela API
        /// </summary>
        [XmlElement]
        public string TraceId { get; set; }

        /// <summary>
        /// Indica se o PDF foi gerado com sucesso
        /// </summary>
        [XmlIgnore]
        public bool PdfContentSuccess { get; set; }

        /// <summary>
        /// Indica se o PDF foi gerado com sucesso no formato legado do XML
        /// </summary>
        [XmlElement("PdfContentSuccess")]
        public string PdfContentSuccessField
        {
            get => PdfContentSuccess.ToString();
            set => PdfContentSuccess = bool.TryParse(value, out var success) && success;
        }

        /// <summary>
        /// Serializa o indicador do PDF somente nos retornos de sucesso
        /// </summary>
        public bool ShouldSerializePdfContentSuccessField() => Status == 0;

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
        /// Dados da liquidação PIX retornados no registro
        /// </summary>
        [XmlElement]
        public retBoletoRegistrarPIXPagamentoDetalhe PixPagamentoDetalhe { get; set; }

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
        [XmlIgnore]
        public bool Success { get; set; }

        /// <summary>
        /// Indica se o QRCode foi gerado com sucesso no formato legado do XML
        /// </summary>
        [XmlElement("Success")]
        public string SuccessField
        {
            get => Success.ToString();
            set => Success = bool.TryParse(value, out var success) && success;
        }

        /// <summary>
        /// Texto do QRCode
        /// </summary>
        [XmlElement]
        public string Text { get; set; }
    }

    /// <summary>
    /// Dados da liquidação PIX retornados no registro do boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.retBoletoRegistrarPIXPagamentoDetalhe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class retBoletoRegistrarPIXPagamentoDetalhe
    {
        [XmlElement]
        public string DataPagamento { get; set; }

        [XmlElement]
        public string TxId { get; set; }

        [XmlElement]
        public string ValorAbatimento { get; set; }

        [XmlElement]
        public string ValorDesconto { get; set; }

        [XmlElement]
        public string ValorJuros { get; set; }

        [XmlElement]
        public string ValorLiquidado { get; set; }

        [XmlElement]
        public string ValorMulta { get; set; }

        [XmlElement]
        public string ValorOriginal { get; set; }
    }
}
