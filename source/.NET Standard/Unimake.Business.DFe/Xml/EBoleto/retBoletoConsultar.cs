#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.EBoleto
{
    /// <summary>
    /// Retorno da consulta de boletos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.retBoletoConsultar")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("BoletoConsultarResponse", IsNullable = false)]
    public class retBoletoConsultar : retEBoletoRetornoBasico
    {
        /// <summary>
        /// Lista de boletos retornados
        /// </summary>
        [XmlElement("BoletoResponse", Order = 2)]
        public List<retBoletoConsultarItem> BoletoResponse { get; set; } = new List<retBoletoConsultarItem>();
    }

    /// <summary>
    /// Dados de um boleto retornado na consulta
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.retBoletoConsultarItem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class retBoletoConsultarItem
    {
        [XmlElement(Order = 0)]
        public string CodigoBarras { get; set; }

        [XmlElement(Order = 1)]
        public string DataEmissao { get; set; }

        [XmlElement(Order = 2)]
        public string DataLiquidacao { get; set; }

        [XmlElement(Order = 3)]
        public string DataVencimento { get; set; }

        [XmlElement(Order = 4)]
        public string NumeroNaEmpresa { get; set; }

        [XmlElement(Order = 5)]
        public string NumeroNoBanco { get; set; }

        [XmlElement(Order = 6)]
        public retBoletoConsultarPagador Pagador { get; set; }

        [XmlElement(Order = 7)]
        public retBoletoConsultarPdfContent PdfContent { get; set; }

        [XmlElement(Order = 8)]
        public retBoletoConsultarQrCodeContent QrCodeContent { get; set; }

        [XmlElement(Order = 9)]
        public int Situacao { get; set; }

        [XmlElement(Order = 10)]
        public int TipoLiquidacao { get; set; }

        [XmlElement(Order = 11)]
        public string Valor { get; set; }

        [XmlElement(Order = 12)]
        public string ValorAbatimento { get; set; }

        [XmlElement(Order = 13)]
        public string ValorDesconto { get; set; }

        [XmlElement(Order = 14)]
        public string ValorJuros { get; set; }

        [XmlElement(Order = 15)]
        public string ValorLiquidado { get; set; }

        [XmlElement(Order = 16)]
        public string ValorMulta { get; set; }
    }

    /// <summary>
    /// Dados do pagador na consulta de boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.retBoletoConsultarPagador")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class retBoletoConsultarPagador
    {
        [XmlElement(Order = 0)]
        public string Codigo { get; set; }

        [XmlElement(Order = 1)]
        public string Nome { get; set; }

        [XmlElement(Order = 2)]
        public string Inscricao { get; set; }

        [XmlElement(Order = 3)]
        public string Telefone { get; set; }

        [XmlElement(Order = 4)]
        public string Email { get; set; }

        [XmlElement(Order = 5)]
        public int TipoInscricao { get; set; }

        [XmlElement(Order = 6)]
        public retBoletoConsultarEndereco Endereco { get; set; }
    }

    /// <summary>
    /// Endereço do pagador na consulta de boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.retBoletoConsultarEndereco")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class retBoletoConsultarEndereco
    {
        [XmlElement(Order = 0)]
        public string Logradouro { get; set; }

        [XmlElement(Order = 1)]
        public string Numero { get; set; }

        [XmlElement(Order = 2)]
        public string Complemento { get; set; }

        [XmlElement(Order = 3)]
        public string Bairro { get; set; }

        [XmlElement(Order = 4)]
        public string Cidade { get; set; }

        [XmlElement(Order = 5)]
        public string UF { get; set; }

        [XmlElement(Order = 6)]
        public string CEP { get; set; }
    }

    /// <summary>
    /// Dados do PDF retornado na consulta
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.retBoletoConsultarPdfContent")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class retBoletoConsultarPdfContent
    {
        [XmlElement(Order = 0)]
        public string Content { get; set; }

        [XmlElement(Order = 1)]
        public bool Success { get; set; }

        [XmlElement(Order = 2)]
        public string Message { get; set; }
    }

    /// <summary>
    /// Dados do QRCode retornado na consulta
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.retBoletoConsultarQrCodeContent")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class retBoletoConsultarQrCodeContent
    {
        [XmlElement(Order = 0)]
        public string Text { get; set; }

        [XmlElement(Order = 1)]
        public string Image { get; set; }

        [XmlElement(Order = 2)]
        public bool Success { get; set; }
    }
}
