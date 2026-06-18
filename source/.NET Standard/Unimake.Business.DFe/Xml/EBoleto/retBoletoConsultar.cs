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
        [XmlElement("BoletoResponse")]
        public List<retBoletoConsultarItem> BoletoResponse { get; set; } = new List<retBoletoConsultarItem>();

        /// <summary>
        /// Versão da DLL
        /// </summary>
        [XmlElement]
        public string DLLVersao { get; set; }
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
        [XmlElement]
        public string CodigoBarras { get; set; }

        [XmlElement]
        public string DataEmissao { get; set; }

        [XmlElement]
        public string DataLiquidacao { get; set; }

        [XmlElement]
        public string DataVencimento { get; set; }

        [XmlElement]
        public string NumeroNaEmpresa { get; set; }

        [XmlElement]
        public string NumeroNoBanco { get; set; }

        [XmlElement]
        public retBoletoConsultarPagador Pagador { get; set; }

        [XmlElement]
        public retBoletoConsultarPdfContent PdfContent { get; set; }

        [XmlElement]
        public retBoletoConsultarQrCodeContent QrCodeContent { get; set; }

        [XmlElement]
        public int Situacao { get; set; }

        [XmlElement]
        public int TipoLiquidacao { get; set; }

        [XmlElement]
        public string Valor { get; set; }

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
        [XmlElement]
        public string Codigo { get; set; }

        [XmlElement]
        public string Nome { get; set; }

        [XmlElement]
        public string Inscricao { get; set; }

        [XmlElement]
        public string Telefone { get; set; }

        [XmlElement]
        public string Email { get; set; }

        [XmlElement]
        public int TipoInscricao { get; set; }

        [XmlElement]
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
        [XmlElement]
        public string Logradouro { get; set; }

        [XmlElement]
        public string Numero { get; set; }

        [XmlElement]
        public string Complemento { get; set; }

        [XmlElement]
        public string Bairro { get; set; }

        [XmlElement]
        public string Cidade { get; set; }

        [XmlElement]
        public string UF { get; set; }

        [XmlElement]
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
        [XmlElement]
        public string Content { get; set; }

        [XmlElement]
        public bool Success { get; set; }

        [XmlElement]
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
        [XmlElement]
        public string Text { get; set; }

        [XmlElement]
        public string Image { get; set; }

        [XmlElement]
        public bool Success { get; set; }
    }
}
