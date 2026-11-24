using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Dados do DARF gerado para o contribuinte.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.GerarDarfContribuinteDadosDarf")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "GerarDarfContribuinteDadosDarfTipo", Namespace = AACBSFormat.Namespace)]
    public class GerarDarfContribuinteDadosDarf
    {
        /// <summary>
        /// Chave PIX do DARF.
        /// </summary>
        [XmlElement("chavePIX")]
        public string ChavePIX { get; set; }

        /// <summary>
        /// Código de barras do DARF.
        /// </summary>
        [XmlElement("codigoBarras")]
        public string CodigoBarras { get; set; }

        /// <summary>
        /// URI absoluta para o PDF do DARF.
        /// </summary>
        [XmlElement("linkDarfPDF", DataType = "anyURI")]
        public string LinkDarfPDF { get; set; }
    }

    /// <summary>
    /// Informações de pagamento do DARF do contribuinte.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.GerarDarfContribuinteInformacoesPagamento")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "GerarDarfContribuinteInformacoesPagamentoTipo", Namespace = AACBSFormat.Namespace)]
    public class GerarDarfContribuinteInformacoesPagamento
    {
        /// <summary>
        /// Data máxima de pagamento no formato yyyy-MM-dd.
        /// </summary>
        [XmlElement("dataMaximaPagamento")]
        public string DataMaximaPagamento { get; set; }

        /// <summary>
        /// Data de vencimento no formato yyyy-MM-dd.
        /// </summary>
        [XmlElement("dataVencimento")]
        public string DataVencimento { get; set; }

        /// <summary>
        /// Valor principal do DARF.
        /// </summary>
        [XmlIgnore]
        public decimal ValorDarfPrincipal { get; set; }

        /// <summary>
        /// Representação XML do valor principal do DARF, disponível também no INTEROP.
        /// </summary>
        [XmlElement("valorDarfPrincipal")]
        public string ValorDarfPrincipalField
        {
            get => AACBSFormat.FormatDecimal(ValorDarfPrincipal);
            set => ValorDarfPrincipal = AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor da multa do DARF.
        /// </summary>
        [XmlIgnore]
        public decimal ValorDarfMulta { get; set; }

        /// <summary>
        /// Representação XML do valor da multa do DARF, disponível também no INTEROP.
        /// </summary>
        [XmlElement("valorDarfMulta")]
        public string ValorDarfMultaField
        {
            get => AACBSFormat.FormatDecimal(ValorDarfMulta);
            set => ValorDarfMulta = AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor dos juros do DARF.
        /// </summary>
        [XmlIgnore]
        public decimal ValorDarfJuros { get; set; }

        /// <summary>
        /// Representação XML do valor dos juros do DARF, disponível também no INTEROP.
        /// </summary>
        [XmlElement("valorDarfJuros")]
        public string ValorDarfJurosField
        {
            get => AACBSFormat.FormatDecimal(ValorDarfJuros);
            set => ValorDarfJuros = AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor total do DARF.
        /// </summary>
        [XmlIgnore]
        public decimal ValorDarfTotal { get; set; }

        /// <summary>
        /// Representação XML do valor total do DARF, disponível também no INTEROP.
        /// </summary>
        [XmlElement("valorDarfTotal")]
        public string ValorDarfTotalField
        {
            get => AACBSFormat.FormatDecimal(ValorDarfTotal);
            set => ValorDarfTotal = AACBSFormat.ParseDecimal(value);
        }
    }
}
