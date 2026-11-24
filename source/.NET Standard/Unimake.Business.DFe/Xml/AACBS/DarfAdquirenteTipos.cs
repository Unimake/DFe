using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Dados de um documento para o DARF do adquirente.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.SolicitacaoDarfAdquirente")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "SolicitacaoDarfAdquirenteTipo", Namespace = AACBSFormat.Namespace)]
    public class SolicitacaoDarfAdquirente
    {
        /// <summary>
        /// Chave do documento fiscal eletrônico.
        /// </summary>
        [XmlElement("chaveDFE")]
        public string ChaveDFE { get; set; }

        /// <summary>
        /// Opção de pagamento parcial ou total.
        /// </summary>
        [XmlElement("opcaoPagamento")]
        public string OpcaoPagamento { get; set; }

        /// <summary>
        /// Valor informado para pagamento parcial do DARF.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal? ValorDarfParcial { get; set; }

        /// <summary>
        /// Valor monetário no formato XML. Ausência é representada por null, inclusive no INTEROP.
        /// </summary>
        [XmlElement("valorDarfParcial")]
        public string ValorDarfParcialField
        {
            get => ValorDarfParcial.HasValue ? AACBSFormat.FormatDecimal(ValorDarfParcial.Value) : null;
            set => ValorDarfParcial = value == null ? (decimal?)null : AACBSFormat.ParseDecimal(value);
        }
    }

    /// <summary>
    /// Dados do DARF gerado para o adquirente.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.GerarDarfAdquirenteDadosDarf")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "GerarDarfAdquirenteDadosDarfTipo", Namespace = AACBSFormat.Namespace)]
    public class GerarDarfAdquirenteDadosDarf
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
    /// Informações de pagamento do DARF do adquirente.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.GerarDarfAdquirenteInformacoesPagamento")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "GerarDarfAdquirenteInformacoesPagamentoTipo", Namespace = AACBSFormat.Namespace)]
    public class GerarDarfAdquirenteInformacoesPagamento
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
