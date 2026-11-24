using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Solicitação de geração de DARF pelo contribuinte.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.GerarDarfContribuinte")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "GerarDarfContribuinteTipo", Namespace = AACBSFormat.Namespace)]
    [XmlRoot("gerarDarfContribuinte", Namespace = AACBSFormat.Namespace, IsNullable = false)]
    public class GerarDarfContribuinte : AACBSBase
    {
        /// <summary>
        /// CNPJ completo, com quatorze dígitos.
        /// </summary>
        [XmlElement("cnpj")]
        public string Cnpj { get; set; }

        /// <summary>
        /// Período de apuração no formato MM/yyyy.
        /// </summary>
        [XmlElement("pa")]
        public string Pa { get; set; }

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
}
