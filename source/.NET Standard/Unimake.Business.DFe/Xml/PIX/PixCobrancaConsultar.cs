using System;
using System.Xml.Serialization;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.PIX
{
    /// <summary>
    /// XML de consulta de cobrança PIX
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.PIX.PixCobrancaConsultar")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("PIXConsultaRequest", IsNullable = false)]
    public class PixCobrancaConsultar : XMLBase
    {
        /// <summary>
        /// Identificador da configuração
        /// </summary>
        [XmlElement("ConfigurationId", Order = 0)]
        public string ConfigurationId { get; set; }

        /// <summary>
        /// Data inicial de emissão do PIX
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime StartDate { get; set; }
#else
        public DateTimeOffset StartDate { get; set; }
#endif

        /// <summary>
        /// Data inicial de emissão do PIX no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("StartDate", Order = 1)]
        public string StartDateField
        {
            get => StartDate.ToString("yyyy-MM-dd");
#if INTEROP
            set => StartDate = DateTime.Parse(value);
#else
            set => StartDate = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Data final de emissão do PIX
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime EndDate { get; set; }
#else
        public DateTimeOffset EndDate { get; set; }
#endif

        /// <summary>
        /// Data final de emissão do PIX no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("EndDate", Order = 2)]
        public string EndDateField
        {
            get => EndDate.ToString("yyyy-MM-dd");
#if INTEROP
            set => EndDate = DateTime.Parse(value);
#else
            set => EndDate = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Indicador de teste
        /// </summary>
        [XmlElement("Testing", Order = 3)]
        public bool Testing { get; set; }

        /// <summary>
        /// Beneficiário
        /// </summary>
        [XmlElement("Beneficiario", Order = 4)]
        public PixBeneficiario Beneficiario { get; set; }

        /// <summary>
        /// Uso de servidor de homologação
        /// </summary>
        [XmlElement("UseHomologServer", Order = 5)]
        public bool UseHomologServer { get; set; }

        /// <summary>
        /// Indica se UseHomologServer deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool UseHomologServerSpecified { get; set; }

        /// <summary>
        /// Versão do leiaute
        /// </summary>
        [XmlAttribute("versao")]
        public string Versao { get; set; } = "1.00";

        /// <summary>
        /// Indica se StartDateField deve ser serializado
        /// </summary>
        public bool ShouldSerializeStartDateField()
        {
#if INTEROP
            return StartDate > DateTime.MinValue;
#else
            return StartDate > DateTimeOffset.MinValue;
#endif
        }

        /// <summary>
        /// Indica se EndDateField deve ser serializado
        /// </summary>
        public bool ShouldSerializeEndDateField()
        {
#if INTEROP
            return EndDate > DateTime.MinValue;
#else
            return EndDate > DateTimeOffset.MinValue;
#endif
        }

        /// <summary>
        /// Indica se Testing deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool TestingSpecified { get; set; }
    }
}
