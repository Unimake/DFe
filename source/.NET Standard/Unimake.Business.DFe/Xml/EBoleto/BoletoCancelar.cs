using System;
using System.Xml.Serialization;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.EBoleto
{
    /// <summary>
    /// XML de cancelamento/baixa de boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.BoletoCancelar")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("BoletoCancelar", IsNullable = false)]
    public class BoletoCancelar : XMLBase
    {
        /// <summary>
        /// Identificador da configuracao
        /// </summary>
        [XmlElement("ConfigurationId")]
        public string ConfigurationId { get; set; }

        /// <summary>
        /// Numero no banco
        /// </summary>
        [XmlElement("NumeroNoBanco")]
        public string NumeroNoBanco { get; set; }

        /// <summary>
        /// Indicador de teste
        /// </summary>
        [XmlElement("Testing")]
        public bool Testing { get; set; }

        /// <summary>
        /// Indica se Testing deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool TestingSpecified { get; set; }

        /// <summary>
        /// Uso de servidor de homologacao
        /// </summary>
        [XmlElement("UseHomologServer")]
        public bool UseHomologServer { get; set; }

        /// <summary>
        /// Indica se UseHomologServer deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool UseHomologServerSpecified { get; set; }

        /// <summary>
        /// Versao do leiaute
        /// </summary>
        [XmlAttribute("versao")]
        public string Versao { get; set; } = "1.00";
    }
}
