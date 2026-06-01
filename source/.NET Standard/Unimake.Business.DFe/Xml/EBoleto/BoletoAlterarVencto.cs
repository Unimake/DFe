using System;
using System.Xml.Serialization;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.EBoleto
{
    /// <summary>
    /// XML de alteracao de vencimento do boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.BoletoAlterarVencto")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("BoletoAlterarVencto", IsNullable = false)]
    public class BoletoAlterarVencto : XMLBase
    {
        /// <summary>
        /// Identificador da configuracao
        /// </summary>
        [XmlElement("ConfigurationId")]
        public string ConfigurationId { get; set; }

        /// <summary>
        /// Data de vencimento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DataVencimento { get; set; }
#else
        public DateTimeOffset DataVencimento { get; set; }
#endif

        /// <summary>
        /// Data de vencimento no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("DataVencimento")]
        public string DataVencimentoField
        {
            get => DataVencimento.ToString("yyyy-MM-dd");
#if INTEROP
            set => DataVencimento = DateTime.Parse(value);
#else
            set => DataVencimento = DateTimeOffset.Parse(value);
#endif
        }

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
