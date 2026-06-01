using System;
using System.Collections.Generic;
using System.Xml.Serialization;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.EBoleto
{
    /// <summary>
    /// XML de consulta de boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.BoletoConsultar")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("BoletoConsultar", IsNullable = false)]
    public class BoletoConsultar : XMLBase
    {
        /// <summary>
        /// Identificador da configuracao
        /// </summary>
        [XmlElement("ConfigurationId")]
        public string ConfigurationId { get; set; }

        /// <summary>
        /// Data de emissao inicial
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DataEmissaoInicial { get; set; }
#else
        public DateTimeOffset DataEmissaoInicial { get; set; }
#endif

        /// <summary>
        /// Data de emissao inicial no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("DataEmissaoInicial")]
        public string DataEmissaoInicialField
        {
            get => DataEmissaoInicial.ToString("yyyy-MM-dd");
#if INTEROP
            set => DataEmissaoInicial = DateTime.Parse(value);
#else
            set => DataEmissaoInicial = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Data de emissao final
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DataEmissaoFinal { get; set; }
#else
        public DateTimeOffset DataEmissaoFinal { get; set; }
#endif

        /// <summary>
        /// Data de emissao final no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("DataEmissaoFinal")]
        public string DataEmissaoFinalField
        {
            get => DataEmissaoFinal.ToString("yyyy-MM-dd");
#if INTEROP
            set => DataEmissaoFinal = DateTime.Parse(value);
#else
            set => DataEmissaoFinal = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Lista de numeros no banco
        /// </summary>
        [XmlElement("NumerosNoBanco")]
        public EBoletoNumerosNoBanco NumerosNoBanco { get; set; }

        /// <summary>
        /// Numero da pagina
        /// </summary>
        [XmlElement("PageNumber")]
        public int PageNumber { get; set; }

        /// <summary>
        /// Indica se PageNumber deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool PageNumberSpecified { get; set; }

        /// <summary>
        /// Tamanho da pagina
        /// </summary>
        [XmlElement("PageSize")]
        public int PageSize { get; set; }

        /// <summary>
        /// Indica se PageSize deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool PageSizeSpecified { get; set; }

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

        /// <summary>
        /// Indica se DataEmissaoInicialField deve ser serializado
        /// </summary>
        public bool ShouldSerializeDataEmissaoInicialField()
        {
#if INTEROP
            return DataEmissaoInicial > DateTime.MinValue;
#else
            return DataEmissaoInicial > DateTimeOffset.MinValue;
#endif
        }

        /// <summary>
        /// Indica se DataEmissaoFinalField deve ser serializado
        /// </summary>
        public bool ShouldSerializeDataEmissaoFinalField()
        {
#if INTEROP
            return DataEmissaoFinal > DateTime.MinValue;
#else
            return DataEmissaoFinal > DateTimeOffset.MinValue;
#endif
        }
    }

    /// <summary>
    /// Lista de numeros no banco
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoNumerosNoBanco")]
    [ComVisible(true)]
#endif
    public class EBoletoNumerosNoBanco
    {
        /// <summary>
        /// Numeros no banco
        /// </summary>
        [XmlElement("NumeroNoBanco")]
        public List<string> NumeroNoBanco { get; set; }
    }
}
