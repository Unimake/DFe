using System;
using System.Xml.Serialization;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.EBoleto
{
    /// <summary>
    /// XML de envio de instrucao do boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.BoletoEnviarInstrucao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("BoletoEnviarInstrucao", IsNullable = false)]
    public class BoletoEnviarInstrucao : XMLBase
    {
        /// <summary>
        /// Identificador da configuracao
        /// </summary>
        [XmlElement("ConfigurationId")]
        public string ConfigurationId { get; set; }

        /// <summary>
        /// Data da instrucao
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime Data { get; set; }
#else
        public DateTimeOffset Data { get; set; }
#endif

        /// <summary>
        /// Data da instrucao no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("Data")]
        public string DataField
        {
            get => Data.ToString("yyyy-MM-dd");
#if INTEROP
            set => Data = DateTime.Parse(value);
#else
            set => Data = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Indica se DataField deve ser serializado
        /// </summary>
        public bool ShouldSerializeDataField()
        {
#if INTEROP
            return Data > DateTime.MinValue;
#else
            return Data > DateTimeOffset.MinValue;
#endif
        }

        /// <summary>
        /// Instrucao
        /// </summary>
        [XmlElement("Instrucao")]
        public EBoletoInstrucao Instrucao { get; set; }

        /// <summary>
        /// Instrucao adicional
        /// </summary>
        [XmlElement("InstrucaoAdicional")]
        public EBoletoInstrucaoAdicional InstrucaoAdicional { get; set; }

        /// <summary>
        /// Indica se InstrucaoAdicional deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool InstrucaoAdicionalSpecified { get; set; }

        /// <summary>
        /// Nosso numero
        /// </summary>
        [XmlElement("NossoNumero")]
        public string NossoNumero { get; set; }

        /// <summary>
        /// Numero no banco
        /// </summary>
        [XmlElement("NumeroNoBanco")]
        public string NumeroNoBanco { get; set; }

        /// <summary>
        /// Quantidade de dias de desconto
        /// </summary>
        [XmlElement("QuantidadeDiasDesconto")]
        public int QuantidadeDiasDesconto { get; set; }

        /// <summary>
        /// Indica se QuantidadeDiasDesconto deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool QuantidadeDiasDescontoSpecified { get; set; }

        /// <summary>
        /// Seu numero
        /// </summary>
        [XmlElement("SeuNumero")]
        public string SeuNumero { get; set; }

        /// <summary>
        /// Tipo de desconto
        /// </summary>
        [XmlElement("TipoDesconto")]
        public EBoletoTipoDesconto TipoDesconto { get; set; }

        /// <summary>
        /// Indica se TipoDesconto deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool TipoDescontoSpecified { get; set; }

        /// <summary>
        /// Tipo de juros
        /// </summary>
        [XmlElement("TipoJuros")]
        public EBoletoTipoJuros TipoJuros { get; set; }

        /// <summary>
        /// Indica se TipoJuros deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool TipoJurosSpecified { get; set; }

        /// <summary>
        /// Valor
        /// </summary>
        [XmlElement("Valor")]
        public decimal Valor { get; set; }

        /// <summary>
        /// Indica se Valor deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool ValorSpecified { get; set; }

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
