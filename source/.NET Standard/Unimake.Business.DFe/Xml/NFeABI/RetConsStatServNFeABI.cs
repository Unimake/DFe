#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFeABI
{
/// <summary>Representa o grupo RetConsStatServNFeABI do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.RetConsStatServNFeABI")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("retConsStatServNFeABI", Namespace = "http://www.portalfiscal.inf.br/nfeabi", IsNullable = false)]
    public class RetConsStatServNFeABI : XMLBase
    {
        /// <summary>Obtém ou define o atributo versao da NFeABI.</summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag tpAmb da NFeABI.</summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag verAplic da NFeABI.</summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag cStat da NFeABI.</summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xMotivo da NFeABI.</summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag cUF da NFeABI.</summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag cUF da NFeABI.</summary>
        [XmlElement("cUF")]
        public int CUFField { get => (int)CUF; set => CUF = (UFBrasil)value; }
        /// <summary>Obtém ou define o conteúdo da tag dhRecbto da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif
        /// <summary>Obtém ou define o conteúdo da tag dhRecbto da NFeABI.</summary>
        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRecbto = DateTime.Parse(value, CultureInfo.InvariantCulture);
#else
            set => DhRecbto = DateTimeOffset.Parse(value, CultureInfo.InvariantCulture);
#endif
        }
        /// <summary>Obtém ou define o conteúdo da tag tMed da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public int? TMed { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag tMed da NFeABI.</summary>
        [XmlElement("tMed")]
        public string TMedField { get => TMed?.ToString(CultureInfo.InvariantCulture); set => TMed = int.Parse(value, CultureInfo.InvariantCulture); }
        /// <summary>Obtém ou define o conteúdo da tag dhRetorno da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRetorno { get; set; }
#else
        public DateTimeOffset DhRetorno { get; set; }
#endif
        /// <summary>Obtém ou define o conteúdo da tag dhRetorno da NFeABI.</summary>
        [XmlElement("dhRetorno")]
        public string DhRetornoField
        {
            get => DhRetorno.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRetorno = DateTime.Parse(value, CultureInfo.InvariantCulture);
#else
            set => DhRetorno = DateTimeOffset.Parse(value, CultureInfo.InvariantCulture);
#endif
        }
        /// <summary>Obtém ou define o conteúdo da tag xObs da NFeABI.</summary>
        [XmlElement("xObs")]
        public string XObs { get; set; }
        /// <summary>Indica se o membro TMedField deve ser serializado.</summary>
        public bool ShouldSerializeTMedField() => TMed.HasValue;
        /// <summary>Indica se o membro DhRetornoField deve ser serializado.</summary>
        public bool ShouldSerializeDhRetornoField() => DhRetorno > DateTime.MinValue;
    }
}
