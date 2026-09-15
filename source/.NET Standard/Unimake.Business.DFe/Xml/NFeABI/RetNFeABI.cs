#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFeABI
{
/// <summary>Representa o grupo RetNFeABI do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.RetNFeABI")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("retNFeABI", Namespace = "http://www.portalfiscal.inf.br/nfeabi", IsNullable = false)]
    public class RetNFeABI : XMLBase
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
        /// <summary>Obtém ou define o conteúdo da tag protNFeABI da NFeABI.</summary>
        [XmlElement("protNFeABI")]
        public ProtNFeABI ProtNFeABI { get; set; }
    }
}
