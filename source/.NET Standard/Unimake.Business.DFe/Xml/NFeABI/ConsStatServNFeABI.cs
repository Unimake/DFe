#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFeABI
{
/// <summary>Representa o grupo ConsStatServNFeABI do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.ConsStatServNFeABI")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("consStatServNFeABI", Namespace = "http://www.portalfiscal.inf.br/nfeabi", IsNullable = false)]
    public class ConsStatServNFeABI : XMLBase
    {
        /// <summary>Obtém ou define o atributo versao da NFeABI.</summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag tpAmb da NFeABI.</summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag cUF da NFeABI.</summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag cUF da NFeABI.</summary>
        [XmlElement("cUF")]
        public int CUFField { get => (int)CUF; set => CUF = (UFBrasil)value; }
        /// <summary>Obtém ou define o conteúdo da tag xServ da NFeABI.</summary>
        [XmlElement("xServ")]
        public string XServ { get; set; } = "STATUS";
    }
}
