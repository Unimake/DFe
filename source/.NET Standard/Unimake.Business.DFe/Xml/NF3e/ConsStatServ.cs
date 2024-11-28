#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NF3e
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.ConsStatServ")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("consStatServNF3e", Namespace = "http://www.portalfiscal.inf.br/nf3e", IsNullable = false)]
    public class ConsStatServ : XMLBase
    {
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("xServ")]
        public string XServ { get; set; }

        [XmlAttribute(AttributeName ="versao")]
        public string versao {  get; set; }
    }
}