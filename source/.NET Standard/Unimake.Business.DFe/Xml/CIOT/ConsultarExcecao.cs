#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CIOT
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.ConsultarExcecao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("ConsultarExcecao", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class ConsultarExcecao : XMLBase
    {
        [XmlElement("CpfCnpjTransportador")]
        public string CpfCnpjTransportador { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetConsultarExcecao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("RetConsultarExcecao", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetConsultarExcecao : XMLBase
    {
        [XmlElement("Retorno")]
        public RetornoExcecaoCIOT Retorno { get; set; }

        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        [XmlElement("Mensagem")]
        public string Mensagem { get; set; }
    }
}
