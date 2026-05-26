#pragma warning disable CS1591

#if INTEROP
using System.Collections.Generic;
using System.Runtime.InteropServices;
#else
using System.Collections.Generic;
#endif

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CIOT
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.ConsultarCIOTGerado")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("ConsultarCIOTGerado", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class ConsultarCIOTGerado : XMLBase
    {
        [XmlElement("CodigoIdentificacaoOperacao")]
        public string CodigoIdentificacaoOperacao { get; set; }

        [XmlElement("AnoDeclaracao")]
        public int AnoDeclaracao { get; set; }

        public bool ShouldSerializeAnoDeclaracao() => AnoDeclaracao > 0;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetConsultarCIOTGerado")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("RetConsultarCIOTGerado", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetConsultarCIOTGerado : XMLBase
    {
        [XmlElement("CodigoIdentificacaoOperacao")]
        public string CodigoIdentificacaoOperacao { get; set; }

        [XmlArray("Codigo")]
        [XmlArrayItem("Item")]
        public List<string> Codigo { get; set; }

        [XmlArray("Mensagem")]
        [XmlArrayItem("Item")]
        public List<string> Mensagem { get; set; }
    }
}
