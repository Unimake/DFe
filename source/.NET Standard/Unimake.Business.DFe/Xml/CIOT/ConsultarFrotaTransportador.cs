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
    [ProgId("Unimake.Business.DFe.Xml.CIOT.ConsultarFrotaTransportador")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("ConsultarFrotaTransportador", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class ConsultarFrotaTransportador : ConsultarSituacaoTransportador
    {
        [XmlArray("Placas")]
        [XmlArrayItem("Placa")]
        public List<string> Placas { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetConsultarFrotaTransportador")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("RetConsultarFrotaTransportador", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetConsultarFrotaTransportador : XMLBase
    {
        [XmlElement("temp")]
        public TempCIOT Temp { get; set; }

        [XmlElement("CpfCnpjTransportador")]
        public string CpfCnpjTransportador { get; set; }

        [XmlElement("RNTRCTransportador")]
        public string RNTRCTransportador { get; set; }

        [XmlElement("NomeRazaoSocialTransportador")]
        public string NomeRazaoSocialTransportador { get; set; }

        [XmlElement("RNTRCAtivo")]
        public bool RNTRCAtivo { get; set; }

        [XmlArray("Frota")]
        [XmlArrayItem("VeiculoFrota")]
        public List<VeiculoFrotaCIOT> Frota { get; set; }

        [XmlElement("Mensagem")]
        public string Mensagem { get; set; }

        [XmlElement("Protocolo")]
        public string Protocolo { get; set; }

        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        public bool ShouldSerializeTemp() => Temp != null;
        public bool ShouldSerializeRNTRCAtivo() => Temp == null;
    }
}
