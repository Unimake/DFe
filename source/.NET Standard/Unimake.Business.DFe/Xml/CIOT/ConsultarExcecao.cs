#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;

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
        /// <summary>
        /// Provedor utilizado para executar o serviço CIOT.
        /// Quando não informado, o serviço utiliza a ANTT.
        /// </summary>
        [XmlElement("ProvedorCIOT")]
        [Newtonsoft.Json.JsonIgnore]
#if INTEROP
        public Unimake.Business.DFe.Servicos.ProvedorCIOT ProvedorCIOT { get; set; } = (Unimake.Business.DFe.Servicos.ProvedorCIOT)(-1);
#else
        public Unimake.Business.DFe.Servicos.ProvedorCIOT? ProvedorCIOT { get; set; }
#endif

#if INTEROP
        public bool ShouldSerializeProvedorCIOT() => ProvedorCIOT != (Unimake.Business.DFe.Servicos.ProvedorCIOT)(-1);
#else
        public bool ShouldSerializeProvedorCIOT() => ProvedorCIOT.HasValue;
#endif


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
        [XmlElement("temp")]
        public Temp Temp { get; set; }

        [XmlElement("Retorno")]
        public Retorno Retorno { get; set; }

        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        [XmlElement("Mensagem")]
        public string Mensagem { get; set; }

        public bool ShouldSerializeTemp() => Temp != null;
    }
}
