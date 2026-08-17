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
    [ProgId("Unimake.Business.DFe.Xml.CIOT.ConsultarSituacaoTransportador")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("ConsultarSituacaoTransportador", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public partial class ConsultarSituacaoTransportador : XMLBase
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


        [XmlElement("CpfCnpjInteressado")]
        public string CpfCnpjInteressado { get; set; }

        [XmlElement("CpfCnpjTransportador")]
        public string CpfCnpjTransportador { get; set; }

        [XmlElement("RNTRCTransportador")]
        public string RNTRCTransportador { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetConsultarSituacaoTransportador")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("RetConsultarSituacaoTransportador", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetConsultarSituacaoTransportador : XMLBase
    {
        [XmlElement("temp")]
        public Temp Temp { get; set; }

        [XmlElement("CpfCnpjTransportador")]
        public string CpfCnpjTransportador { get; set; }

        [XmlElement("RNTRCTransportador")]
        public string RNTRCTransportador { get; set; }

        [XmlElement("NomeRazaoSocialTransportador")]
        public string NomeRazaoSocialTransportador { get; set; }

        [XmlElement("RNTRCAtivo")]
        public bool RNTRCAtivo { get; set; }

        [XmlElement("TipoTransportador")]
        public TipoTransportadorCIOT TipoTransportador { get; set; }

        [XmlElement("EquiparadoTAC")]
        public bool EquiparadoTAC { get; set; }

        [XmlElement("Mensagem")]
        public string Mensagem { get; set; }

        [XmlElement("Protocolo")]
        public string Protocolo { get; set; }

        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        public bool ShouldSerializeTemp() => Temp != null;
        public bool ShouldSerializeRNTRCAtivo() => Temp == null;
        public bool ShouldSerializeTipoTransportador() => Temp == null;
        public bool ShouldSerializeEquiparadoTAC() => Temp == null;
    }
}
