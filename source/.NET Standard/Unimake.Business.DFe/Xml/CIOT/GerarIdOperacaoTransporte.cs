#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using Newtonsoft.Json;
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.CIOT
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.GerarIdOperacaoTransporte")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("GerarIdOperacaoTransporte", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class GerarIdOperacaoTransporte : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        [JsonIgnore]
        public string Versao { get; set; } = "1.00";

        [XmlElement("CpfCnpj")]
        public string CpfCnpj { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetGerarIdOperacaoTransporte")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("RetGerarIdOperacaoTransporte", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetGerarIdOperacaoTransporte : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; } = "1.00";

        [XmlElement("temp")]
        public TempCIOT Temp { get; set; }

        [XmlElement("IdOperacaoTransporte")]
        public string IdOperacaoTransporte { get; set; }

        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        [XmlElement("Mensagem")]
        public string Mensagem { get; set; }

        public bool ShouldSerializeTemp() => Temp != null;
        public bool ShouldSerializeIdOperacaoTransporte() => !string.IsNullOrWhiteSpace(IdOperacaoTransporte);
    }
}
