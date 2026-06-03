#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using Newtonsoft.Json;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CIOT
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.CancelamentoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("CancelamentoOperacaoTransporte", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class CancelamentoOperacaoTransporte : XMLBase
    {
        [XmlElement("CodigoIdentificacaoOperacao")]
        public string CodigoIdentificacaoOperacao { get; set; }

        [XmlElement("MotivoCancelamento")]
        public string MotivoCancelamento { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetCancelamentoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("RetCancelamentoOperacaoTransporte", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetCancelamentoOperacaoTransporte : XMLBase
    {
        [XmlElement("temp")]
        public TempCIOT Temp { get; set; }

        [XmlElement("CodigoIdentificacaoOperacao")]
        public string CodigoIdentificacaoOperacao { get; set; }

        [XmlIgnore]
        [JsonIgnore]
#if INTEROP
        public DateTime DataCancelamento { get; set; }
#else
        public DateTimeOffset DataCancelamento { get; set; }
#endif

        [XmlElement("DataCancelamento")]
        [JsonProperty("DataCancelamento")]
        public string DataCancelamentoField
        {
            get => CIOTDateTimeFormat.DateTime(DataCancelamento);
            set => DataCancelamento = CIOTDateTimeFormat.ParseDateTimeOrMinValue(value);
        }

        [XmlElement("Protocolo")]
        public string Protocolo { get; set; }

        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        [XmlElement("Mensagem")]
        public string Mensagem { get; set; }

        public bool ShouldSerializeTemp() => Temp != null;
        public bool ShouldSerializeDataCancelamento() => false;
#if INTEROP
        public bool ShouldSerializeDataCancelamentoField() => Temp == null && DataCancelamento > DateTime.MinValue;
#else
        public bool ShouldSerializeDataCancelamentoField() => Temp == null && DataCancelamento > DateTimeOffset.MinValue;
#endif
    }
}
