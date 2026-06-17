#pragma warning disable CS1591

#if INTEROP
using System.Collections.Generic;
using System.Runtime.InteropServices;
#else
using System.Collections.Generic;
#endif

using System;
using System.Xml.Serialization;
using Newtonsoft.Json;

namespace Unimake.Business.DFe.Xml.CIOT
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.EncerramentoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("EncerramentoOperacaoTransporte", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class EncerramentoOperacaoTransporte : XMLBase
    {
        [XmlElement("CodigoIdentificacaoOperacao")]
        public string CodigoIdentificacaoOperacao { get; set; }

        [XmlArray("OrigemDestino")]
        [XmlArrayItem("ParOrigemDestino")]
        public List<OrigemDestino> OrigemDestino { get; set; }

        [XmlElement("DadosCarga")]
        public DadosCargaEncerramento DadosCarga { get; set; }

        public bool ShouldSerializeOrigemDestino() => OrigemDestino?.Count > 0;
        public bool ShouldSerializeDadosCarga() => DadosCarga != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetEncerramentoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("RetEncerramentoOperacaoTransporte", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetEncerramentoOperacaoTransporte : XMLBase
    {
        [XmlElement("temp")]
        public Temp Temp { get; set; }

        [XmlElement("CodigoIdentificacaoOperacao")]
        public string CodigoIdentificacaoOperacao { get; set; }

        [XmlIgnore]
        [JsonIgnore]
#if INTEROP
        public DateTime DataEncerramento { get; set; }
#else
        public DateTimeOffset DataEncerramento { get; set; }
#endif

        [XmlElement("DataEncerramento")]
        [JsonProperty("DataEncerramento")]
        public string DataEncerramentoField
        {
            get => CIOTDateTimeFormat.DateTime(DataEncerramento);
            set => DataEncerramento = CIOTDateTimeFormat.ParseDateTimeOrMinValue(value);
        }

        [XmlElement("Protocolo")]
        public string Protocolo { get; set; }

        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        [XmlElement("Mensagem")]
        public string Mensagem { get; set; }

        public bool ShouldSerializeTemp() => Temp != null;
        public bool ShouldSerializeDataEncerramento() => false;
#if INTEROP
        public bool ShouldSerializeDataEncerramentoField() => Temp == null && DataEncerramento > DateTime.MinValue;
#else
        public bool ShouldSerializeDataEncerramentoField() => Temp == null && DataEncerramento > DateTimeOffset.MinValue;
#endif
    }
}
