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
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CIOT
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetificacaoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("RetificacaoOperacaoTransporte", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetificacaoOperacaoTransporte : XMLBase
    {
        [XmlElement("CodigoIdentificacaoOperacao")]
        public string CodigoIdentificacaoOperacao { get; set; }

        [XmlElement("ValorFrete")]
        public string ValorFrete { get; set; }

        [XmlIgnore]
        [JsonIgnore]
#if INTEROP
        public DateTime DataFimViagem { get; set; }
#else
        public DateTimeOffset DataFimViagem { get; set; }
#endif

        [XmlElement("DataFimViagem")]
        [JsonProperty("DataFimViagem")]
        public string DataFimViagemField
        {
            get => DataFimViagem.ToString("yyyy-MM-dd");
#if INTEROP
            set => DataFimViagem = DateTime.Parse(value);
#else
            set => DataFimViagem = DateTimeOffset.Parse(value);
#endif
        }

        [XmlArray("OrigemDestino")]
        [XmlArrayItem("ParOrigemDestino")]
        public List<ParOrigemDestinoCIOT> OrigemDestino { get; set; }

        [XmlElement("DadosCarga")]
        public DadosCargaCIOT DadosCarga { get; set; }

        public bool ShouldSerializeValorFrete() => !string.IsNullOrEmpty(ValorFrete);
        public bool ShouldSerializeDataFimViagem() => false;
#if INTEROP
        public bool ShouldSerializeDataFimViagemField() => DataFimViagem > DateTime.MinValue;
#else
        public bool ShouldSerializeDataFimViagemField() => DataFimViagem > DateTimeOffset.MinValue;
#endif
        public bool ShouldSerializeOrigemDestino() => OrigemDestino?.Count > 0;
        public bool ShouldSerializeDadosCarga() => DadosCarga != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetRetificacaoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("RetRetificacaoOperacaoTransporte", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetRetificacaoOperacaoTransporte : XMLBase
    {
        [XmlElement("temp")]
        public TempCIOT Temp { get; set; }

        [XmlElement("CodigoIdentificacaoOperacao")]
        public string CodigoIdentificacaoOperacao { get; set; }

        [XmlIgnore]
        [JsonIgnore]
#if INTEROP
        public DateTime DataRetificacao { get; set; }
#else
        public DateTimeOffset DataRetificacao { get; set; }
#endif

        [XmlElement("DataRetificacao")]
        [JsonProperty("DataRetificacao")]
        public string DataRetificacaoField
        {
            get => CIOTDateTimeFormat.DateTime(DataRetificacao);
            set => DataRetificacao = CIOTDateTimeFormat.ParseDateTimeOrMinValue(value);
        }

        [XmlElement("Protocolo")]
        public string Protocolo { get; set; }

        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        [XmlElement("Mensagem")]
        public string Mensagem { get; set; }

        public bool ShouldSerializeTemp() => Temp != null;
        public bool ShouldSerializeDataRetificacao() => false;
#if INTEROP
        public bool ShouldSerializeDataRetificacaoField() => Temp == null && DataRetificacao > DateTime.MinValue;
#else
        public bool ShouldSerializeDataRetificacaoField() => Temp == null && DataRetificacao > DateTimeOffset.MinValue;
#endif
    }
}
