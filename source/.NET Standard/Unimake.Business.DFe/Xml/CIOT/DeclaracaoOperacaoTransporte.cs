#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Newtonsoft.Json;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CIOT
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.DeclaracaoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("DeclaracaoOperacaoTransporte", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class DeclaracaoOperacaoTransporte : XMLBase
    {
        [XmlElement("IdOperacaoTransporte")]
        public string IdOperacaoTransporte { get; set; }

        [XmlElement("TipoOperacao")]
        public TipoOperacaoTransporteCIOT TipoOperacao { get; set; }

        [XmlElement("CpfCnpjContratado")]
        public string CpfCnpjContratado { get; set; }

        [XmlElement("RNTRCContratado")]
        public string RNTRCContratado { get; set; }

        [XmlElement("CpfCnpjContratante")]
        public string CpfCnpjContratante { get; set; }

        [XmlElement("RNTRCContratante")]
        public string RNTRCContratante { get; set; }

        [XmlElement("CpfCnpjDestinatario")]
        public string CpfCnpjDestinatario { get; set; }

        [XmlElement("ValorFrete")]
        public string ValorFrete { get; set; }

        [XmlIgnore]
        [JsonIgnore]
#if INTEROP
        public DateTime DataDeclaracao { get; set; }
#else
        public DateTimeOffset DataDeclaracao { get; set; }
#endif

        [XmlElement("DataDeclaracao")]
        [JsonProperty("DataDeclaracao")]
        public string DataDeclaracaoField
        {
            get => CIOTDateTimeFormat.DateTime(DataDeclaracao);
#if INTEROP
            set => DataDeclaracao = DateTime.Parse(value);
#else
            set => DataDeclaracao = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("IndContingencia")]
        public bool IndContingencia { get; set; }

        [XmlElement("JustificativaContingencia")]
        public string JustificativaContingencia { get; set; }

        [XmlIgnore]
        [JsonIgnore]
#if INTEROP
        public DateTime DataInicioViagem { get; set; }
#else
        public DateTimeOffset DataInicioViagem { get; set; }
#endif

        [XmlElement("DataInicioViagem")]
        [JsonProperty("DataInicioViagem")]
        public string DataInicioViagemField
        {
            get => DataInicioViagem.ToString("yyyy-MM-dd");
#if INTEROP
            set => DataInicioViagem = DateTime.Parse(value);
#else
            set => DataInicioViagem = DateTimeOffset.Parse(value);
#endif
        }

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

        [XmlArray("Veiculos")]
        [XmlArrayItem("Veiculo")]
        public List<VeiculoCIOT> Veiculos { get; set; }

        [XmlArray("OrigemDestino")]
        [XmlArrayItem("ParOrigemDestino")]
        public List<ParOrigemDestinoCIOT> OrigemDestino { get; set; }

        [XmlElement("DadosCarga")]
        public DadosCargaCIOT DadosCarga { get; set; }

        [XmlArray("InfPagamento")]
        [XmlArrayItem("Pagamento")]
        public List<PagamentoCIOT> InfPagamento { get; set; }

        [XmlElement("InfIndicadoresOperacionais")]
        public IndicadoresOperacionaisCIOT InfIndicadoresOperacionais { get; set; }

        public bool ShouldSerializeRNTRCContratante() => !string.IsNullOrEmpty(RNTRCContratante);
        public bool ShouldSerializeCpfCnpjDestinatario() => !string.IsNullOrEmpty(CpfCnpjDestinatario);
        public bool ShouldSerializeJustificativaContingencia() => !string.IsNullOrEmpty(JustificativaContingencia);
        public bool ShouldSerializeDataDeclaracao() => false;
        public bool ShouldSerializeDataInicioViagem() => false;
        public bool ShouldSerializeDataFimViagem() => false;
#if INTEROP
        public bool ShouldSerializeDataFimViagemField() => DataFimViagem > DateTime.MinValue;
#else
        public bool ShouldSerializeDataFimViagemField() => DataFimViagem > DateTimeOffset.MinValue;
#endif
        public bool ShouldSerializeOrigemDestino() => OrigemDestino?.Count > 0;
        public bool ShouldSerializeDadosCarga() => DadosCarga != null;
        public bool ShouldSerializeInfIndicadoresOperacionais() => InfIndicadoresOperacionais != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetDeclaracaoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("RetDeclaracaoOperacaoTransporte", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetDeclaracaoOperacaoTransporte : XMLBase
    {
        [XmlElement("temp")]
        public TempCIOT Temp { get; set; }

        [XmlElement("CodigoIdentificacaoOperacao")]
        public string CodigoIdentificacaoOperacao { get; set; }

        [XmlElement("CodigoVerificador")]
        public string CodigoVerificador { get; set; }

        [XmlElement("Protocolo")]
        public string Protocolo { get; set; }

        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        [XmlElement("Mensagem")]
        public string Mensagem { get; set; }

        [XmlElement("AvisoTransportador")]
        public string AvisoTransportador { get; set; }

        public bool ShouldSerializeTemp() => Temp != null;
        public bool ShouldSerializeAvisoTransportador() => !string.IsNullOrEmpty(AvisoTransportador);
    }
}
