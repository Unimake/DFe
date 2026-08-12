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


        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        [JsonIgnore]
        public string Versao { get; set; } = "1.00";

        [XmlElement("CpfCnpj")]
        [JsonProperty("cpfCnpj")]
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
        public Temp Temp { get; set; }

        [XmlElement("Sucesso")]
        public bool? Sucesso { get; set; }

        [XmlElement("Mensagem")]
        public string Mensagem { get; set; }

        [XmlElement("Dados")]
        public DadosGerarIdOperacaoTransporte Dados { get; set; }

        [XmlElement("Erros")]
        public string Erros { get; set; }

        [XmlIgnore]
        public string IdOperacaoTransporte => Dados?.CIOT;

        [XmlIgnore]
        public string Codigo => Sucesso == true ? "110" : "999";

        public bool ShouldSerializeTemp() => Temp != null;
        public bool ShouldSerializeSucesso() => Sucesso.HasValue;
        public bool ShouldSerializeDados() => Dados != null;
        public bool ShouldSerializeErros() => Erros != null;
    }

    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    public class DadosGerarIdOperacaoTransporte
    {
        [XmlElement("CIOT")]
        public string CIOT { get; set; }

        [XmlElement("CpfCnpj")]
        public string CpfCnpj { get; set; }

        [XmlElement("DataGeracao")]
        public string DataGeracao { get; set; }
    }
}
