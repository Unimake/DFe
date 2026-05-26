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

        [XmlElement("DataDeclaracao")]
        public string DataDeclaracao { get; set; }

        [XmlElement("IndContingencia")]
        public bool IndContingencia { get; set; }

        [XmlElement("JustificativaContingencia")]
        public string JustificativaContingencia { get; set; }

        [XmlElement("DataInicioViagem")]
        public string DataInicioViagem { get; set; }

        [XmlElement("DataFimViagem")]
        public string DataFimViagem { get; set; }

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
        public bool ShouldSerializeDataFimViagem() => !string.IsNullOrEmpty(DataFimViagem);
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

        public bool ShouldSerializeAvisoTransportador() => !string.IsNullOrEmpty(AvisoTransportador);
    }
}
