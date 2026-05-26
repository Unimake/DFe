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
        public List<ParOrigemDestinoCIOT> OrigemDestino { get; set; }

        [XmlElement("DadosCarga")]
        public DadosCargaEncerramentoCIOT DadosCarga { get; set; }

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
        [XmlElement("CodigoIdentificacaoOperacao")]
        public string CodigoIdentificacaoOperacao { get; set; }

        [XmlElement("DataEncerramento")]
        public string DataEncerramento { get; set; }

        [XmlElement("Protocolo")]
        public string Protocolo { get; set; }

        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        [XmlElement("Mensagem")]
        public string Mensagem { get; set; }
    }
}
