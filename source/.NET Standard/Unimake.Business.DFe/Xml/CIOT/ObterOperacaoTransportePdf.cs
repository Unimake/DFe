#if INTEROP
using System.Runtime.InteropServices;
#endif
using Newtonsoft.Json;
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CIOT
{
    /// <summary>Solicita o PDF de uma operação de transporte na eFrete.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.ObterOperacaoTransportePdf")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("ObterOperacaoTransportePdf", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class ObterOperacaoTransportePdf : XMLBase
    {
        /// <summary>Provedor CIOT. Este serviço é exclusivo da eFrete.</summary>
        [XmlElement("ProvedorCIOT")]
        [JsonIgnore]
#if INTEROP
        public ProvedorCIOT ProvedorCIOT { get; set; } = (ProvedorCIOT)(-1);
#else
        public ProvedorCIOT? ProvedorCIOT { get; set; }
#endif

        /// <summary>Código de identificação da operação de transporte.</summary>
        [XmlElement("CodigoIdentificacaoOperacao")]
        public string CodigoIdentificacaoOperacao { get; set; }

        /// <summary>Documento da viagem, quando for necessário obter o PDF de uma viagem específica.</summary>
        [XmlElement("DocumentoViagem")]
        public string DocumentoViagem { get; set; }

#if INTEROP
        /// <summary>Indica se o provedor deve ser serializado.</summary>
        public bool ShouldSerializeProvedorCIOT() => ProvedorCIOT != (ProvedorCIOT)(-1);
#else
        /// <summary>Indica se o provedor deve ser serializado.</summary>
        public bool ShouldSerializeProvedorCIOT() => ProvedorCIOT.HasValue;
#endif

        /// <summary>Indica se o documento da viagem deve ser serializado.</summary>
        public bool ShouldSerializeDocumentoViagem() => !string.IsNullOrWhiteSpace(DocumentoViagem);
    }

    /// <summary>Retorno da obtenção do PDF da operação de transporte na eFrete.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetObterOperacaoTransportePdf")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("RetObterOperacaoTransportePdf", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetObterOperacaoTransportePdf : XMLBase
    {
        /// <summary>Detalhes do erro normalizado.</summary>
        [XmlElement("temp")]
        public Temp Temp { get; set; }

        /// <summary>Código retornado pela eFrete.</summary>
        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        /// <summary>Mensagem retornada pela eFrete.</summary>
        [XmlElement("Mensagem")]
        public string Mensagem { get; set; }

        /// <summary>Conteúdo do PDF codificado em Base64.</summary>
        [XmlElement("Pdf")]
        public string Pdf { get; set; }

        /// <summary>Indica se a operação foi concluída com sucesso.</summary>
        [XmlElement("Sucesso")]
        public bool Sucesso { get; set; }

        /// <summary>Versão retornada pela eFrete.</summary>
        [XmlElement("Versao")]
        public int Versao { get; set; }

        /// <summary>Indica se os detalhes do erro devem ser serializados.</summary>
        public bool ShouldSerializeTemp() => Temp != null;

        /// <summary>Indica se o código deve ser serializado.</summary>
        public bool ShouldSerializeCodigo() => !string.IsNullOrWhiteSpace(Codigo);

        /// <summary>Indica se a mensagem deve ser serializada.</summary>
        public bool ShouldSerializeMensagem() => !string.IsNullOrWhiteSpace(Mensagem);

        /// <summary>Indica se o PDF deve ser serializado.</summary>
        public bool ShouldSerializePdf() => !string.IsNullOrWhiteSpace(Pdf);

        /// <summary>Indica se a versão deve ser serializada.</summary>
        public bool ShouldSerializeVersao() => Versao > 0;
    }
}
