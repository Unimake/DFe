#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.BPe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.ProtBPe")]
    [ComVisible(true)]
#endif
    public class ProtBPe
    {
        /// <summary>
        /// Versao do leiaute
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Informacoes do protocolo
        /// </summary>
        [XmlElement("infProt")]
        public InfProtBPe InfProt { get; set; }

        /// <summary>
        /// Informacoes do fisco
        /// </summary>
        [XmlElement("infFisco")]
        public InfFisco InfFisco { get; set; }

        /// <summary>
        /// Assinatura digital
        /// </summary>
        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.InfProtBPe")]
    [ComVisible(true)]
#endif
    public class InfProtBPe
    {
        /// <summary>
        /// Identificador da tag
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "ID")]
        public string Id { get; set; }

        /// <summary>
        /// Identificacao do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versao do aplicativo
        /// </summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        /// <summary>
        /// Chave de acesso do BP-e
        /// </summary>
        [XmlElement("chBPe")]
        public string ChBPe { get; set; }

        /// <summary>
        /// Data e hora do recebimento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif

        /// <summary>
        /// Data e hora do recebimento serializada no XML
        /// </summary>
        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRecbto = DateTime.Parse(value);
#else
            set => DhRecbto = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Numero do protocolo
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Digest value
        /// </summary>
        [XmlElement("digVal")]
        public string DigVal { get; set; }

        /// <summary>
        /// Codigo do status
        /// </summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }

        /// <summary>
        /// Descricao do status
        /// </summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        public bool ShouldSerializeNProt() => !string.IsNullOrEmpty(NProt);
        public bool ShouldSerializeDigVal() => !string.IsNullOrEmpty(DigVal);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.InfFisco")]
    [ComVisible(true)]
#endif
    public class InfFisco
    {
        /// <summary>
        /// Codigo da mensagem
        /// </summary>
        [XmlElement("cMsg")]
        public int CMsg { get; set; }

        /// <summary>
        /// Descricao da mensagem
        /// </summary>
        [XmlElement("xMsg")]
        public string XMsg { get; set; }
    }
}
