#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.BPe
{
    /// <summary>
    /// Retorno do evento do BP-e
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.RetEventoBPe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("retEventoBPe", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class RetEventoBPe : XMLBase
    {
        /// <summary>
        /// Versao do leiaute
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Informacoes do evento
        /// </summary>
        [XmlElement("infEvento")]
        public InfRetEventoBPe InfEvento { get; set; }

        /// <summary>
        /// Assinatura digital
        /// </summary>
        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.InfRetEventoBPe")]
    [ComVisible(true)]
#endif
    public class InfRetEventoBPe
    {
        /// <summary>
        /// Identificador da tag
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
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
        /// Codigo do orgao de recepcao do evento
        /// </summary>
        [XmlElement("cOrgao")]
        public int COrgao { get; set; }

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

        /// <summary>
        /// Chave de acesso do BP-e
        /// </summary>
        [XmlElement("chBPe")]
        public string ChBPe { get; set; }

        /// <summary>
        /// Tipo do evento
        /// </summary>
        [XmlElement("tpEvento")]
        public TipoEventoBPe TpEvento { get; set; }

        /// <summary>
        /// Descricao do evento
        /// </summary>
        [XmlElement("xEvento")]
        public string XEvento { get; set; }

        /// <summary>
        /// Sequencial do evento
        /// </summary>
        [XmlElement("nSeqEvento")]
        public int NSeqEvento { get; set; }

        /// <summary>
        /// Data e hora do registro do evento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRegEvento { get; set; }
#else
        public DateTimeOffset DhRegEvento { get; set; }
#endif

        /// <summary>
        /// Data e hora do registro do evento serializada no XML
        /// </summary>
        [XmlElement("dhRegEvento")]
        public string DhRegEventoField
        {
            get => DhRegEvento.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRegEvento = DateTime.Parse(value);
#else
            set => DhRegEvento = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Numero do protocolo
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        public bool ShouldSerializeChBPe() => !string.IsNullOrEmpty(ChBPe);
        public bool ShouldSerializeTpEvento() => TpEvento != (TipoEventoBPe)0;
        public bool ShouldSerializeXEvento() => !string.IsNullOrEmpty(XEvento);
        public bool ShouldSerializeNSeqEvento() => NSeqEvento > 0;

#if INTEROP
        public bool ShouldSerializeDhRegEventoField() => DhRegEvento > DateTime.MinValue;
#else
        public bool ShouldSerializeDhRegEventoField() => DhRegEvento > DateTimeOffset.MinValue;
#endif

        public bool ShouldSerializeNProt() => !string.IsNullOrEmpty(NProt);
    }
}
