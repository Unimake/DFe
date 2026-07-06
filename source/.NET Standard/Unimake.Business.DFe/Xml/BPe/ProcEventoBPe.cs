#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.BPe
{
    /// <summary>
    /// Evento do BP-e processado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.ProcEventoBPe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("procEventoBPe", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class ProcEventoBPe : XMLBase
    {
        /// <summary>
        /// Versao do evento
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// IP do transmissor
        /// </summary>
        [XmlAttribute(AttributeName = "ipTransmissor", DataType = "token")]
        public string IPTransmissor { get; set; }

        /// <summary>
        /// Porta de origem da conexao
        /// </summary>
        [XmlAttribute(AttributeName = "nPortaCon", DataType = "token")]
        public string NPortaCon { get; set; }

        /// <summary>
        /// Data e hora da conexao
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhConexao { get; set; }
#else
        public DateTimeOffset DhConexao { get; set; }
#endif

        /// <summary>
        /// Data e hora da conexao serializada no XML
        /// </summary>
        [XmlAttribute(AttributeName = "dhConexao", DataType = "token")]
        public string DhConexaoField
        {
            get => DhConexao.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhConexao = DateTime.Parse(value);
#else
            set => DhConexao = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Evento do BP-e
        /// </summary>
        [XmlElement("eventoBPe", Order = 0, Namespace = "http://www.portalfiscal.inf.br/bpe")]
        public EventoBPe EventoBPe { get; set; }

        /// <summary>
        /// Retorno do evento do BP-e
        /// </summary>
        [XmlElement("retEventoBPe", Order = 1, Namespace = "http://www.portalfiscal.inf.br/bpe")]
        public RetEventoBPe RetEventoBPe { get; set; }

        /// <summary>
        /// Nome do arquivo de distribuicao
        /// </summary>
        [XmlIgnore]
        public string NomeArquivoDistribuicao => EventoBPe.InfEvento.ChBPe + "_" + ((int)EventoBPe.InfEvento.TpEvento).ToString("000000") + "_" + EventoBPe.InfEvento.NSeqEvento + "-proceventoBPe.xml";

        public bool ShouldSerializeIPTransmissor() => !string.IsNullOrEmpty(IPTransmissor);
        public bool ShouldSerializeNPortaCon() => !string.IsNullOrEmpty(NPortaCon);

#if INTEROP
        public bool ShouldSerializeDhConexaoField() => DhConexao > DateTime.MinValue;
#else
        public bool ShouldSerializeDhConexaoField() => DhConexao > DateTimeOffset.MinValue;
#endif

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();
            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();

            var xmlElementEvento = (XmlElement)xmlDocument.GetElementsByTagName("eventoBPe")[0];
            xmlElementEvento.SetAttribute("xmlns", attribute.Namespace);

            var xmlElementRetEvento = (XmlElement)xmlDocument.GetElementsByTagName("retEventoBPe")[0];
            xmlElementRetEvento.SetAttribute("xmlns", attribute.Namespace);

            var xmlElementRetEventoInfEvento = (XmlElement)xmlElementRetEvento.GetElementsByTagName("infEvento")[0];
            xmlElementRetEventoInfEvento.SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        public override void ReadXml(XmlDocument document)
        {
            var nodeListEvento = document.GetElementsByTagName("eventoBPe");

            if (nodeListEvento.Count > 0)
            {
                EventoBPe = XMLUtility.Deserializar<EventoBPe>(((XmlElement)nodeListEvento[0]).OuterXml);
            }

            var nodeListRetEvento = document.GetElementsByTagName("retEventoBPe");

            if (nodeListRetEvento.Count > 0)
            {
                RetEventoBPe = XMLUtility.Deserializar<RetEventoBPe>(((XmlElement)nodeListRetEvento[0]).OuterXml);
            }
        }
    }
}
