#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
# endif

using System;
using System.Xml.Serialization;
using System.Xml;
using System.Reflection;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NF3e
{
    /// <summary>
    /// Pedido de Registro de Evento de NF3e processado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ProcEventoNF3e")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("procEventoNF3e", Namespace = "http://www.portalfiscal.inf.br/nf3e", IsNullable = false)]
    public class ProcEventoNF3e : XMLBase
    {
        #region Public Properties

        /// <summary>
        /// Versão do evento
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// IP do transmissor do documento fiscal para o ambiente autorizador
        /// </summary>
        [XmlAttribute(AttributeName = "ipTransmissor", DataType = "token")]
        public string IpTransmissor { get; set; }

        /// <summary>
        /// Porta de origem utilizada na conexão (De 0 a 65535)
        /// </summary>
        [XmlAttribute(AttributeName = "nPortaCon", DataType = "token")]
        public string NPortaCon { get; set; }

        /// <summary>
        /// Data e Hora da Conexão de Origem
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhConexao { get; set; }
#else
        public DateTimeOffset DhConexao { get; set; }
#endif

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
        /// Evento da NF3e
        /// </summary>
        [XmlElement("eventoNF3e", Order = 0, Namespace = "http://www.portalfiscal.inf.br/nf3e")]
        public EventoNF3e EventoNF3e { get; set; }

        /// <summary>
        /// Retorno do evento NF3e
        /// </summary>
        [XmlElement("retEventoNF3e", Order = 1, Namespace = "http://www.portalfiscal.inf.br/nf3e")]
        public RetEventoNF3e RetEventoNF3e { get; set; }

        /// <summary>
        /// Nome do arquivo de distribuição
        /// </summary>
        [XmlIgnore]
        public string NomeArquivoDistribuicao => EventoNF3e.InfEvento.ChNF3e + "_" + ((int)EventoNF3e.InfEvento.TpEvento).ToString("000000") + "_" + EventoNF3e.InfEvento.NSeqEvento + "-proceventonf3e.xml";

        #endregion Public Properties

        #region ShouldSerialize

        public bool ShouldSerializeIpTransmissor() => !string.IsNullOrEmpty(IpTransmissor);
        public bool ShouldSerializeNPortaCon() => !string.IsNullOrEmpty(NPortaCon);
        public bool ShouldSerializeDhConexaoField() => DhConexao > DateTime.MinValue;

        #endregion ShouldSerialize

        #region Public Methods

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();

            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();

            var xmlElementEvento = (XmlElement)xmlDocument.GetElementsByTagName("eventoNF3e")[0];
            xmlElementEvento.SetAttribute("xmlns", attribute.Namespace);

            var xmlElementRetEvento = (XmlElement)xmlDocument.GetElementsByTagName("retEventoNF3e")[0];
            xmlElementRetEvento.SetAttribute("xmlns", attribute.Namespace);

            var xmlElementRetEventoInfEvento = (XmlElement)xmlElementRetEvento.GetElementsByTagName("infEvento")[0];
            xmlElementRetEventoInfEvento.SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        public override void ReadXml(XmlDocument document)
        {
            var nodeListEvento = document.GetElementsByTagName("eventoNF3e");

            if (nodeListEvento.Count > 0)
            {
                EventoNF3e = XMLUtility.Deserializar<EventoNF3e>(((XmlElement)nodeListEvento[0]).OuterXml);
            }

            var nodeListRetEvento = document.GetElementsByTagName("retEventoNF3e");

            if (nodeListRetEvento.Count > 0)
            {
                RetEventoNF3e = XMLUtility.Deserializar<RetEventoNF3e>(((XmlElement)nodeListRetEvento[0]).OuterXml);
            }
        }

        #endregion Public Methods
    }
}