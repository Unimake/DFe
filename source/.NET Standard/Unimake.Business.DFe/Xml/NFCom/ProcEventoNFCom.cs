#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
# endif

using System;
using System.Xml.Serialization;
using System.Xml;
using System.Reflection;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NF3e;

namespace Unimake.Business.DFe.Xml.NFCom
{
    /// <summary>
    /// Pedido de Registro de Evento de NFCom processado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ProcEventoNFCom")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("procEventoNFCom", Namespace = "http://www.portalfiscal.inf.br/nfcom", IsNullable = false)]
    public class ProcEventoNFCom : XMLBase
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
        /// Evento da NFCom
        /// </summary>
        [XmlElement("eventoNFCom", Order = 0, Namespace = "http://www.portalfiscal.inf.br/nfcom")]
        public EventoNFCom EventoNFCom { get; set; }

        /// <summary>
        /// Retorno do evento NFCom
        /// </summary>
        [XmlElement("retEventoNFCom", Order = 1, Namespace = "http://www.portalfiscal.inf.br/nfcom")]
        public RetEventoNFCom RetEventoNFCom { get; set; }

        /// <summary>
        /// Nome do arquivo de distribuição
        /// </summary>
        [XmlIgnore]
        public string NomeArquivoDistribuicao => EventoNFCom.InfEvento.ChNFCom + "_" + ((int)EventoNFCom.InfEvento.TpEvento).ToString("000000") + "_" + EventoNFCom.InfEvento.NSeqEvento + "-proceventoNFCom.xml";

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

            var xmlElementEvento = (XmlElement)xmlDocument.GetElementsByTagName("eventoNFCom")[0];
            xmlElementEvento.SetAttribute("xmlns", attribute.Namespace);

            var xmlElementRetEvento = (XmlElement)xmlDocument.GetElementsByTagName("retEventoNFCom")[0];
            xmlElementRetEvento.SetAttribute("xmlns", attribute.Namespace);

            var xmlElementRetEventoInfEvento = (XmlElement)xmlElementRetEvento.GetElementsByTagName("infEvento")[0];
            xmlElementRetEventoInfEvento.SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        public override void ReadXml(XmlDocument document)
        {
            var nodeListEvento = document.GetElementsByTagName("eventoNFCom");

            if (nodeListEvento.Count > 0)
            {
                EventoNFCom = XMLUtility.Deserializar<EventoNFCom>(((XmlElement)nodeListEvento[0]).OuterXml);
            }

            var nodeListRetEvento = document.GetElementsByTagName("retEventoNFCom");

            if (nodeListRetEvento.Count > 0)
            {
                RetEventoNFCom = XMLUtility.Deserializar<RetEventoNFCom>(((XmlElement)nodeListRetEvento[0]).OuterXml);
            }
        }

        #endregion Public Methods
    }
}