#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.CTe
{
    /// <summary>
    /// Pedido de Registro de Eventos de CT-e processado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ProcEventoCTe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("procEventoCTe", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class ProcEventoCTe : XMLBase
    {
        /// <summary>
        /// Versão do leiaute do schema
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Evento do CT-e
        /// </summary>
        [XmlElement("eventoCTe")]
        public EventoCTe EventoCTe { get; set; }

        /// <summary>
        /// Retorno Evento do CT-e
        /// </summary>
        [XmlElement("retEventoCTe")]
        public RetEventoCTe RetEventoCTe { get; set; }

        /// <summary>
        /// Ip do Transmissor
        /// </summary>
        [XmlAttribute("ipTransmissor")]
        public string IpTransmissor { get; set; }

        /// <summary>
        /// Porta de origem utilizada na conexão (de 0 a. 65535).
        /// </summary>
        [XmlAttribute("nPortaCon")]
        public int NPortaCon { get; set; }

        /// <summary>
        /// Data e Hora da conexão
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhConexao { get; set; }
#else
        public DateTimeOffset DhConexao { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DhConexao para atribuir ou resgatar o valor)
        /// </summary>
        [XmlAttribute("dhConexao")]
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
        /// Nome do arquivo de distribuição
        /// </summary>
        [XmlIgnore]
        public string NomeArquivoDistribuicao => EventoCTe.InfEvento.ChCTe + "_" + ((int)EventoCTe.InfEvento.TpEvento).ToString("000000") + "_" + EventoCTe.InfEvento.NSeqEvento.ToString((EventoCTe.Versao.Equals("3.00") ? "00" : "000")) + "-proceventocte.xml";

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();

            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();

            var xmlElementEvento = (XmlElement)xmlDocument.GetElementsByTagName("eventoCTe")[0];
            xmlElementEvento.SetAttribute("xmlns", attribute.Namespace);

            var xmlElementRetEvento = (XmlElement)xmlDocument.GetElementsByTagName("retEventoCTe")[0];
            xmlElementRetEvento.SetAttribute("xmlns", attribute.Namespace);

            var xmlElementRetEventoInfEvento = (XmlElement)xmlElementRetEvento.GetElementsByTagName("infEvento")[0];
            xmlElementRetEventoInfEvento.SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        public override void ReadXml(XmlDocument document)
        {
            var nodeListEvento = document.GetElementsByTagName("eventoCTe");

            if (nodeListEvento != null)
            {
                EventoCTe = XMLUtility.Deserializar<EventoCTe>(((XmlElement)nodeListEvento[0]).OuterXml);
                var nodeListEventoSignature = ((XmlElement)nodeListEvento[0]).GetElementsByTagName("Signature");
                if (nodeListEventoSignature != null)
                {
                    if (nodeListEventoSignature.Count > 0)
                    {
                        var signature = ((XmlElement)nodeListEventoSignature[0]).OuterXml;

                        signature = signature.Replace("<Signature xmlns=\"http://www.portalfiscal.inf.br/cte\">", "<Signature xmlns=\"http://www.w3.org/2000/09/xmldsig#\">");

                        EventoCTe.Signature = XMLUtility.Deserializar<Signature>(signature);
                    }
                }
            }

            var nodeListRetEvento = document.GetElementsByTagName("retEventoCTe");
            if (nodeListRetEvento != null)
            {
                RetEventoCTe = XMLUtility.Deserializar<RetEventoCTe>(((XmlElement)nodeListRetEvento[0]).OuterXml);
            }
        }

        public bool ShouldSerializeDhConexaoField() => DhConexao > DateTime.MinValue;
        public bool ShouldSerializeNPortaCon() => NPortaCon > 0;
        public bool ShouldSerializeIpTransmissor() => !string.IsNullOrWhiteSpace(IpTransmissor);
    }
}
