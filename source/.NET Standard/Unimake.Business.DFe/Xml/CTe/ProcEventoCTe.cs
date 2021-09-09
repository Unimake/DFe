#pragma warning disable CS1591

using System;
using System.IO;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.CTe
{
    [Serializable()]
    [XmlRoot("procEventoCTe", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class ProcEventoCTe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("eventoCTe")]
        public EventoCTe EventoCTe { get; set; }

        [XmlElement("retEventoCTe")]
        public RetEventoCTe RetEventoCTe { get; set; }

        [XmlAttribute("ipTransmissor")]
        public string IpTransmissor { get; set; }

        [XmlAttribute("nPortaCon")]
        public int NPortaCon { get; set; }

        [XmlIgnore]
        public DateTime DhConexao { get; set; }

        [XmlAttribute("dhConexao")]
        public string DhConexaoField
        {
            get => DhConexao.ToString("yyyy-MM-ddTHH:mm:sszzz");
            set => DhConexao = DateTime.Parse(value);
        }

        /// <summary>
        /// Nome do arquivo de distribuição
        /// </summary>
        [XmlIgnore]
        public string NomeArquivoDistribuicao => EventoCTe.InfEvento.ChCTe + "_" + ((int)EventoCTe.InfEvento.TpEvento).ToString("000000") + "_" + EventoCTe.InfEvento.NSeqEvento.ToString("00") + "-proceventocte.xml";

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();

            XmlRootAttribute attribute = GetType().GetCustomAttribute<XmlRootAttribute>();

            XmlElement xmlElementEvento = (XmlElement)xmlDocument.GetElementsByTagName("eventoCTe")[0];
            xmlElementEvento.SetAttribute("xmlns", attribute.Namespace);

            XmlElement xmlElementRetEvento = (XmlElement)xmlDocument.GetElementsByTagName("retEventoCTe")[0];
            xmlElementRetEvento.SetAttribute("xmlns", attribute.Namespace);

            XmlElement xmlElementRetEventoInfEvento = (XmlElement)xmlElementRetEvento.GetElementsByTagName("infEvento")[0];
            xmlElementRetEventoInfEvento.SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        public override void ReadXml(XmlDocument document)
        {
            base.ReadXml(document);

            var reader = XmlReader.Create(new StringReader(document.InnerXml));

            while(reader.Read())
            {
                if(reader.NodeType != XmlNodeType.Element)
                {
                    continue;
                }

                switch(reader.Name)
                {
                    case "Signature":
                        EventoCTe.Signature = reader.ToSignature();
                        break;

                    case "retEvento":
                        var versao = reader.GetAttribute("versao");
                        var infEvento = reader.DeserializeTo<RetEventoCTeInfEvento>();

                        RetEventoCTe = new RetEventoCTe
                        {
                            Versao = versao,
                            InfEvento = infEvento
                        };
                        break;
                }
            }
        }
    }
}
