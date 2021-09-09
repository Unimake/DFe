#pragma warning disable CS1591

using System;
using System.IO;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFe
{
    [Serializable()]
    [XmlRoot("procEventoNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class ProcEventoNFe : XMLBase
    {
        #region Public Properties

        [XmlElement("evento", Order = 0, Namespace = "http://www.portalfiscal.inf.br/nfe")]
        public Evento Evento { get; set; }

        /// <summary>
        /// Nome do arquivo de distribuição
        /// </summary>
        [XmlIgnore]
        public string NomeArquivoDistribuicao => Evento.InfEvento.ChNFe + "_" + ((int)Evento.InfEvento.TpEvento).ToString("000000") + "_" + Evento.InfEvento.NSeqEvento.ToString("00") + "-proceventonfe.xml";

        [XmlElement("retEvento", Order = 1, Namespace = "http://www.portalfiscal.inf.br/nfe")]
        public RetEvento RetEvento { get; set; }

        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        #endregion Public Properties

        #region Public Methods

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();

            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();

            var xmlElementEvento = (XmlElement)xmlDocument.GetElementsByTagName("evento")[0];
            xmlElementEvento.SetAttribute("xmlns", attribute.Namespace);

            var xmlElementRetEvento = (XmlElement)xmlDocument.GetElementsByTagName("retEvento")[0];
            xmlElementRetEvento.SetAttribute("xmlns", attribute.Namespace);

            var xmlElementRetEventoInfEvento = (XmlElement)xmlElementRetEvento.GetElementsByTagName("infEvento")[0];
            xmlElementRetEventoInfEvento.SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        public override void ReadXml(XmlDocument document)
        {
            base.ReadXml(document);
            var reader = XmlReader.Create(new StringReader(document.InnerXml));

            while (reader.Read())
            {
                if(reader.NodeType != XmlNodeType.Element)
                {
                    continue;
                }

                switch(reader.Name)
                {
                    case "Signature":
                        Evento.Signature = reader.ToSignature();
                        break;

                    case "retEvento":
                        var versao = reader.GetAttribute("versao");
                        var infEvento = reader.DeserializeTo<InfEventoRetEvento>();

                        RetEvento = new RetEvento
                        {
                            Versao = versao,
                            InfEvento = infEvento
                        };
                        break;
                }
            }
        }

        #endregion Public Methods
    }
}