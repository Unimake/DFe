#pragma warning disable CS1591

using System;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.MDFe
{
    [Serializable()]
    [XmlRoot("mdfeProc", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class MdfeProc: XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("MDFe")]
        public MDFe MDFe { get; set; }

        [XmlElement("protMDFe")]
        public ProtMDFe ProtMDFe { get; set; }

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
        public string NomeArquivoDistribuicao
        {
            get
            {
                switch(ProtMDFe.InfProt.CStat)
                {
                    case 110: //Uso Denegado
                    case 205: //NF-e está denegada na base de dados da SEFAZ [nRec:999999999999999]
                    case 301: //Uso Denegado: Irregularidade fiscal do emitente
                    case 302: //Uso Denegado: Irregularidade fiscal do destinatário
                    case 303: //Uso Denegado: Destinatário não habilitado a operar na UF
                        return ProtMDFe.InfProt.ChMDFe + "-den.xml";

                    case 100: //Autorizado o uso da NF-e
                    case 150: //Autorizado o uso da NF-e, autorização fora de prazo
                    default:
                        return ProtMDFe.InfProt.ChMDFe + "-procmdfe.xml";
                }
            }
        }

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();

            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
            var xmlElementMDFe = (XmlElement)xmlDocument.GetElementsByTagName("MDFe")[0];
            xmlElementMDFe.SetAttribute("xmlns", attribute.Namespace);
            var xmlElementProtMDFe = (XmlElement)xmlDocument.GetElementsByTagName("protMDFe")[0];
            xmlElementProtMDFe.SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }
    }
}
