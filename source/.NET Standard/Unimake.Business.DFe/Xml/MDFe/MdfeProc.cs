#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;
using System.Text;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.MDFe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.MdfeProc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("mdfeProc", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class MdfeProc : XMLBase
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
#if INTEROP
        public DateTime DhConexao { get; set; }
#else
        public DateTimeOffset DhConexao { get; set; }
#endif

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
        public string NomeArquivoDistribuicao
        {
            get
            {
                switch (ProtMDFe.InfProt.CStat)
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

        /// <summary>
        /// Deserializar o XML no objeto MdfeProc
        /// </summary>
        /// <param name="filename">Localização do arquivo XML de distribuição do MDFe</param>
        /// <returns>Objeto do XML de distribuição do MDFe</returns>
        public MdfeProc LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<MdfeProc>(doc);
        }
    }
}
