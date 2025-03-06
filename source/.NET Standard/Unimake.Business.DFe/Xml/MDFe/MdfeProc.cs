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
    /// <summary>
    /// MDFe Processado + Protocolo de autorização
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.MdfeProc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("mdfeProc", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class MdfeProc : XMLBase
    {
        /// <summary>
        /// Versão do leiaute.
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Manifesto Eletrônico de Documentos Fiscais (MDFe).
        /// </summary>
        [XmlElement("MDFe")]
        public MDFe MDFe { get; set; }

        /// <summary>
        /// Protocolo de autorização do MDFe.
        /// </summary>
        [XmlElement("protMDFe")]
        public ProtMDFe ProtMDFe { get; set; }

        /// <summary>
        /// Endereço IP do transmissor.
        /// </summary>
        [XmlAttribute("ipTransmissor")]
        public string IpTransmissor { get; set; }

        /// <summary>
        /// Número da porta de conexão.
        /// </summary>
        [XmlAttribute("nPortaCon")]
        public int NPortaCon { get; set; }

        /// <summary>
        /// Data e hora da conexão.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhConexao { get; set; }
#else
        public DateTimeOffset DhConexao { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhConexao" para atribuir ou resgatar o valor)
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
        /// Nome do arquivo de distribuição.
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

        /// <summary>
        /// Gera o XML do objeto MdfeProc.
        /// </summary>
        /// <returns>XmlDocument contendo o XML gerado.</returns>
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
        /// Deserializar o XML no objeto MdfeProc.
        /// </summary>
        /// <param name="filename">Localização do arquivo XML de distribuição do MDFe</param>
        /// <returns>Objeto do XML de distribuição do MDFe</returns>
        public MdfeProc LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<MdfeProc>(doc);
        }

        /// <summary>
        /// Desserializar a string do XML MdfeProc no objeto MdfeProc
        /// </summary>
        /// <param name="xml">string do XML MdfeProc</param>
        /// <returns>Objeto da MdfeProc</returns>
        public MdfeProc LoadFromXML(string xml) => XMLUtility.Deserializar<MdfeProc>(xml);
    }
}
