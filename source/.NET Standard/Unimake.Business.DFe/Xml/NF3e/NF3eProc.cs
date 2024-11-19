#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
# endif

using System;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NF3e
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3eProc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("nf3eProc", Namespace = "http://www.portalfiscal.inf.br/nf3e", IsNullable = false)]
    public class NF3eProc : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlAttribute(AttributeName = "ipTransmissor", DataType = "token")]
        public string IpTransmissor { get; set; }

        [XmlAttribute(AttributeName = "nPortaCon", DataType = "token")]
        public string NPortaCon { get; set; }

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

        [XmlElement("NF3e")]
        public NF3e NF3e { get; set; }

        [XmlElement("protNF3e")]
        public ProtNF3e ProtNF3e { get; set; }

        /// <summary>
        /// Nome do arquivo de distribuição
        /// </summary>
        [XmlIgnore]
        public string NomeArquivoDistribuicao
        {
            get
            {
                switch (ProtNF3e.InfProt.CStat)
                {
                    case 100: //Autorizado o Uso da NF3e
                    case 150: //Autorizado o Uso da NF3e, autorização fora de prazo
                    default:
                        return ProtNF3e.InfProt.ChNF3e + "-procnf3e.xml";
                }
            }
        }

        #region ShouldSerialize

        public bool ShouldSerializeIpTransmissor() => !string.IsNullOrEmpty(IpTransmissor);
        public bool ShouldSerializeNPortaCon() => !string.IsNullOrEmpty(NPortaCon);
        public bool ShouldSerializeDhConexaoField() => DhConexao > DateTime.MinValue;

        #endregion ShouldSerialize

        #region Public Methods

        /// <summary>
        /// Serializa o objeto (Converte o objeto para XML)
        /// </summary>
        /// <returns>Conteúdo do XML</returns>
        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();

            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
            var xmlElementNF3e = (XmlElement)xmlDocument.GetElementsByTagName("NF3e")[0];
            xmlElementNF3e.SetAttribute("xmlns", attribute.Namespace);
            var xmlElementProtNF3e = (XmlElement)xmlDocument.GetElementsByTagName("protNF3e")[0];
            xmlElementProtNF3e.SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        /// <summary>
        /// Desserializar o arquivo XML no objeto NF3eProc
        /// </summary>
        /// <param name="filename">Localização do arquivo XML de distribuição do NF3e</param>
        /// <returns>Objeto do XML de distribuição do NF3e</returns>
        public NF3eProc LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<NF3eProc>(doc);
        }

        /// <summary>
        /// Desserializar a string do XML NF3eProc no objeto NF3eProc
        /// </summary>
        /// <param name="xml">string do XML NF3eProc</param>
        /// <returns>Objeto da NF3eProc</returns>
        public NF3eProc LoadFromXML(string xml) => XMLUtility.Deserializar<NF3eProc>(xml);

        #endregion Public Methods
    }
}
