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
using Unimake.Business.DFe.Xml.NFCom;

namespace Unimake.Business.DFe.Xml.NFCom
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.NFComProc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("nfcomProc", Namespace = "http://www.portalfiscal.inf.br/nfcom", IsNullable = false)]
    public class NFComProc : XMLBase
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

        [XmlElement("NFCom")]
        public NFCom NFCom { get; set; }

        [XmlElement("protNFCom")]
        public ProtNFCom ProtNFCom { get; set; }

        /// <summary>
        /// Nome do arquivo de distribuição
        /// </summary>
        [XmlIgnore]
        public string NomeArquivoDistribuicao
        {
            get
            {
                switch (ProtNFCom.InfProt.CStat)
                {
                    case 100: //Autorizado o Uso da NFCom
                    case 150: //Autorizado o Uso da NFCom, autorização fora de prazo
                    default:
                        return ProtNFCom.InfProt.ChNFCom + "-procNFCom.xml";
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
            var xmlElementNFCom = (XmlElement)xmlDocument.GetElementsByTagName("NFCom")[0];
            xmlElementNFCom.SetAttribute("xmlns", attribute.Namespace);
            var xmlElementProtNFCom = (XmlElement)xmlDocument.GetElementsByTagName("protNFCom")[0];
            xmlElementProtNFCom.SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        /// <summary>
        /// Desserializar o arquivo XML no objeto NFComProc
        /// </summary>
        /// <param name="filename">Localização do arquivo XML de distribuição do NFCom</param>
        /// <returns>Objeto do XML de distribuição do NFCom</returns>
        public NFComProc LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<NFComProc>(doc);
        }

        /// <summary>
        /// Desserializar a string do XML NFComProc no objeto NFComProc
        /// </summary>
        /// <param name="xml">string do XML NFComProc</param>
        /// <returns>Objeto da NFComProc</returns>
        public NFComProc LoadFromXML(string xml) => XMLUtility.Deserializar<NFComProc>(xml);

        #endregion Public Methods
    }
}
