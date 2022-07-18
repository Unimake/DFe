#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.CTe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.CteProc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("cteProc", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class CteProc : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("CTe")]
        public CTe CTe { get; set; }

        [XmlElement("protCTe")]
        public ProtCTe ProtCTe { get; set; }

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
                switch (ProtCTe.InfProt.CStat)
                {
                    case 110: //Uso Denegado
                    case 205: //NF-e está denegada na base de dados da SEFAZ [nRec:999999999999999]
                    case 301: //Uso Denegado: Irregularidade fiscal do emitente
                    case 302: //Uso Denegado: Irregularidade fiscal do destinatário
                    case 303: //Uso Denegado: Destinatário não habilitado a operar na UF
                        return ProtCTe.InfProt.ChCTe + "-den.xml";

                    case 100: //Autorizado o uso da NF-e
                    case 150: //Autorizado o uso da NF-e, autorização fora de prazo
                    default:
                        return ProtCTe.InfProt.ChCTe + "-proccte.xml";
                }
            }
        }

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();

            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
            var xmlElementCTe = (XmlElement)xmlDocument.GetElementsByTagName("CTe")[0];
            xmlElementCTe.SetAttribute("xmlns", attribute.Namespace);
            var xmlElementProtCTe = (XmlElement)xmlDocument.GetElementsByTagName("protCTe")[0];
            xmlElementProtCTe.SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        /// <summary>
        /// Deserializar o XML no objeto CteProc
        /// </summary>
        /// <param name="filename">Localização do arquivo XML de distribuição do CTe</param>
        /// <returns>Objeto do XML de distribuição do CTe</returns>
        public CteProc LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<CteProc>(doc);
        }

#region ShouldSerialize

        public bool ShouldSerializeIpTransmissor() => !string.IsNullOrWhiteSpace(IpTransmissor);
        public bool ShouldSerializeNPortaCon() => NPortaCon > 0;
        public bool ShouldSerializeDhConexaoField() => DhConexao > DateTime.MinValue;

#endregion
    }
}
