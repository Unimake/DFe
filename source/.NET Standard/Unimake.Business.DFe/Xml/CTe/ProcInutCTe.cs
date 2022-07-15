#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.CTe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ProcInutCTe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("procInutCTe", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class ProcInutCTe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("inutCTe")]
        public InutCTe InutCTe { get; set; }

        [XmlElement("retInutCTe")]
        public RetInutCTe RetInutCTe { get; set; }

        [XmlAttribute("ipTransmissor")]
        public string IpTransmissor { get; set; }

        [XmlAttribute("nPortaCon")]
        public int NPortaCon { get; set; }

        [XmlIgnore]
        public DateTimeOffset DhConexao { get; set; }

        [XmlAttribute("dhConexao")]
        public string DhConexaoField
        {
            get => DhConexao.ToString("yyyy-MM-ddTHH:mm:sszzz");
            set => DhConexao = DateTimeOffset.Parse(value);
        }

        /// <summary>
        /// Nome do arquivo de distribuição
        /// </summary>
        [XmlIgnore]
        public string NomeArquivoDistribuicao => InutCTe.InfInut.Id.Substring(2, InutCTe.InfInut.Id.Length - 2) + "-procinutcte.xml";

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();

            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
            var xmlElementCTe = (XmlElement)xmlDocument.GetElementsByTagName("inutCTe")[0];
            xmlElementCTe.SetAttribute("xmlns", attribute.Namespace);
            var xmlElementProtCTe = (XmlElement)xmlDocument.GetElementsByTagName("retInutCTe")[0];
            xmlElementProtCTe.SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }
    }
}
