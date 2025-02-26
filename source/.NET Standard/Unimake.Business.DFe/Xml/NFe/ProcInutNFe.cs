#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Classe do XML da inutilização com o protocolo de autorização
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ProcInutNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("procInutNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class ProcInutNFe: XMLBase
    {
        #region Public Fields

        /// <summary>
        /// Extensão final do arquivo de distribuição da inutilização
        /// </summary>
        public const string ExtensaoDoArquivo = "-procinutnfe.xml";

        #endregion Public Fields

        #region Public Properties

        /// <summary>
        /// Inutilização de números de notas fiscais (NFe/NFCe)
        /// </summary>
        [XmlElement("inutNFe", Order = 0)]
        public InutNFe InutNFe { get; set; }

        /// <summary>
        /// Nome do arquivo de distribuição
        /// </summary>
        [XmlIgnore]
        public string NomeArquivoDistribuicao => MontarNomeArquivo(InutNFe.InfInut.Id);

        /// <summary>
        /// Retorno da inutilização de número da NFe/NFCe
        /// </summary>
        [XmlElement("retInutNFe", Order = 1)]
        public RetInutNFe RetInutNFe { get; set; }

        /// <summary>
        /// Versão do schema do XML de distribuição da inutilização da NFe/NFCe
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        #endregion Public Properties

        #region Public Methods

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();

            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
            var xmlElementNFe = (XmlElement)xmlDocument.GetElementsByTagName("inutNFe")[0];
            xmlElementNFe.SetAttribute("xmlns", attribute.Namespace);
            var xmlElementProtNFe = (XmlElement)xmlDocument.GetElementsByTagName("retInutNFe")[0];
            xmlElementProtNFe.SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        public string MontarNomeArquivo(string id) => id.Substring(2, id.Length - 2) + ExtensaoDoArquivo;

        #endregion Public Methods
    }
}