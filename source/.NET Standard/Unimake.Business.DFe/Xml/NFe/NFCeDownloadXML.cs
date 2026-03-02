using System;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Estrutura XML para download de NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.NFCeDownloadXML")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("nfceDownloadXML", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class NFCeDownloadXML : XMLBase
    {
        /// <summary>
        /// Versão do leiaute
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Tipo do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Chave de acesso da NFCe
        /// </summary>
        [XmlElement("chNFCe")]
        public string ChNFCe { get; set; }

        /// <summary>
        /// Deserializar a string do XML no objeto NFCeDownloadXML
        /// </summary>
        /// <param name="xml">String do XML</param>
        /// <returns>Objeto NFCeDownloadXML</returns>
        public NFCeDownloadXML LoadFromXML(string xml) => XMLUtility.Deserializar<NFCeDownloadXML>(xml);
    }
}
