#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFGas
{
    /// <summary>
    /// NFGas processada
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.NFGasProc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfgas")]
    [XmlRoot("nfgasProc", Namespace = "http://www.portalfiscal.inf.br/nfgas", IsNullable = false)]
    public class NFGasProc : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlAttribute("ipTransmissor")]
        public string IPTransmissor { get; set; }

        [XmlAttribute("nPortaCon")]
        public string NPortaCon { get; set; }

        [XmlAttribute("dhConexao")]
        public string DhConexao { get; set; }

        [XmlElement("NFGas")]
        public NFGas NFGas { get; set; }

        [XmlElement("protNFGas")]
        public ProtNFGas ProtNFGas { get; set; }

        [XmlIgnore]
        public string NomeArquivoDistribuicao => ProtNFGas.InfProt.ChNFGas + "-procNFGas.xml";
    }
}
