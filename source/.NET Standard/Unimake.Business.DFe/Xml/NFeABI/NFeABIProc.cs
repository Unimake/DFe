#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFeABI
{
/// <summary>Representa o grupo NFeABIProc do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.NFeABIProc")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("nfeabiProc", Namespace = "http://www.portalfiscal.inf.br/nfeabi", IsNullable = false)]
    public class NFeABIProc : XMLBase
    {
        /// <summary>Obtém ou define o atributo versao da NFeABI.</summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }
        /// <summary>Obtém ou define o atributo ipTransmissor da NFeABI.</summary>
        [XmlAttribute("ipTransmissor")]
        public string IPTransmissor { get; set; }
        /// <summary>Obtém ou define o atributo nPortaCon da NFeABI.</summary>
        [XmlAttribute("nPortaCon")]
        public string NPortaCon { get; set; }
        /// <summary>Obtém ou define o atributo dhConexao da NFeABI.</summary>
        [XmlAttribute("dhConexao")]
        public string DhConexao { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag NFeABI da NFeABI.</summary>
        [XmlElement("NFeABI")]
        public NFeABI NFeABI { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag protNFeABI da NFeABI.</summary>
        [XmlElement("protNFeABI")]
        public ProtNFeABI ProtNFeABI { get; set; }

        /// <summary>Obtém o nome do arquivo de distribuição da NFeABI processada.</summary>
        [XmlIgnore]
        public string NomeArquivoDistribuicao => ProtNFeABI.InfProt.ChNFeABI + "-procNFeABI.xml";
    }
}
