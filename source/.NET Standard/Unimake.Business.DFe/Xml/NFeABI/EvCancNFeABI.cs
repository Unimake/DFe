#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFeABI
{
    /// <summary>Detalhe do evento de cancelamento da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.EvCancNFeABI")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("evCancNFeABI", Namespace = "http://www.portalfiscal.inf.br/nfeabi", IsNullable = false)]
    public class EvCancNFeABI : XMLBase
    {
        /// <summary>Versão do leiaute do detalhe.</summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>Descrição fixa do evento.</summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Cancelamento";

        /// <summary>Protocolo de autorização da NFeABI.</summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>Justificativa do cancelamento.</summary>
        [XmlElement("xJust")]
        public string XJust { get; set; }
    }
}
