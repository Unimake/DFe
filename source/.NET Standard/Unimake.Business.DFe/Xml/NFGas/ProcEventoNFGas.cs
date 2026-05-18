#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFGas
{
    /// <summary>
    /// Evento da NFGas processado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.ProcEventoNFGas")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfgas")]
    [XmlRoot("procEventoNFGas", Namespace = "http://www.portalfiscal.inf.br/nfgas", IsNullable = false)]
    public class ProcEventoNFGas : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlAttribute("ipTransmissor")]
        public string IPTransmissor { get; set; }

        [XmlAttribute("nPortaCon")]
        public string NPortaCon { get; set; }

        [XmlAttribute("dhConexao")]
        public string DhConexao { get; set; }

        [XmlElement("eventoNFGas")]
        public EventoNFGas EventoNFGas { get; set; }

        [XmlElement("retEventoNFGas")]
        public RetEventoNFGas RetEventoNFGas { get; set; }

        [XmlIgnore]
        public string NomeArquivoDistribuicao => EventoNFGas.InfEvento.ChNFGas + "_" + ((int)EventoNFGas.InfEvento.TpEvento).ToString("000000") + "_" + EventoNFGas.InfEvento.NSeqEvento + "-proceventoNFGas.xml";
    }
}
