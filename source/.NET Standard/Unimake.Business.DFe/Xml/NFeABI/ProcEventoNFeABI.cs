#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Globalization;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFeABI
{
    /// <summary>Evento da NFeABI e seu respectivo protocolo de processamento.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.ProcEventoNFeABI")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("procEventoNFeABI", Namespace = "http://www.portalfiscal.inf.br/nfeabi", IsNullable = false)]
    public class ProcEventoNFeABI : XMLBase
    {
        /// <summary>Versão do leiaute processado.</summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>IP do transmissor, quando informado pelo autorizador.</summary>
        [XmlAttribute("ipTransmissor")]
        public string IPTransmissor { get; set; }

        /// <summary>Porta de conexão, quando informada pelo autorizador.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public int? NPortaCon { get; set; }

        /// <summary>Campo auxiliar para serialização de nPortaCon.</summary>
        [XmlAttribute("nPortaCon")]
        public string NPortaConField { get => NPortaCon?.ToString(CultureInfo.InvariantCulture); set => NPortaCon = string.IsNullOrWhiteSpace(value) ? (int?)null : int.Parse(value, CultureInfo.InvariantCulture); }

        /// <summary>Data e hora da conexão, quando informada pelo autorizador.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
        public DateTime? DhConexao { get; set; }
#else
        public DateTimeOffset? DhConexao { get; set; }
#endif

        /// <summary>Campo auxiliar para serialização de dhConexao.</summary>
        [XmlAttribute("dhConexao")]
        public string DhConexaoField
        {
            get => DhConexao.HasValue ? DhConexao.Value.ToString("yyyy-MM-ddTHH:mm:sszzz") : null;
#if INTEROP
            set => DhConexao = string.IsNullOrWhiteSpace(value) ? (DateTime?)null : DateTime.Parse(value, CultureInfo.InvariantCulture);
#else
            set => DhConexao = string.IsNullOrWhiteSpace(value) ? (DateTimeOffset?)null : DateTimeOffset.Parse(value, CultureInfo.InvariantCulture);
#endif
        }

        /// <summary>Evento originalmente transmitido.</summary>
        [XmlElement("eventoNFeABI")]
        public EventoNFeABI EventoNFeABI { get; set; }

        /// <summary>Retorno do processamento do evento.</summary>
        [XmlElement("retEventoNFeABI")]
        public RetEventoNFeABI RetEventoNFeABI { get; set; }

        /// <summary>Indica se nPortaCon deve ser serializado.</summary>
        public bool ShouldSerializeNPortaConField() => NPortaCon.HasValue;

        /// <summary>Indica se dhConexao deve ser serializado.</summary>
        public bool ShouldSerializeDhConexaoField() => DhConexao.HasValue;

        /// <summary>Nome convencional do arquivo de distribuição do evento.</summary>
        [XmlIgnore]
        public string NomeArquivoDistribuicao => EventoNFeABI.InfEvento.ChNFeABI + "_" + ((int)EventoNFeABI.InfEvento.TpEvento).ToString("000000") + "_" + EventoNFeABI.InfEvento.NSeqEvento + "-procEventoNFeABI.xml";
    }
}
