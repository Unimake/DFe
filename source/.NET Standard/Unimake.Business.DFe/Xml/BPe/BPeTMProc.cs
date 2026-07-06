#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Xml.BPe;

namespace Unimake.Business.DFe.Xml.BPeTM
{
    /// <summary>
    /// BP-e TM processado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTM.BPeTMProc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("bpeTMProc", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class BPeTMProc : XMLBase
    {
        /// <summary>
        /// Versao do leiaute
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// IP do transmissor
        /// </summary>
        [XmlAttribute("ipTransmissor")]
        public string IPTransmissor { get; set; }

        /// <summary>
        /// Porta de origem utilizada na conexao
        /// </summary>
        [XmlAttribute("nPortaCon")]
        public string NPortaCon { get; set; }

        /// <summary>
        /// Data e hora da conexao de origem
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhConexao { get; set; }
#else
        public DateTimeOffset? DhConexao { get; set; }
#endif

        /// <summary>
        /// Data e hora da conexao serializada no XML
        /// </summary>
        [XmlAttribute("dhConexao")]
        public string DhConexaoField
        {
#if INTEROP
            get => DhConexao.ToString("yyyy-MM-ddTHH:mm:sszzz");
            set => DhConexao = DateTime.Parse(value);
#else
            get => DhConexao.HasValue ? DhConexao.Value.ToString("yyyy-MM-ddTHH:mm:sszzz") : null;
            set => DhConexao = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// BP-e TM
        /// </summary>
        [XmlElement("BPeTM")]
        public BPeTM BPeTM { get; set; }

        /// <summary>
        /// Protocolo do BP-e
        /// </summary>
        [XmlElement("protBPe")]
        public ProtBPe ProtBPe { get; set; }

        /// <summary>
        /// Nome do arquivo de distribuicao
        /// </summary>
        [XmlIgnore]
        public string NomeArquivoDistribuicao => ProtBPe.InfProt.ChBPe + "-procBPeTM.xml";

        public bool ShouldSerializeIPTransmissor() => !string.IsNullOrWhiteSpace(IPTransmissor);
        public bool ShouldSerializeNPortaCon() => !string.IsNullOrWhiteSpace(NPortaCon);
#if INTEROP
        public bool ShouldSerializeDhConexaoField() => DhConexao > DateTime.MinValue;
#else
        public bool ShouldSerializeDhConexaoField() => DhConexao != null;
#endif
    }
}
