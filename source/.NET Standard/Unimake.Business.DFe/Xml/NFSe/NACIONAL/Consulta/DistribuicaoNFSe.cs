#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta
{
    /// <summary>
    /// Consulta de NSU para a NFSe NACIONAL
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.DistribuicaoNFSe")]
    [ComVisible(true)]
#endif
    [XmlRoot("distribuicaoNFSe")]
    public class DistribuicaoNFSe : XMLBase
    {
        /// <summary>
        /// Número de Sequência Único (NSU) para consulta de distribuição
        /// </summary>
        [XmlElement("NSU")]
        public string NSU { get; set; }

        /// <summary>
        /// Tipo de NSU (ex: DISTRIBUICAO)
        /// </summary>
        [XmlElement("tipoNSU")]
        public string TipoNSU { get; set; }

        /// <summary>
        /// Indicador se é envio em lote
        /// </summary>
        [XmlElement("lote")]
        public string Lote { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeNSU() => !string.IsNullOrWhiteSpace(NSU);
        public bool ShouldSerializeTipoNSU() => !string.IsNullOrWhiteSpace(TipoNSU);
        public bool ShouldSerializeLote() => !string.IsNullOrWhiteSpace(Lote);
        #endregion ShouldSerialize
    }
}