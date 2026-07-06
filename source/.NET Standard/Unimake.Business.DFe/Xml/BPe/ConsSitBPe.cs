#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.BPe
{
    /// <summary>
    /// Consulta situacao do BP-e
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.ConsSitBPe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("consSitBPe", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class ConsSitBPe : XMLBase
    {
        /// <summary>
        /// Versao do leiaute
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Identificacao do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Servico solicitado
        /// </summary>
        [XmlElement("xServ")]
        public string XServ { get; set; } = "CONSULTAR";

        /// <summary>
        /// Chave de acesso do BP-e
        /// </summary>
        [XmlElement("chBPe")]
        public string ChBPe { get; set; }
    }
}
