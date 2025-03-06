#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.MDFe
{
    /// <summary>
    /// Consulta Situação MDFe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.ConsSitMDFe")]
    [ComVisible(true)]
#endif
    [XmlRoot("consSitMDFe", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class ConsSitMDFe : XMLBase
    {
        /// <summary>
        /// Versão do leiaute
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Tipo de Ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Serviço Solicitado
        /// </summary>
        [XmlElement("xServ")]
        public string XServ { get; set; } = "CONSULTAR";

        /// <summary>
        /// Chave de acesso do MDF-e
        /// </summary>
        [XmlElement("chMDFe")]
        public string ChMDFe { get; set; }
    }
}