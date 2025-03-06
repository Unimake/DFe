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
    /// Consulta Status Serviço MFDe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.ConsStatServMDFe")]
    [ComVisible(true)]
#endif
    [XmlRoot("consStatServMDFe", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class ConsStatServMDFe : XMLBase
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
        public string XServ { get; set; } = "STATUS";
    }
}