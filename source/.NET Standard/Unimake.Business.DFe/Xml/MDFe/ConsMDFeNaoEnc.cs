#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.MDFe
{
    /// <summary>
    /// Consultar MDFe Não Encerrados
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.ConsMDFeNaoEnc")]
    [ComVisible(true)]
#endif
    [XmlRoot("consMDFeNaoEnc", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class ConsMDFeNaoEnc : XMLBase
    {
        /// <summary>
        /// Versão do leiaute
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; } = "3.00";

        /// <summary>
        /// Tipo de Ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Serviço Solicitado
        /// </summary>
        [XmlElement("xServ")]
        public string XServ { get; set; } = "CONSULTAR NÃO ENCERRADOS";

        /// <summary>
        /// CNPJ do Emitente
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do Emitente
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }
    }
}
