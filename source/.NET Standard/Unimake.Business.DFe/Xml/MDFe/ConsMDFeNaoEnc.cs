#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.MDFe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.ConsMDFeNaoEnc")]
    [ComVisible(true)]
#endif
    [XmlRoot("consMDFeNaoEnc", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class ConsMDFeNaoEnc : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; } = "3.00";

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("xServ")]
        public string XServ { get; set; } = "CONSULTAR NÃO ENCERRADOS";

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }
    }
}
