#pragma warning disable CS1591

using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe
{
    [XmlRoot("ConsCad", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class ConsCad : ConsCadBase
    {
    }

    public class ConsCadBase : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement(ElementName = "infCons")]
        public InfCons InfCons = new InfCons();
    }

    public class InfCons
    {
        [XmlElement("xServ")]
        public string XServ { get; set; } = "CONS-CAD";

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ()
        {
            return !string.IsNullOrWhiteSpace(CNPJ);
        }
        public bool ShouldSerializeCPF()
        {
            return !string.IsNullOrWhiteSpace(CPF);
        }
        public bool ShouldSerializeIE()
        {
            return !string.IsNullOrWhiteSpace(IE);
        }

        #endregion
    }
}