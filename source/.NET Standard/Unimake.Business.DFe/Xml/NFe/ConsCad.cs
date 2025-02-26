#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Classe da consulta de cadastro de contribuintes
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ConsCad")]
    [ComVisible(true)]
#endif
    [XmlRoot("ConsCad", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class ConsCad : ConsCadBase
    {
    }

    /// <summary>
    /// Classe da consulta de cadastro de contribuintes
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ConsCadBase")]
    [ComVisible(true)]
#endif
    public class ConsCadBase : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de consulta cadastro de contribuinte
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Dados do pedido de consulta de cadastro de contribuintes
        /// </summary>
        [XmlElement(ElementName = "infCons")]
        public InfCons InfCons = new InfCons();
    }

    /// <summary>
    /// Classe de dados do pedido de consulta de cadastro de contribuintes
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfCons")]
    [ComVisible(true)]
#endif
    public class InfCons
    {
        /// <summary>
        /// Serviço solicitado. Padrão = CONS-CAD
        /// </summary>
        [XmlElement("xServ")]
        public string XServ { get; set; } = "CONS-CAD";

        /// <summary>
        /// Sigla da UF consultada, utilizar SU para SUFRAMA
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// CNPJ do contribuinte
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do contribuinte
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Inscrição estadual do contribuinte
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);
        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        #endregion
    }
}