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
    /// Classe da consulta da situação atual da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ConsSitNFe")]
    [ComVisible(true)]
#endif
    [XmlRoot("consSitNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class ConsSitNFe : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de consulta situação da NFe/NFCe
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Tipo do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Descrição do serviço. Padrão = CONSULTAR
        /// </summary>
        [XmlElement("xServ")]
        public string XServ { get; set; } = "CONSULTAR";

        /// <summary>
        /// Chave de acesso da NFe/NFCe que será consultada
        /// </summary>
        [XmlElement("chNFe")]
        public string ChNFe { get; set; }
    }
}