#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFSe.Nacional
{

    /// <summary>
    /// Classe da consulta da NFS-e de padrão NACIONAL;
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.Nacional.ConsultarNfseEnvio")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("NFSe", Namespace = "http://www.sped.fazenda.gov.br/nfse", IsNullable = false)]
    public class ConsultarNfseEnvio : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de consulta da NFS-e
        /// </summary>
        [XmlAttribute(AttributeName = "versao")]
        public string Versao { get; set; }

        /// <summary>
        /// Informações da NFS-e a ser consultada
        /// </summary>
        [XmlElement("infNFSe")]
        public InfNFSeConsulta InfNFSe {  get; set; } = new InfNFSeConsulta();
    }

    /// <summary>
    /// Informações da NFS-e a ser consultada
    /// </summary>
    [Serializable]
    public class InfNFSeConsulta
    {

        /// <summary>
        /// ID da NFS-e a ser consultada
        /// </summary>
        [XmlAttribute("Id")]
        public string Id { get; set; }
    }
}
