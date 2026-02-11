#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta
{
    /// <summary>
    /// Consulta de NFS-e – Padrão NACIONAL (SPED)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.NFSe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = "http://www.sped.fazenda.gov.br/nfse")]
    [XmlRoot("NFSe", Namespace = "http://www.sped.fazenda.gov.br/nfse", IsNullable = false)]
    public class NFSe : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de consulta da NFS-e.
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Informações da NFS-e a ser consultada.
        /// </summary>
        [XmlElement("infNFSe")]
        public InfNFSe InfNFSe { get; set; } = new InfNFSe();
    }

    /// <summary>
    /// Informações da NFS-e a ser consultada.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.InfNFSe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = "http://www.sped.fazenda.gov.br/nfse")]
    public class InfNFSe
    {
        /// <summary>
        /// ID da NFS-e a ser consultada. Fornecido externamente (não é calculado aqui).
        /// </summary>
        [XmlAttribute("Id", DataType = "token")]
        public string Id { get; set; }
    }
}
