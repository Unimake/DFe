#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{
    /// <summary>
    /// Classe da consulta da NFS-e por RPS de padrão NACIONAL;
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.Nacional.ConsultarNfsePorRpsEnvio")]
    [ComVisible(true)]
#endif 
    [Serializable]
    [XmlRoot("DPS", Namespace = "http://www.sped.fazenda.gov.br/nfse", IsNullable = false)]
    public class ConsultarNfsePorRpsEnvio : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de consulta da NFS-e
        /// </summary>
        [XmlAttribute(AttributeName = "versao")]
        public string Versao { get; set; }

        /// <summary>
        /// Informações da NFS-e a ser consultada
        /// </summary>
        [XmlElement("infDPS")]
        public InfDPSConsulta InfDPS { get; set; } = new InfDPSConsulta();
    }

    /// <summary>
    /// Informações da NFS-e a ser consultada
    /// </summary>
    [Serializable]
    public class InfDPSConsulta
    {
        /// <summary>
        /// ID da NFS-e a ser consultada
        /// </summary>
        [XmlAttribute("Id")]
        public string Id { get; set; }
    }

}
