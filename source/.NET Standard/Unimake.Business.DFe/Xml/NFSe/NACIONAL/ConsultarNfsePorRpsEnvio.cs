#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{
    /// <summary>
    /// Consulta de NFS-e por RPS (Padrão Nacional – DPS).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.ConsultarNfsePorRpsEnvio")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = "http://www.sped.fazenda.gov.br/nfse")]
    [XmlRoot("DPS", Namespace = "http://www.sped.fazenda.gov.br/nfse", IsNullable = false)]
    public class ConsultarNfsePorRpsEnvio : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de consulta por RPS (DPS).
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Informações do DPS a ser consultado.
        /// </summary>
        [XmlElement("infDPS")]
        public InfDPSConsulta InfDPS { get; set; } = new InfDPSConsulta();
    }

    /// <summary>
    /// Informações do DPS a ser consultado.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.InfDPSConsulta")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = "http://www.sped.fazenda.gov.br/nfse")]
    public class InfDPSConsulta
    {
        /// <summary>
        /// Id do DPS (fornecido externamente; não é calculado aqui).
        /// </summary>
        [XmlAttribute("Id", DataType = "token")]
        public string Id { get; set; }
    }
}
