#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{
    /// <summary>
    /// Consulta de Pedido de Registro de Evento da NFS-e - Padrão NACIONAL
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.ConsPedRegEvento")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("consPedRegEvento", Namespace = "http://www.sped.fazenda.gov.br/nfse", IsNullable = false)]
    public class ConsPedRegEvento : XMLBase
    {
        /// <summary>
        /// Versão do schema XML
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Informações da consulta de pedido de registro de evento
        /// </summary>
        [XmlElement("infConsPedRegEvento")]
        public InfConsPedRegEvento InfConsPedRegEvento { get; set; }
    }

    /// <summary>
    /// Informações da consulta de pedido de registro de evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.InfConsPedRegEvento")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.sped.fazenda.gov.br/nfse")]
    public class InfConsPedRegEvento
    {
        /// <summary>
        /// Chave de acesso da NFS-e
        /// </summary>
        [XmlElement("chNFSe")]
        public string ChNFSe { get; set; }

        /// <summary>
        /// Tipo do evento (ex: 101101 para cancelamento)
        /// </summary>
        [XmlElement("tipoEvento")]
        public string TipoEvento { get; set; }

        /// <summary>
        /// Número sequencial do evento
        /// </summary>
        [XmlElement("numSeqEvento")]
        public string NumSeqEvento { get; set; }
    }
}
