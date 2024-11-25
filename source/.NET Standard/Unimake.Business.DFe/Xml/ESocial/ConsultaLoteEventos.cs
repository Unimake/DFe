#pragma warning disable CS1591
using System;
using System.Xml;
using System.Xml.Serialization;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// Consultar o protocolo de um lote de eventos do eSocial
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultarLoteEventos")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/lote/eventos/envio/consulta/retornoProcessamento/v1_0_0", IsNullable = false)]
    public class ConsultarLoteEventos : XMLBase
    {
        /// <summary>
        /// Contém os parâmetros da consulta ao lote de eventos.
        /// </summary>
        [XmlElement("consultaLoteEventos")]
        public ConsultaLoteEventos ConsultaLoteEventos { get; set; }

        /// <summary>
        /// Versão da consulta
        /// </summary>
        [XmlIgnore]
        public string Versao { get; set; } = "1.1.0";
    }

    /// <summary>
    /// Contém os parâmetros da consulta ao lote de eventos.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultaLoteEventos")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.esocial.gov.br/schema/lote/eventos/envio/consulta/retornoProcessamento/v1_0_0")]
    public class ConsultaLoteEventos
    {
        /// <summary>
        /// Número sequencial único retornado pelo eSocial instante de recepção do lote de eventos.
        /// </summary>
        [XmlElement("protocoloEnvio")]
        public string ProtocoloEnvio { get; set; }
    }
}