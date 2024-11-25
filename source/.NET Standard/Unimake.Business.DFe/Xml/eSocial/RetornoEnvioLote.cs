#pragma warning disable CS1591

using System;
using System.Xml.Serialization;
using System.Runtime.InteropServices;
using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.ESocial.Retorno
{
    /// <summary>
    /// Retorno do envio em lote de eventos do eSocial
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.RetornoEnvioLote")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/lote/eventos/envio/retornoEnvio/v1_1_0", IsNullable = true)]
    public class RetornoEnvioLote : XMLBase
    {
        /// <summary>
        /// Contém o retorno do envio em lote de eventos
        /// </summary>
        [XmlElement("retornoEnvioLoteEventos")]
        public RetornoEnvioLoteEventos RetornoEnvioLoteEventos { get; set; }
    }

    /// <summary>
    /// Contém o retorno do envio em lote de eventos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.RetornoEnvioLoteEventos")]
    [ComVisible(true)]
#endif
    public class RetornoEnvioLoteEventos
    {
        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do transmissor (certificado digital)
        /// </summary>
        [XmlElement("ideTransmissor")]
        public IdeTransmissor IdeTransmissor { get; set; }

        /// <summary>
        /// Indica o status do envio
        /// </summary>
        [XmlElement("status")]
        public Status Status { get; set; }

        /// <summary>
        /// Contém os dados relativos a recepção de um lote.
        /// </summary>
        [XmlElement("dadosRecepcaoLote")]
        public DadosRecepcaoLote DadosRecepcaoLote { get; set; }
    }
}
