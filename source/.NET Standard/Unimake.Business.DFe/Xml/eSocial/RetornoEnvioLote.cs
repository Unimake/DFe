#pragma warning disable CS1591

using System;
using System.Xml.Serialization;
using System.Runtime.InteropServices;
using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.ESocial.Retorno
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.RetornoEnvioLote")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/lote/eventos/envio/retornoEnvio/v1_1_0", IsNullable = true)]
    public class RetornoEnvioLote : XMLBase
    {
        [XmlElement("retornoEnvioLoteEventos")]
        public RetornoEnvioLoteEventos RetornoEnvioLoteEventos { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.RetornoEnvioLoteEventos")]
    [ComVisible(true)]
#endif
    public class RetornoEnvioLoteEventos
    {
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideTransmissor")]
        public IdeTransmissor IdeTransmissor { get; set; }

        [XmlElement("status")]
        public Status Status { get; set; }

        [XmlElement("dadosRecepcaoLote")]
        public DadosRecepcaoLote DadosRecepcaoLote { get; set; }
    }
}
