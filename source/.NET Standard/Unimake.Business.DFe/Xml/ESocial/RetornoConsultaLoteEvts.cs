#pragma warning disable CS1591

using System;
using System.Xml.Serialization;
using System.Runtime.InteropServices;
using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RetornoConsultaLoteEvts")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/lote/eventos/envio/retornoEnvio/v1_1_0", IsNullable = true)]
    public class RetornoConsultaLoteEvts : XMLBase
    {
        //[XmlElement("RetornoConsultaLoteEvtsEventos")]
        //public RetornoConsultaLoteEvtsEventos RetornoConsultaLoteEvtsEventos { get; set; }
    }
}