﻿#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.EFDReinf
{

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ReinfConsultaFechamento2099")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://sped.fazenda.gov.br/", IsNullable = false)]
    public class ReinfConsultaFechamento2099 : XMLBase
    {
        [XmlElement("ConsultaResultadoFechamento2099")]
        public ConsultaResultadoFechamento2099 ConsultaResultadoFechamento2099 { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ConsultaResultadoFechamento2099")]
    [ComVisible(true)]
#endif
    public class ConsultaResultadoFechamento2099
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("numeroProtocoloFechamento")]
        public string NumeroProtocoloFechamento { get; set; }
    }
}
