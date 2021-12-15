#pragma warning disable CS1591

using System;
using System.Dynamic;
using System.Runtime.InteropServices;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.GNRE
{
    [ComVisible(true)]
    [Serializable()]
    [XmlRoot("TConsultaConfigUf", Namespace = "http://www.gnre.pe.gov.br", IsNullable = false)]
    public class TConsultaConfigUf: XMLBase
    {
        [XmlElement("ambiente")]
        public TipoAmbiente Ambiente { get; set; }

        [XmlElement("uf")]
        public UFBrasil UF { get; set; }

        [XmlElement("receita")]
        public Receita Receita { get; set; }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Receita
    {
        [XmlAttribute("courier")]
        public SimNaoLetra Courier { get; set; } = SimNaoLetra.Nao;

        [XmlText()]
        public Int32 Value { get; set; }
    }
}