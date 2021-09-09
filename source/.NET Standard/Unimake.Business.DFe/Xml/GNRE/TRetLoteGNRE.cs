#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.GNRE
{
    [ComVisible(true)]
    [Serializable()]
    [XmlRoot("TRetLote_GNRE", Namespace = "http://www.gnre.pe.gov.br", IsNullable = false)]
    public class TRetLoteGNRE: XMLBase
    {
        [XmlElement("ambiente")]
        public TipoAmbiente Ambiente { get; set; }

        [XmlElement("situacaoRecepcao")]
        public SituacaoRecepcao SituacaoRecepcao { get; set; }

        [XmlElement("recibo")]
        public Recibo Recibo { get; set; }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class SituacaoRecepcao
    {
        [XmlElement("codigo")]
        public string Codigo { get; set; }

        [XmlElement("descricao")]
        public string Descricao { get; set; }

        [XmlElement("guiaErro")]
        public int GuiaErro { get; set; }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Recibo
    {
        [XmlElement("numero")]
        public string Numero { get; set; }

        [XmlIgnore]
        public DateTime DataHoraRecibo { get; set; }

        [XmlElement("dataHoraRecibo")]
        public string DataHoraReciboField
        {
            get => DataHoraRecibo.ToString("yyyy-MM-ddTHH:mm:sszzz");
            set => DataHoraRecibo = DateTime.Parse(value);
        }

        [XmlElement("tempoEstimadoProc")]
        public int TempoEstimadoProc { get; set; }
    }
}
