#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.GNRE
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.TRetLoteGNRE")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("TRetLote_GNRE", Namespace = "http://www.gnre.pe.gov.br", IsNullable = false)]
    public class TRetLoteGNRE : XMLBase
    {
        [XmlElement("ambiente")]
        public TipoAmbiente Ambiente { get; set; }

        [XmlElement("situacaoRecepcao")]
        public SituacaoRecepcao SituacaoRecepcao { get; set; }

        [XmlElement("recibo")]
        public Recibo Recibo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.SituacaoRecepcao")]
    [ComVisible(true)]
#endif
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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Recibo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Recibo
    {
        [XmlElement("numero")]
        public string Numero { get; set; }

        [XmlIgnore]
#if INTEROP        
        public DateTime DataHoraRecibo { get; set; }
#else
        public DateTimeOffset DataHoraRecibo { get; set; }
#endif

        [XmlElement("dataHoraRecibo")]
        public string DataHoraReciboField
        {
            get => DataHoraRecibo.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DataHoraRecibo = DateTime.Parse(value);
#else
            set => DataHoraRecibo = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tempoEstimadoProc")]
        public int TempoEstimadoProc { get; set; }
    }
}
