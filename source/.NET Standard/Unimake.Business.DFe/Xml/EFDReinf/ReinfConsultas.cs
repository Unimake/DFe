#pragma warning disable CS1591

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
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ReinfConsultas")]
    [ComVisible(true)]
#endif

    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/envioLoteEventos/v1_05_01", IsNullable = false)]
    public class ReinfConsultas : XMLBase
    {
        [XmlElement("ConsultaReciboEvento")]
        public ConsultaReciboEvento ConsultaReciboEvento { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ConsultaReciboEvento")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.reinf.esocial.gov.br/schemas/envioLoteEventos/v1_05_01")]
    public class ConsultaReciboEvento
    {
        [XmlElement("tipoEvento")]
        public string TipoEvento { get; set; }

        [XmlElement("tpInsc")]
#if INTEROP
        public TiposInscricao TpInsc { get; set; } = (TiposInscricao)(-1);
#else
        public TiposInscricao? TpInsc { get; set; }
#endif

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime PerApur { get; set; }
#else
        public DateTimeOffset PerApur { get; set; }
#endif

        [XmlElement("perApur")]
        public string PerApurField
        {
            get => PerApur.ToString("yyyy-MM");
#if INTEROP
            set => PerApur = DateTime.Parse(value);
#else
            set => PerApur = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DtApur { get; set; }
#else
        public DateTimeOffset DtApur { get; set; }
#endif

        [XmlElement("dtApur")]
        public string DtApurField
        {
            get => DtApur.ToString("yyyy-MM");
#if INTEROP
            set => DtApur = DateTime.Parse(value);
#else
            set => DtApur = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DtApuracao { get; set; }
#else
        public DateTimeOffset DtApuracao { get; set; }
#endif

        [XmlElement("dtApuracao")]
        public string DtApuracaoField
        {
            get => DtApuracao.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtApuracao = DateTime.Parse(value);
#else
            set => DtApuracao = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tpInscEstab")]
        public string TpInscEstab { get; set; }

        [XmlElement("nrInscEstabPrest")]
        public string NrInscEstabPrest { get; set; }

        [XmlElement("tpInscAdq")]
#if INTEROP
        public TipoInscricaoAdquirente TpInscAdq { get; set; } = (TipoInscricaoAdquirente)(-1);
#else
        public TipoInscricaoAdquirente? TpInscAdq { get; set; }
#endif

        [XmlElement("tpInscTomador")]
#if INTEROP
        public TipoInscricaoEstabelecimento TpInscTomador { get; set; } = (TipoInscricaoEstabelecimento)(-1);
#else
        public TipoInscricaoEstabelecimento? TpInscTomador { get; set; }
#endif

        [XmlElement("nrInscTomador")]
        public string NrInscTomador { get; set; }

        [XmlElement("cnpjPrestador")]
        public string CnpjPrestador { get; set; }

        [XmlElement("nrInscEstab")]
        public string NrInscEstab { get; set; }

        [XmlElement("nrInscAdq")]
        public string NrInscAdq { get; set; }

        [XmlElement("tpInscProd")]
#if INTEROP
        public TiposInscricao TpInscProd { get; set; } = (TiposInscricao)(-1);
#else
        public TiposInscricao? TpInscProd { get; set; }
#endif

        [XmlElement("nrInscProd")]
        public string NrInscProd { get; set; }

        [XmlElement("nrInscEstabelecimento")]
        public string NrInscEstabelecimento { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializePerApurField() => PerApur > DateTime.MinValue;

        public bool ShouldSerializeDtApurField() => DtApur > DateTime.MinValue;

        public bool ShouldSerializeDtApuracaoField() => DtApuracao > DateTime.MinValue;

        public bool ShouldSerializeTpInscEstabField() => !string.IsNullOrEmpty(TpInscEstab);

        public bool ShouldSerializeNrInscTomadorField() => !string.IsNullOrEmpty(NrInscTomador);

        public bool ShouldSerializeCnpjPrestadorField() => !string.IsNullOrEmpty(CnpjPrestador);

        public bool ShouldSerializeNrInscEstabField() => !string.IsNullOrEmpty(NrInscEstab);

        public bool ShouldSerializeNrInscProdField() => !string.IsNullOrEmpty(NrInscProd);

        public bool ShouldSerializeNrInscEstabelecimentoField() => !string.IsNullOrEmpty(NrInscEstabelecimento);

        public bool ShouldSerializeNrInscEstabPrestField() => !string.IsNullOrEmpty(NrInscEstabPrest);

        public bool ShouldSerializeNrInscAdqField() => !string.IsNullOrEmpty(NrInscAdq);

#if INTEROP
        public bool ShouldSerializeTpInscProd() => TpInscProd != (TiposInscricao)(-1);
#else
        public bool ShouldSerializeTpInscProd() => TpInscProd != null;
#endif

#if INTEROP
        public bool ShouldSerializeTpInscAdq() => TpInscAdq != (TipoInscricaoAdquirente)(-1);
#else
        public bool ShouldSerializeTpInscAdq()=> TpInscAdq != null;        
#endif

#if INTEROP
        public bool ShouldSerializeTpInscTomador() => TpInscTomador != (TipoInscricaoEstabelecimento)(-1);
#else
        public bool ShouldSerializeTpInscTomador() => TpInscTomador != null;
#endif
        #endregion
    }
}
