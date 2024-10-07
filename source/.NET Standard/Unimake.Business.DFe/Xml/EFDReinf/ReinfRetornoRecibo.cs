#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ReinfRetornoRecibo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/retornoRecibosChaveEvento/v1_05_01", IsNullable = false)]
    public class ReinfRetornoRecibo : XMLBase
    {

        [XmlElement("ideStatus")]
        public IdeStatusRetorno IdeStatus { get; set; }

        [XmlElement("regOcorrs")]
        public List<RegOcorrsRetorno> RegOcorrs { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRegOcorrs(RegOcorrsRetorno item)
        {
            if (RegOcorrs == null)
            {
                RegOcorrs = new List<RegOcorrsRetorno>();
            }

            RegOcorrs.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RegOcorrs (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RegOcorrs</returns>
        public RegOcorrsRetorno GetRegOcorrs(int index)
        {
            if ((RegOcorrs?.Count ?? 0) == 0)
            {
                return default;
            };

            return RegOcorrs[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RegOcorrs
        /// </summary>
        public int GetRegOcorrsCount => (RegOcorrs != null ? RegOcorrs.Count : 0);
#endif

        [XmlElement("retornoEventos")]
        public RetornoEventos RetornoEventos { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeStatusRetorno")]
    [ComVisible(true)]
#endif
    public class IdeStatusRetorno
    {
        [XmlElement("cdRetorno")]
        public string CdRetorno { get; set; }

        [XmlElement("descRetorno")]
        public string DescRetorno { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RegOcorrsRetorno")]
    [ComVisible(true)]
#endif
    public class RegOcorrsRetorno
    {
        [XmlElement("tpOcorr")]
        public TipoDaOcorrencia TpOcorr { get; set; }

        [XmlElement("localErroAviso")]
        public string LocalErroAviso { get; set; }

        [XmlElement("codResp")]
        public string CodResp { get; set; }

        [XmlElement("dscResp")]
        public string DscResp { get; set; }

        #region ShouldSerialize
        public bool ShouldSereializeCodResp() => !string.IsNullOrEmpty(CodResp);
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RetornoEventos")]
    [ComVisible(true)]
#endif
    public class RetornoEventos
    {
        [XmlElement("evento")]
        public List<ReinfEventoRecibo> Evento { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddEvento(ReinfEventoRecibo item)
        {
            if (Evento == null)
            {
                Evento = new List<ReinfEventoRecibo>();
            }

            Evento.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Evento (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Evento</returns>
        public ReinfEventoRecibo GetEvento(int index)
        {
            if ((Evento?.Count ?? 0) == 0)
            {
                return default;
            };

            return Evento[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Evento
        /// </summary>
        public int GetEventoCount => (Evento != null ? Evento.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ReinfEventoRecibo")]
    [ComVisible(true)]
#endif
    public class ReinfEventoRecibo
    {
        [XmlAttribute(AttributeName = "id", DataType = "token")]
        public string ID { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtHoraRecebimento { get; set; }
#else
        public DateTimeOffset DtHoraRecebimento { get; set; }
#endif

        [XmlElement("dtHoraRecebimento")]
        public string DtHoraRecebimentoField
        {
            get => DtHoraRecebimento.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DtHoraRecebimento = DateTime.Parse(value);
#else
            set => DtHoraRecebimento = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("nrProtocolo")]
        public string NrProtocolo { get; set; }

        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

        [XmlElement("situacaoEvento")]
#if INTEROP
        public ReinfSituacaoEvento SituacaoEvento { get; set; } = (ReinfSituacaoEvento)(-1);
#else
        public ReinfSituacaoEvento? SituacaoEvento { get; set; }
#endif

        [XmlElement("aplicacaoRecepcao")]
#if INTEROP
        public ReinfAplicacaoRecepcao AplicacaoRecepcao { get; set; } = (ReinfAplicacaoRecepcao)(-1);
#else
        public ReinfAplicacaoRecepcao? AplicacaoRecepcao { get; set; }
#endif

        [XmlElement("tpProc")]
#if INTEROP
        public TipoProcesso TpProc { get; set; } = (TipoProcesso)(-1);
#else
        public TipoProcesso? TpProc { get; set; }
#endif

        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime IniValid { get; set; }
#else
        public DateTimeOffset IniValid { get; set; }
#endif

        [XmlElement("iniValid")]
        public string IniValidField
        {
            get => IniValid.ToString("yyyy-MM");
#if INTEROP
            set => IniValid = DateTime.Parse(value);
#else
            set => IniValid = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime FimValid { get; set; }
#else
        public DateTimeOffset FimValid { get; set; }
#endif

        [XmlElement("fimValid")]
        public string FimValidField
        {
            get => FimValid.ToString("yyyy-MM");
#if INTEROP
            set => FimValid = DateTime.Parse(value);
#else
            set => FimValid = DateTimeOffset.Parse(value);
#endif
        }

        #region ShouldSerialize

        public bool ShouldSerializeDtHoraRecebimentoField() => DtHoraRecebimento > DateTime.MinValue;

        public bool ShouldSereializeNrProtocoloField() => !string.IsNullOrEmpty(NrProtocolo);

        public bool ShouldSereializeNrReciboField() => !string.IsNullOrEmpty(NrRecibo);

        public bool ShouldSereializeNrProcField() => !string.IsNullOrEmpty(NrProc);

        public bool ShouldSerializeIniValidField() => IniValid > DateTime.MinValue;

        public bool ShouldSerializeFimValidField() => FimValid > DateTime.MinValue;

        #endregion

    }
}
