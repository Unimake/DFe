#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
    /// <summary>
    /// R-3010 - Receita de espetáculos desportivos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf3010")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtEspDesportivo/v2_01_02", IsNullable = false)]
    public class Reinf3010 : XMLBase
    {
        /// <summary>
        /// Evento espetáculo desportivo
        /// </summary>
        [XmlElement("evtEspDesportivo")]
        public EvtEspDesportivo EvtEspDesportivo { get; set; }
    }

    /// <summary>
    /// Evento espetáculo desportivo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtEspDesportivo")]
    [ComVisible(true)]
#endif
    public class EvtEspDesportivo : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEvento3010 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri3010 IdeContri { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEvento3010")]
    [ComVisible(true)]
#endif
    public class IdeEvento3010
    {
        [XmlElement("indRetif")]
        public IndicativoRetificacao IndRetif { get; set; }

        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

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

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("procEmi")]
        public ProcessoEmissaoReinf ProcEmi { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNrRecibo() => !string.IsNullOrEmpty(NrRecibo);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeContri3010")]
    [ComVisible(true)]
#endif
    public class IdeContri3010 : IdeContri
    {
        [XmlElement("ideEstab")]
        public IdeEstab3010 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstab3010")]
    [ComVisible(true)]
#endif
    public class IdeEstab3010
    {
        [XmlElement("tpInscEstab")]
        public TipoInscricaoEstabelecimento TpInscEstab { get; set; }

        [XmlElement("nrInscEstab")]
        public string NrInscEstab { get; set; }

        /// <summary>
        /// Boletim do espetáculo desportivo
        /// </summary>
        [XmlElement("boletim")]
        public List<Boletim> Boletim { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBoletim(Boletim item)
        {
            if (Boletim == null)
            {
                Boletim = new List<Boletim>();
            }

            Boletim.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Boletim (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Boletim</returns>
        public Boletim GetBoletim(int index)
        {
            if ((Boletim?.Count ?? 0) == 0)
            {
                return default;
            };

            return Boletim[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Nfs
        /// </summary>
        public int GetBoletimCount => (Boletim != null ? Boletim.Count : 0);
#endif

        [XmlElement("receitaTotal")]
        public ReceitaTotal ReceitaTotal { get; set; }
    }

    /// <summary>
    /// Boletim do espetáculo desportivo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Boletim")]
    [ComVisible(true)]
#endif
    public class Boletim
    {
        [XmlElement("nrBoletim")]
        public string NrBoletim { get; set; }

        [XmlElement("tpCompeticao")]
        public TipoCompeticao TpCompeticao { get; set; }

        [XmlElement("categEvento")]
        public CategoriaEventoReinf CategEvento { get; set; }

        [XmlElement("modDesportiva")]
        public string ModDesportiva { get; set; }

        [XmlElement("nomeCompeticao")]
        public string NomeCompeticao { get; set; }

        [XmlElement("cnpjMandante")]
        public string CnpjMandante { get; set; }

        [XmlElement("cnpjVisitante")]
        public string CnpjVisitante { get; set; }

        [XmlElement("nomeVisitante")]
        public string NomeVisitante { get; set; }

        [XmlElement("pracaDesportiva")]
        public string PracaDesportiva { get; set; }

        [XmlElement("codMunic")]
        public string CodMunic { get; set; }

        [XmlElement("uf")]
        public UFBrasil Uf { get; set; }

        [XmlElement("qtdePagantes")]
        public string QtdePagantes { get; set; }

        [XmlElement("qtdeNaoPagantes")]
        public string QtdeNaoPagantes { get; set; }

        [XmlElement("receitaIngressos")]
        public List<ReceitaIngressos> ReceitaIngressos { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReceitaIngressos(ReceitaIngressos item)
        {
            if (ReceitaIngressos == null)
            {
                ReceitaIngressos = new List<ReceitaIngressos>();
            }

            ReceitaIngressos.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ReceitaIngressos (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ReceitaIngressos</returns>
        public ReceitaIngressos GetReceitaIngressos(int index)
        {
            if ((ReceitaIngressos?.Count ?? 0) == 0)
            {
                return default;
            };

            return ReceitaIngressos[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ReceitaIngressos
        /// </summary>
        public int GetReceitaIngressosCount => (ReceitaIngressos != null ? ReceitaIngressos.Count : 0);
#endif

        [XmlElement("outrasReceitas")]
        public List<OutrasReceitas> OutrasReceitas { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddOutrasReceitas(OutrasReceitas item)
        {
            if (OutrasReceitas == null)
            {
                OutrasReceitas = new List<OutrasReceitas>();
            }

            OutrasReceitas.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista OutrasReceitas (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da OutrasReceitas</returns>
        public OutrasReceitas GetOutrasReceitas(int index)
        {
            if ((OutrasReceitas?.Count ?? 0) == 0)
            {
                return default;
            };

            return OutrasReceitas[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Nfs
        /// </summary>
        public int GetOutrasReceitasCount => (OutrasReceitas != null ? OutrasReceitas.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeCnpjVisitante() => !string.IsNullOrEmpty(CnpjVisitante);
        public bool ShouldSerializeNomeVisitante() => !string.IsNullOrEmpty(NomeVisitante);
        public bool ShouldSerializeCodMunic() => !string.IsNullOrEmpty(CodMunic);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ReceitaIngressos")]
    [ComVisible(true)]
#endif
    public class ReceitaIngressos
    {
        [XmlElement("tpIngresso")]
        public TipoDeIngresso TpIngresso { get; set; }

        [XmlElement("descIngr")]
        public string DescIngr { get; set; }

        [XmlElement("qtdeIngrVenda")]
        public string QtdeIngrVenda { get; set; }

        [XmlElement("qtdeIngrVendidos")]
        public string QtdeIngrVendidos { get; set; }

        [XmlElement("qtdeIngrDev")]
        public string QtdeIngrDev { get; set; }

        [XmlIgnore]
        public double PrecoIndiv { get; set; }

        [XmlElement("precoIndiv")]
        public string PrecoIndivField
        {
            get => PrecoIndiv.ToString("F2", CultureInfoReinf.Info);
            set => PrecoIndiv = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrTotal { get; set; }

        [XmlElement("vlrTotal")]
        public string VlrTotalField
        {
            get => VlrTotal.ToString("F2", CultureInfoReinf.Info);
            set => VlrTotal = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.OutrasReceitas")]
    [ComVisible(true)]
#endif
    public class OutrasReceitas
    {
        [XmlElement("tpReceita")]
        public TipoReceita TpReceita { get; set; }

        [XmlIgnore]
        public double VlrReceita { get; set; }

        [XmlElement("vlrReceita")]
        public string VlrReceitaField
        {
            get => VlrReceita.ToString("F2", CultureInfoReinf.Info);
            set => VlrReceita = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("descReceita")]
        public string DescReceita { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ReceitaTotal")]
    [ComVisible(true)]
#endif
    public class ReceitaTotal
    {
        [XmlIgnore]
        public double VlrReceitaTotal { get; set; }

        [XmlElement("vlrReceitaTotal")]
        public string VlrReceitaTotalField
        {
            get => VlrReceitaTotal.ToString("F2", CultureInfoReinf.Info);
            set => VlrReceitaTotal = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCP { get; set; }

        [XmlElement("vlrCP")]
        public string VlrCPField
        {
            get => VlrCP.ToString("F2", CultureInfoReinf.Info);
            set => VlrCP = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCPSuspTotal { get; set; }

        [XmlElement("vlrCPSuspTotal")]
        public string VlrCPSuspTotalField
        {
            get => VlrCPSuspTotal.ToString("F2", CultureInfoReinf.Info);
            set => VlrCPSuspTotal = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrReceitaClubes { get; set; }

        [XmlElement("vlrReceitaClubes")]
        public string VlrReceitaClubesField
        {
            get => VlrReceitaClubes.ToString("F2", CultureInfoReinf.Info);
            set => VlrReceitaClubes = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrRetParc { get; set; }

        [XmlElement("vlrRetParc")]
        public string VlrRetParcField
        {
            get => VlrRetParc.ToString("F2", CultureInfoReinf.Info);
            set => VlrRetParc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("infoProc")]
        public List<InfoProc3010> InfoProc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProc(InfoProc3010 item)
        {
            if (InfoProc == null)
            {
                InfoProc = new List<InfoProc3010>();
            }

            InfoProc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProc</returns>
        public InfoProc3010 GetInfoProc(int index)
        {
            if ((InfoProc?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoProc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoProc
        /// </summary>
        public int GetInfoProcCount => (InfoProc != null ? InfoProc.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVlrCPSuspTotalField() => VlrCPSuspTotal > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProc3010")]
    [ComVisible(true)]
#endif
    public class InfoProc3010
    {
        [XmlElement("tpProc")]
        public TipoProcesso TpProc { get; set; }

        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        [XmlElement("codSusp")]
        public string CodSusp { get; set; }

        [XmlIgnore]
        public double VlrCPSusp { get; set; }

        [XmlElement("vlrCPSusp")]
        public string VlrCPSuspField
        {
            get => VlrCPSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrCPSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeCodSusp() => !string.IsNullOrEmpty(CodSusp);

        #endregion ShouldSerialize
    }
}
