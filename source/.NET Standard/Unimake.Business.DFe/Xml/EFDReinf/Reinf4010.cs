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
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf4010")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4010PagtoBeneficiarioPF/v2_01_02", IsNullable = false)]
    public class Reinf4010 : XMLBase
    {
        [XmlElement("evtRetPF")]
        public EvtRetPF EvtRetPF { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtRetPF")]
    [ComVisible(true)]
#endif
    public class EvtRetPF : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEventoReinf4010 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContriReinf4010 IdeContri { get; set; }

        [XmlElement("ideEstab")]
        public IdeEstabReinf4010 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEventoReinf4010")]
    [ComVisible(true)]
#endif
    public class IdeEventoReinf4010
    {
        [XmlElement("indRetif")]
        public IndicativoRetificacao IndRetif { get; set; }

        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

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

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("procEmi")]
        public ProcessoEmissaoReinf ProcEmi { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeContriReinf4010")]
    [ComVisible(true)]
#endif
    public class IdeContriReinf4010 : IdeContri
    {
        [XmlElement("infoComplContri")]
        public InfoComplContri InfoComplContri { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoComplContri")]
    [ComVisible(true)]
#endif
    public class InfoComplContri
    {
        [XmlElement("natJur")]
        public string NatJur { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstabReinf4010")]
    [ComVisible(true)]
#endif
    public class IdeEstabReinf4010 : IdeEstab
    {
        [XmlElement("ideBenef")]
        public IdeBenef IdeBenef { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeBenef")]
    [ComVisible(true)]
#endif
    public class IdeBenef
    {
        [XmlElement("cpfBenef")]
        public string CpfBenef { get; set; }

        [XmlElement("nmBenef")]
        public string NmBenef { get; set; }

        [XmlElement("ideEvtAdic")]
        public string IdeEvtAdic { get; set; }

        [XmlElement("ideDep")]
        public List<IdeDep> IdeDep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeDep(IdeDep item)
        {
            if (IdeDep == null)
            {
                IdeDep = new List<IdeDep>();
            }

            IdeDep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeDep (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeDep</returns>
        public IdeDep GetIdeDep(int index)
        {
            if ((IdeDep?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeDep[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeDep
        /// </summary>
        public int GetIdeDepCount => (IdeDep != null ? IdeDep.Count : 0);
#endif

        [XmlElement("idePgto")]
        public List<IdePgto> IdePgto { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdePgto(IdePgto item)
        {
            if (IdePgto == null)
            {
                IdePgto = new List<IdePgto>();
            }

            IdePgto.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdePgto (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdePgto</returns>
        public IdePgto GetIdePgto(int index)
        {
            if ((IdePgto?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdePgto[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdePgto
        /// </summary>
        public int GetIdePgtoCount => (IdePgto != null ? IdePgto.Count : 0);
#endif

        [XmlElement("ideOpSaude")]
        public List<IdeOpSaude> IdeOpSaude { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeOpSaude(IdeOpSaude item)
        {
            if (IdeOpSaude == null)
            {
                IdeOpSaude = new List<IdeOpSaude>();
            }

            IdeOpSaude.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeOpSaude (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeOpSaude</returns>
        public IdeOpSaude GetIdeOpSaude(int index)
        {
            if ((IdeOpSaude?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeOpSaude[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeOpSaude
        /// </summary>
        public int GetIdeOpSaudeCount => (IdeOpSaude != null ? IdeOpSaude.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSereializeCpfBenef() => !string.IsNullOrEmpty(CpfBenef);

        public bool ShouldSereializeNmBenef() => !string.IsNullOrEmpty(NmBenef);

        public bool ShouldSereializeIdeEvtAdic() => !string.IsNullOrEmpty(IdeEvtAdic);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeDep")]
    [ComVisible(true)]
#endif
    public class IdeDep
    {
        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        [XmlElement("relDep")]
        public RelacaoDeDependencia RelDep { get; set; }

        [XmlElement("descrDep")]
        public string DescrDep { get; set; }

        #region ShouldSerialize

        public bool ShouldSereializeDescrDep() => !string.IsNullOrEmpty(DescrDep);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdePgto")]
    [ComVisible(true)]
#endif
    public class IdePgto
    {
        [XmlElement("natRend")]
        public string NatRend { get; set; }

        [XmlElement("observ")]
        public string Observ { get; set; }

        [XmlElement("infoPgto")]
        public List<InfoPgto> InfoPgto { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoPgto(InfoPgto item)
        {
            if (InfoPgto == null)
            {
                InfoPgto = new List<InfoPgto>();
            }

            InfoPgto.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoPgto (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoPgto</returns>
        public InfoPgto GetInfoPgto(int index)
        {
            if ((InfoPgto?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoPgto[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoPgto
        /// </summary>
        public int GetInfoPgtoCount => (InfoPgto != null ? InfoPgto.Count : 0);
#endif

        #region ShoulSerialize

        public bool ShouldSereializeObserv() => !string.IsNullOrEmpty(Observ);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoPgto")]
    [ComVisible(true)]
#endif
    public class InfoPgto
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtFG { get; set; }
#else
        public DateTimeOffset DtFG { get; set; }
#endif

        [XmlElement("dtFG")]
        public string DtFGField
        {
            get => DtFG.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtFG = DateTime.Parse(value);
#else
            set => DtFG = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("compFP")]
        public string CompFP { get; set; }

        [XmlElement("indDecTerc")]
        public string IndDecTerc { get; set; }

        [XmlIgnore]
        public double VlrRendBruto { get; set; }

        [XmlElement("vlrRendBruto")]
        public string VlrRendBrutoField
        {
            get => VlrRendBruto.ToString("F2", CultureInfoReinf.Info);
            set => VlrRendBruto = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrRendTrib { get; set; }

        [XmlElement("vlrRendTrib")]
        public string VlrRendTribField
        {
            get => VlrRendTrib.ToString("F2", CultureInfoReinf.Info);
            set => VlrRendTrib = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrIR { get; set; }

        [XmlElement("vlrIR")]
        public string VlrIRField
        {
            get => VlrIR.ToString("F2", CultureInfoReinf.Info);
            set => VlrIR = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("indRRA")]
        public string IndRRA { get; set; }

        [XmlElement("indFciScp")]
#if INTEROP
        public IndicativoFundoDeInvestimento IndFciScp { get; set; } = (IndicativoFundoDeInvestimento)(-1);
#else
        public IndicativoFundoDeInvestimento ? IndFciScp { get; set; }
#endif

        [XmlElement("nrInscFciScp")]
        public string NrInscFciScp { get; set; }

        [XmlElement("percSCP")]
        public string PercSCP { get; set; }

        [XmlElement("indJud")]
#if INTEROP
        public SimNaoLetra IndJud { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndJud { get; set; }
#endif

        [XmlElement("paisResidExt")]
        public string PaisResidExt { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtEscrCont { get; set; }
#else
        public DateTimeOffset DtEscrCont { get; set; }
#endif

        [XmlElement("dtEscrCont")]
        public string DtEscrContField
        {
            get => DtEscrCont.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtEscrCont = DateTime.Parse(value);
#else
            set => DtEscrCont = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("observ")]
        public string Observ { get; set; }

        [XmlElement("detDed")]
        public List<DetDed> DetDed { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDetDed(DetDed item)
        {
            if (DetDed == null)
            {
                DetDed = new List<DetDed>();
            }

            DetDed.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DetDed (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetDed</returns>
        public DetDed GetDetDed(int index)
        {
            if ((DetDed?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetDed[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetDed
        /// </summary>
        public int GetDetDedCount => (DetDed != null ? DetDed.Count : 0);
#endif

        [XmlElement("rendIsento")]
        public List<RendIsento> RendIsento { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRendIsento(RendIsento item)
        {
            if (RendIsento == null)
            {
                RendIsento = new List<RendIsento>();
            }

            RendIsento.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RendIsento (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RendIsento</returns>
        public RendIsento GetRendIsento(int index)
        {
            if ((RendIsento?.Count ?? 0) == 0)
            {
                return default;
            };

            return RendIsento[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RendIsento
        /// </summary>
        public int GetRendIsentoCount => (RendIsento != null ? RendIsento.Count : 0);
#endif

        [XmlElement("infoProcRet")]
        public List<InfoProcRet> InfoProcRet { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProcRet(InfoProcRet item)
        {
            if (InfoProcRet == null)
            {
                InfoProcRet = new List<InfoProcRet>();
            }

            InfoProcRet.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProcRet (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProcRet</returns>
        public InfoProcRet GetInfoProcRet(int index)
        {
            if ((InfoProcRet?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoProcRet[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoProcRet
        /// </summary>
        public int GetInfoProcRetCount => (InfoProcRet != null ? InfoProcRet.Count : 0);
#endif

        [XmlElement("infoRRA")]
        public InfoRRA InfoRRA { get; set; }

        [XmlElement("infoProcJud")]
        public InfoProcJudReinf4010 InfoProcJud { get; set; }

        [XmlElement("infoPgtoExt")]
        public InfoPgtoExt InfoPgtoExt { get; set; }

        #region ShouldSerialize

        public bool ShouldSereializeCompFP() => !string.IsNullOrEmpty(CompFP);

        public bool ShouldSereializeIndDecTerc() => !string.IsNullOrEmpty(IndDecTerc);

        public bool ShouldSerializeVlrRendTrib() => VlrRendTrib > 0;

        public bool ShouldSerializeVlrIR() => VlrIR > 0;

        public bool ShouldSereializeIndRRA() => !string.IsNullOrEmpty(IndRRA);

#if INTEROP
        public bool ShouldSerializeIndFciScp() => IndFciScp != (IndicativoFundoDeInvestimento)(-1);
#else
        public bool ShouldSerializeIndFciScp() => IndFciScp != null;
#endif

        public bool ShouldSereializeNrInscFciScp() => !string.IsNullOrEmpty(NrInscFciScp);

        public bool ShouldSereializePercSCP() => !string.IsNullOrEmpty(PercSCP);

#if INTEROP
        public bool ShouldSerializeIndJud() => IndJud != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndJud() => IndJud != null;
#endif

        public bool ShouldSereializePaisResidExt() => !string.IsNullOrEmpty(PaisResidExt);

        public bool ShouldSerializeDtEscrContField() => DtEscrCont > DateTime.MinValue;

        public bool ShouldSereializeObserv() => !string.IsNullOrEmpty(Observ);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.DetDed")]
    [ComVisible(true)]
#endif
    public class DetDed
    {
        [XmlElement("indTpDeducao")]
        public IndicativoTipoDeducao IndTpDeducao { get; set; }

        [XmlIgnore]
        public double VlrDeducao { get; set; }

        [XmlElement("vlrDeducao")]
        public string VlrDeducaoField
        {
            get => VlrDeducao.ToString("F2", CultureInfoReinf.Info);
            set => VlrDeducao = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("infoEntid")]
#if INTEROP
        public SimNaoLetra InfoEntid { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra ? InfoEntid { get; set; }
#endif

        [XmlElement("nrInscPrevComp")]
        public string NrInscPrevComp { get; set; }

        [XmlIgnore]
        public double VlrPatrocFunp { get; set; }

        [XmlElement("vlrPatrocFunp")]
        public string VlrPatrocFunpField
        {
            get => VlrPatrocFunp.ToString("F2", CultureInfoReinf.Info);
            set => VlrPatrocFunp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("benefPen")]
        public List<BenefPen> BenefPen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBenefPen(BenefPen item)
        {
            if (BenefPen == null)
            {
                BenefPen = new List<BenefPen>();
            }

            BenefPen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BenefPen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BenefPen</returns>
        public BenefPen GetBenefPen(int index)
        {
            if ((BenefPen?.Count ?? 0) == 0)
            {
                return default;
            };

            return BenefPen[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista BenefPen
        /// </summary>
        public int GetBenefPenCount => (BenefPen != null ? BenefPen.Count : 0);
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeInfoEntid() => InfoEntid != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeInfoEntid() => InfoEntid != null;
#endif

        public bool ShouldSereializeNrInscPrevComp() => !string.IsNullOrEmpty(NrInscPrevComp);

        public bool ShouldSerializeVlrPatrocFunp() => VlrPatrocFunp > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.BenefPen")]
    [ComVisible(true)]
#endif
    public class BenefPen
    {
        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        [XmlIgnore]
        public double VlrDepen { get; set; }

        [XmlElement("vlrDepen")]
        public string VlrDepenField
        {
            get => VlrDepen.ToString("F2", CultureInfoReinf.Info);
            set => VlrDepen = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.BenefPenSusp")]
    [ComVisible(true)]
#endif
    public class BenefPenSusp
    {
        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        [XmlIgnore]
        public double VlrDepen { get; set; }

        [XmlIgnore]
        public double VlrDepenSusp { get; set; }

        [XmlElement("vlrDepenSusp")]
        public string VlrDepenSuspField
        {
            get => VlrDepenSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrDepenSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RendIsento")]
    [ComVisible(true)]
#endif
    public class RendIsento
    {
        [XmlElement("tpIsencao")]
        public TipoIsencao TpIsencao { get; set; }

        [XmlIgnore]
        public double VlrIsento { get; set; }

        [XmlElement("vlrIsento")]
        public string VlrIsentoField
        {
            get => VlrIsento.ToString("F2", CultureInfoReinf.Info);
            set => VlrIsento = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("descRendimento")]
        public string DescRendimento { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtLaudo { get; set; }
#else
        public DateTimeOffset DtLaudo { get; set; }
#endif

        [XmlElement("dtLaudo")]
        public string DtLaudoField
        {
            get => DtLaudo.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtLaudo = DateTime.Parse(value);
#else
            set => DtLaudo = DateTimeOffset.Parse(value);
#endif
        }

        #region ShouldSerialize

        public bool ShouldSereializeDescRendimento() => !string.IsNullOrEmpty(DescRendimento);

        public bool ShouldSerializeDtObitoField() => DtLaudo > DateTime.MinValue;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcRet")]
    [ComVisible(true)]
#endif
    public class InfoProcRet
    {
        [XmlElement("tpProcRet")]
        public TipoProcesso TpProcRet { get; set; }

        [XmlElement("nrProcRet")]
        public string NrProcRet { get; set; }

        [XmlElement("codSusp")]
        public string CodSusp { get; set; }

        [XmlIgnore]
        public double VlrNRetido { get; set; }

        [XmlElement("vlrNRetido")]
        public string VlrNRetidoField
        {
            get => VlrNRetido.ToString("F2", CultureInfoReinf.Info);
            set => VlrNRetido = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrDepJud { get; set; }

        [XmlElement("vlrDepJud")]
        public string VlrDepJudField
        {
            get => VlrDepJud.ToString("F2", CultureInfoReinf.Info);
            set => VlrDepJud = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCmpAnoCal { get; set; }

        [XmlElement("vlrCmpAnoCal")]
        public string VlrCmpAnoCalField
        {
            get => VlrCmpAnoCal.ToString("F2", CultureInfoReinf.Info);
            set => VlrCmpAnoCal = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCmpAnoAnt { get; set; }

        [XmlElement("vlrCmpAnoAnt")]
        public string VlrCmpAnoAntField
        {
            get => VlrCmpAnoAnt.ToString("F2", CultureInfoReinf.Info);
            set => VlrCmpAnoAnt = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrRendSusp { get; set; }

        [XmlElement("vlrRendSusp")]
        public string VlrRendSuspField
        {
            get => VlrRendSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrRendSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("dedSusp")]
        public List<DedSusp> DedSusp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDedSusp(DedSusp item)
        {
            if (DedSusp == null)
            {
                DedSusp = new List<DedSusp>();
            }

            DedSusp.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DedSusp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DedSusp</returns>
        public DedSusp GetDedSusp(int index)
        {
            if ((DedSusp?.Count ?? 0) == 0)
            {
                return default;
            };

            return DedSusp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DedSusp
        /// </summary>
        public int GetDedSuspCount => (DedSusp != null ? DedSusp.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSereializeCodSusp() => !string.IsNullOrEmpty(CodSusp);

        public bool ShouldSerializeVlrNRetido() => VlrNRetido > 0;

        public bool ShouldSerializeVlrDepJu() => VlrDepJud > 0;

        public bool ShouldSerializeVlrCmpAnoCal() => VlrCmpAnoCal > 0;

        public bool ShouldSerializeVlrCmpAnoAnt() => VlrCmpAnoAnt > 0;

        public bool ShouldSerializeVlrRendSusp() => VlrRendSusp > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.DedSusp")]
    [ComVisible(true)]
#endif
    public class DedSusp
    {
        [XmlElement("indTpDeducao")]
        public IndicativoTipoDeducao IndTpDeducao { get; set; }

        [XmlIgnore]
        public double VlrDedSusp { get; set; }

        [XmlElement("vlrDedSusp")]
        public string VlrDedSuspField
        {
            get => VlrDedSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrDedSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("benefPen")]
        public List<BenefPenSusp> BenefPenSusp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBenefPenSusp(BenefPenSusp item)
        {
            if (BenefPenSusp == null)
            {
                BenefPenSusp = new List<BenefPenSusp>();
            }

            BenefPenSusp.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BenefPenSusp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BenefPenSusp</returns>
        public BenefPenSusp GetBenefPenSusp(int index)
        {
            if ((BenefPenSusp?.Count ?? 0) == 0)
            {
                return default;
            };

            return BenefPenSusp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista BenefPen
        /// </summary>
        public int GetBenefPenSuspCount => (BenefPenSusp != null ? BenefPenSusp.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVlrDedSusp() => VlrDedSusp > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoRRA")]
    [ComVisible(true)]
#endif
    public class InfoRRA
    {
        [XmlElement("tpProcRRA")]
        public TipoProcesso TpProcRRA { get; set; }

        [XmlElement("nrProcRRA")]
        public string NrProcRRA { get; set; }

        [XmlElement("indOrigRec")]
        public IndicativoOrigemRecursos IndOrigRec { get; set; }

        [XmlElement("descRRA")]
        public string DescRRA { get; set; }

        [XmlElement("qtdMesesRRA")]
        public string QtdMesesRRA { get; set; }

        [XmlElement("cnpjOrigRecurso")]
        public string CnpjOrigRecurso { get; set; }

        [XmlElement("despProcJud")]
        public DespProcJud DespProcJud { get; set; }

        #region ShouldSerialize

        public bool ShouldSereializeNrProcRRA() => !string.IsNullOrEmpty(NrProcRRA);

        public bool ShouldSereializeDescRRA() => !string.IsNullOrEmpty(DescRRA);

        public bool ShouldSereializeCnpjOrigRecurso() => !string.IsNullOrEmpty(CnpjOrigRecurso);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.DespProcJud")]
    [ComVisible(true)]
#endif
    public class DespProcJud
    {
        [XmlIgnore]
        public double VlrDespCustas { get; set; }

        [XmlElement("vlrDespCustas")]
        public string VlrDespCustasField
        {
            get => VlrDespCustas.ToString("F2", CultureInfoReinf.Info);
            set => VlrDespCustas = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrDespAdvogados { get; set; }

        [XmlElement("vlrDespAdvogados")]
        public string VlrDespAdvogadosField
        {
            get => VlrDespAdvogados.ToString("F2", CultureInfoReinf.Info);
            set => VlrDespAdvogados = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("ideAdv")]
        public List<IdeAdv> IdeAdv { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeAdv(IdeAdv item)
        {
            if (IdeAdv == null)
            {
                IdeAdv = new List<IdeAdv>();
            }

            IdeAdv.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeAdv (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeAdv</returns>
        public IdeAdv GetIdeAdv(int index)
        {
            if ((IdeAdv?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeAdv[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeAdv
        /// </summary>
        public int GetIdeAdvCount => (IdeAdv != null ? IdeAdv.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeAdv")]
    [ComVisible(true)]
#endif
    public class IdeAdv
    {
        [XmlElement("tpInscAdv")]
        public TiposInscricao TpInscAdv { get; set; }

        [XmlElement("nrInscAdv")]
        public string NrInscAdv { get; set; }

        [XmlIgnore]
        public double VlrAdv { get; set; }

        [XmlElement("vlrAdv")]
        public string VlrAdvField
        {
            get => VlrAdv.ToString("F2", CultureInfoReinf.Info);
            set => VlrAdv = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrAdv() => VlrAdv > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcJudReinf4010")]
    [ComVisible(true)]
#endif
    public class InfoProcJudReinf4010
    {
        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        [XmlElement("indOrigRec")]
        public IndicativoOrigemRecursos IndOrigRec { get; set; }

        [XmlElement("cnpjOrigRecurso")]
        public string CnpjOrigRecurso { get; set; }

        [XmlElement("desc")]
        public string Desc { get; set; }

        [XmlElement("despProcJud")]
        public DespProcJud DespProcJud { get; set; }

        #region ShouldSerialize

        public bool ShouldSereializeCnpjOrigRecurso() => !string.IsNullOrEmpty(CnpjOrigRecurso);

        public bool ShouldSereializeDesc() => !string.IsNullOrEmpty(Desc);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoPgtoExt")]
    [ComVisible(true)]
#endif
    public class InfoPgtoExt
    {
        [XmlElement("indNIF")]
        public IndicativoNIF IndNIF { get; set; }

        [XmlElement("nifBenef")]
        public string NifBenef { get; set; }

        [XmlElement("frmTribut")]
        public string FrmTribut { get; set; }

        [XmlElement("endExt")]
        public EndExt EndExt { get; set; }

        #region ShouldSerialize

        public bool ShouldSereializeNifBenef() => !string.IsNullOrEmpty(NifBenef);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeOpSaude")]
    [ComVisible(true)]
#endif
    public class IdeOpSaude
    {
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("regANS")]
        public string RegANS { get; set; }

        [XmlIgnore]
        public double VlrSaude { get; set; }

        [XmlElement("vlrSaude")]
        public string VlrSaudeField
        {
            get => VlrSaude.ToString("F2", CultureInfoReinf.Info);
            set => VlrSaude = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("infoReemb")]
        public List<InfoReemb> InfoReemb { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoReemb(InfoReemb item)
        {
            if (InfoReemb == null)
            {
                InfoReemb = new List<InfoReemb>();
            }

            InfoReemb.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoReemb (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoReemb</returns>
        public InfoReemb GetInfoReemb(int index)
        {
            if ((InfoReemb?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoReemb[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoReemb
        /// </summary>
        public int GetInfoReembCount => (InfoReemb != null ? InfoReemb.Count : 0);
#endif

        [XmlElement("infoDependPl")]
        public List<InfoDependPl> InfoDependPl { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoDependPl(InfoDependPl item)
        {
            if (InfoDependPl == null)
            {
                InfoDependPl = new List<InfoDependPl>();
            }

            InfoDependPl.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoDependPl (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoDependPl</returns>
        public InfoDependPl GetInfoDependPl(int index)
        {
            if ((InfoDependPl?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoDependPl[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoDependPl
        /// </summary>
        public int GetInfoDependPlCount => (InfoDependPl != null ? InfoDependPl.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeRegANS() => !string.IsNullOrEmpty(RegANS);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoReemb")]
    [ComVisible(true)]
#endif
    public class InfoReemb
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlIgnore]
        public double VlrReemb { get; set; }

        [XmlElement("vlrReemb")]
        public string VlrReembField
        {
            get => VlrReemb.ToString("F2", CultureInfoReinf.Info);
            set => VlrReemb = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrReembAnt { get; set; }

        [XmlElement("vlrReembAnt")]
        public string VlrReembAntField
        {
            get => VlrReembAnt.ToString("F2", CultureInfoReinf.Info);
            set => VlrReembAnt = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrReemb() => VlrReemb > 0;

        public bool ShouldSerializeVlrReembAnt() => VlrReembAnt > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoDependPl")]
    [ComVisible(true)]
#endif
    public class InfoDependPl
    {
        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        [XmlIgnore]
        public double VlrSaude { get; set; }

        [XmlElement("vlrSaude")]
        public string VlrSaudeField
        {
            get => VlrSaude.ToString("F2", CultureInfoReinf.Info);
            set => VlrSaude = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("infoReembDep")]
        public List<InfoReembDep> InfoReembDep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoReembDep(InfoReembDep item)
        {
            if (InfoReembDep == null)
            {
                InfoReembDep = new List<InfoReembDep>();
            }

            InfoReembDep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoReembDep (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoReembDep</returns>
        public InfoReembDep GetInfoReembDep(int index)
        {
            if ((InfoReembDep?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoReembDep[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoReembDep
        /// </summary>
        public int GetInfoReembDepCount => (InfoReembDep != null ? InfoReembDep.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoReembDep")]
    [ComVisible(true)]
#endif
    public class InfoReembDep
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlIgnore]
        public double VlrReemb { get; set; }

        [XmlElement("vlrReemb")]
        public string VlrReembField
        {
            get => VlrReemb.ToString("F2", CultureInfoReinf.Info);
            set => VlrReemb = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrReembAnt { get; set; }

        [XmlElement("vlrReembAnt")]
        public string VlrReembAntField
        {
            get => VlrReembAnt.ToString("F2", CultureInfoReinf.Info);
            set => VlrReembAnt = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrReemb() => VlrReemb > 0;

        public bool ShouldSerializeVlrReembAnt() => VlrReembAnt > 0;

        #endregion
    }
}