#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1210")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtPgtos/v_S_01_02_00", IsNullable = false)]
    public class ESocial1210 : XMLBase
    {
        [XmlElement("evtPgtos")]
        public EvtPgtos EvtPgtos { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtPgtos")]
    [ComVisible(true)]
#endif
    public class EvtPgtos
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEventoESocial1210 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideBenef")]
        public IdeBenef IdeBenef { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEventoESocial1210")]
    [ComVisible(true)]
#endif
    public class IdeEventoESocial1210
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

        [XmlElement("indGuia")]
        public string IndGuia { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("procEmi")]
        public ProcEmiESocial ProcEmi { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }

        #region ShouldSerialize

        public bool ShouldSereializeNrReciboField() => !string.IsNullOrEmpty(NrRecibo);

        public bool ShouldSereializeIndGuiaField() => !string.IsNullOrEmpty(IndGuia);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeBenef")]
    [ComVisible(true)]
#endif
    public class IdeBenef
    {
        [XmlElement("cpfBenef")]
        public string CpfBenef { get; set; }

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

        [XmlElement("infoIRComplem")]
        public InfoIRComplem InfoIRComplem { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPgto")]
    [ComVisible(true)]
#endif
    public class InfoPgto
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtPgto { get; set; }
#else
        public DateTimeOffset DtPgto { get; set; }
#endif

        [XmlElement("dtPgto")]
        public string DtPgtoField
        {
            get => DtPgto.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtPgto = DateTime.Parse(value);
#else
            set => DtPgto = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tpPgto")]
        public TipoPagamentoESocial TpPgto { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime PerRef { get; set; }
#else
        public DateTimeOffset PerRef { get; set; }
#endif

        [XmlElement("perRef")]
        public string PerRefField
        {
            get => PerRef.ToString("yyyy-MM");
#if INTEROP
            set => PerRef = DateTime.Parse(value);
#else
            set => PerRef = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("ideDmDev")]
        public string IdeDmDev { get; set; }

        [XmlElement("vrLiq")]
        public double VrLiq { get; set; }

        [XmlElement("paisResidExt")]
        public string PaisResidExt { get; set; }

        [XmlElement("infoPgtoExt")]
        public InfoPgtoExt InfoPgtoExt { get; set; }

        #region ShouldSerialize

        public bool ShouldSereializePaisResidExtField() => !string.IsNullOrEmpty(PaisResidExt);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPgtoExt")]
    [ComVisible(true)]
#endif
    public class InfoPgtoExt
    {
        [XmlElement("indNIF")]
        public IndicativoNIF IndNIF { get; set; }

        [XmlElement("nifBenef")]
        public string NifBenef { get; set; }

        [XmlElement("frmTribut")]
        public FormasDeTributacaoESocial FrmTribut { get; set; }

        [XmlElement("endExt")]
        public EndExt EndExt { get; set; }

        #region ShouldSerialize

        public bool ShouldSereializeNifBenefField() => !string.IsNullOrEmpty(NifBenef);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EndExt")]
    [ComVisible(true)]
#endif
    public class EndExt
    {
        [XmlElement("endDscLograd")]
        public string EndDscLograd { get; set; }

        [XmlElement("endNrLograd")]
        public string EndNrLograd { get; set; }

        [XmlElement("endComplem")]
        public string EndComplem { get; set; }

        [XmlElement("endBairro")]
        public string EndBairro { get; set; }

        [XmlElement("endCidade")]
        public string EndCidade { get; set; }

        [XmlElement("endEstado")]
        public string EndEstado { get; set; }

        [XmlElement("endCodPostal")]
        public string EndCodPostal { get; set; }

        [XmlElement("telef")]
        public string Telef { get; set; }

        #region ShouldSerialize

        public bool ShouldSereializeEndDscLogradField() => !string.IsNullOrEmpty(EndDscLograd);

        public bool ShouldSereializeEndNrLogradField() => !string.IsNullOrEmpty(EndNrLograd);

        public bool ShouldSereializeEndComplemField() => !string.IsNullOrEmpty(EndComplem);

        public bool ShouldSereializeEndBairroField() => !string.IsNullOrEmpty(EndBairro);

        public bool ShouldSereializeEndCidadeField() => !string.IsNullOrEmpty(EndCidade);

        public bool ShouldSereializeEndEstadoField() => !string.IsNullOrEmpty(EndEstado);

        public bool ShouldSereializeEndCodPostalField() => !string.IsNullOrEmpty(EndCodPostal);

        public bool ShouldSereializeTelefField() => !string.IsNullOrEmpty(Telef);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoIRComplem")]
    [ComVisible(true)]
#endif
    public class InfoIRComplem
    {
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

        [XmlElement("infoDep")]
        public List<InfoDep> InfoDep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoDep(InfoDep item)
        {
            if (InfoDep == null)
            {
                InfoDep = new List<InfoDep>();
            }

            InfoDep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoDep (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoDep</returns>
        public InfoDep GetInfoDep(int index)
        {
            if ((InfoDep?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoDep[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoDep
        /// </summary>
        public int GetInfoDepCount => (InfoDep != null ? InfoDep.Count : 0);
#endif

        [XmlElement("infoIRCR")]
        public List<InfoIRCR> InfoIRCR { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoIRCR(InfoIRCR item)
        {
            if (InfoIRCR == null)
            {
                InfoIRCR = new List<InfoIRCR>();
            }

            InfoIRCR.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoIRCR (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoIRCR</returns>
        public InfoIRCR GetInfoIRCR(int index)
        {
            if ((InfoIRCR?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoIRCR[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoIRCR
        /// </summary>
        public int GetInfoIRCRCount => (InfoIRCR != null ? InfoIRCR.Count : 0);
#endif

        [XmlElement("planSaude")]
        public List<PlanSaude> PlanSaude { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPlanSaude(PlanSaude item)
        {
            if (PlanSaude == null)
            {
                PlanSaude = new List<PlanSaude>();
            }

            PlanSaude.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PlanSaude (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PlanSaude</returns>
        public PlanSaude GetPlanSaude(int index)
        {
            if ((PlanSaude?.Count ?? 0) == 0)
            {
                return default;
            };

            return PlanSaude[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista PlanSaude
        /// </summary>
        public int GetPlanSaudeCount => (PlanSaude != null ? PlanSaude.Count : 0);
#endif

        [XmlElement("infoReembMed")]
        public List<InfoReembMed> InfoReembMed { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoReembMed(InfoReembMed item)
        {
            if (InfoReembMed == null)
            {
                InfoReembMed = new List<InfoReembMed>();
            }

            InfoReembMed.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoReembMed (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoReembMed</returns>
        public InfoReembMed GetInfoReembMed(int index)
        {
            if ((InfoReembMed?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoReembMed[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoReembMed
        /// </summary>
        public int GetInfoReembMedCount => (InfoReembMed != null ? InfoReembMed.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeDtLaudoField() => DtLaudo > DateTime.MinValue;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoDep")]
    [ComVisible(true)]
#endif
    public class InfoDep
    {
        [XmlElement("cpfDep")]
        public int CpfDep { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtNascto { get; set; }
#else
        public DateTimeOffset DtNascto { get; set; }
#endif

        [XmlElement("dtNascto")]
        public string DtNasctoField
        {
            get => DtNascto.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtNascto = DateTime.Parse(value);
#else
            set => DtNascto = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("nome")]
        public string Nome { get; set; }

        [XmlElement("depIRRF")]
        public string DepIRRF { get; set; }

        [XmlElement("tpDep")]
#if INTEROP
        public TiposDeDependente TpDep { get; set; } = (TiposDeDependente)(-1);
#else
        public TiposDeDependente? TpDep { get; set; }
#endif

        [XmlElement("descrDep")]
        public string DescrDep { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDtNasctoField() => DtNascto > DateTime.MinValue;

        public bool ShouldSereializeNomeField() => !string.IsNullOrEmpty(Nome);

        public bool ShouldSereializeDepIRRFField() => !string.IsNullOrEmpty(DepIRRF);

#if INTEROP
        public bool ShouldSerializeTpDep() => TpDep != (TiposDeDependente)(-1);
#else
        public bool ShouldSerializeTpDep() => TpDep != null;
#endif

        public bool ShouldSereializeDescrDepField() => !string.IsNullOrEmpty(DescrDep);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoIRCR")]
    [ComVisible(true)]
#endif
    public class InfoIRCR
    {
        [XmlElement("tpCR")]
        public int TpCR { get; set; }

        [XmlElement("dedDepen")]
        public List<DedDepen> DedDepen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDedDepen(DedDepen item)
        {
            if (DedDepen == null)
            {
                DedDepen = new List<DedDepen>();
            }

            DedDepen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DedDepen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DedDepen</returns>
        public DedDepen GetDedDepen(int index)
        {
            if ((DedDepen?.Count ?? 0) == 0)
            {
                return default;
            };

            return DedDepen[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DedDepen
        /// </summary>
        public int GetDedDepenCount => (DedDepen != null ? DedDepen.Count : 0);
#endif

        [XmlElement("penAlim")]
        public List<PenAlim> PenAlim { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPenAlim(PenAlim item)
        {
            if (PenAlim == null)
            {
                PenAlim = new List<PenAlim>();
            }

            PenAlim.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PenAlim (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PenAlim</returns>
        public PenAlim GetPenAlim(int index)
        {
            if ((PenAlim?.Count ?? 0) == 0)
            {
                return default;
            };

            return PenAlim[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista PenAlim
        /// </summary>
        public int GetPenAlimCount => (PenAlim != null ? PenAlim.Count : 0);
#endif

        [XmlElement("previdCompl")]
        public List<PrevidCompl> PrevidCompl { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPrevidCompl(PrevidCompl item)
        {
            if (PrevidCompl == null)
            {
                PrevidCompl = new List<PrevidCompl>();
            }

            PrevidCompl.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PrevidCompl (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PrevidCompl</returns>
        public PrevidCompl GetPrevidCompl(int index)
        {
            if ((PrevidCompl?.Count ?? 0) == 0)
            {
                return default;
            };

            return PrevidCompl[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista PrevidCompl
        /// </summary>
        public int GetPrevidComplCount => (PrevidCompl != null ? PrevidCompl.Count : 0);
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


        #region ShouldSerialize 

        public bool ShouldSerializeTpCRField() => TpCR > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DedDepen")]
    [ComVisible(true)]
#endif
    public class DedDepen
    {
        [XmlElement("tpRend")]
#if INTEROP
        public TipoDeRendimento TpRend { get; set; } = (TipoDeRendimento)(-1);
#else
        public TipoDeRendimento ? TpRend { get; set; }
#endif

        [XmlElement("cpfDep")]
        public int CpfDep { get; set; }

        [XmlElement("vlrDedDep")]
        public double VlrDedDep { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeTpRend() => TpRend != (TipoDeRendimento)(-1);
#else
        public bool ShouldSerializeTpRend() => TpRend != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PenAlim")]
    [ComVisible(true)]
#endif
    public class PenAlim
    {
        [XmlElement("tpRend")]
#if INTEROP
        public TipoDeRendimento TpRend { get; set; } = (TipoDeRendimento)(-1);
#else
        public TipoDeRendimento ? TpRend { get; set; }
#endif

        [XmlElement("cpfDep")]
        public int CpfDep { get; set; }

        [XmlElement("vlrDedPenAlim")]
        public double VlrDedPenAlim { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeTpRend() => TpRend != (TipoDeRendimento)(-1);
#else
        public bool ShouldSerializeTpRend() => TpRend != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PrevidCompl")]
    [ComVisible(true)]
#endif
    public class PrevidCompl
    {
        [XmlElement("tpPrev")]
        public TipoDePrevidenciaComplementar TpPrev { get; set; }

        [XmlElement("cnpjEntidPC")]
        public string CnpjEntidPC { get; set; }

        [XmlElement("vlrDedPC")]
        public double VlrDedPC { get; set; }

        [XmlElement("vlrPatrocFunp")]
        public double VlrPatrocFunp { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVlrPatrocFunp() => VlrPatrocFunp > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcRet")]
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

        [XmlElement("infoValores")]
        public List<InfoValores> InfoValores { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoValores(InfoValores item)
        {
            if (InfoValores == null)
            {
                InfoValores = new List<InfoValores>();
            }

            InfoValores.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoValores (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoValores</returns>
        public InfoValores GetInfoValores(int index)
        {
            if ((InfoValores?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoValores[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoValores
        /// </summary>
        public int GetInfoValoresCount => (InfoValores != null ? InfoValores.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSereializeCodSuspField() => !string.IsNullOrEmpty(CodSusp);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoValores")]
    [ComVisible(true)]
#endif
    public class InfoValores
    {
        [XmlElement("indApuracao")]
        public IndApuracao IndApuracao { get; set; }

        [XmlElement("vlrNRetido")]
        public double VlrNRetido { get; set; }

        [XmlElement("vlrDepJud")]
        public double VlrDepJud { get; set; }

        [XmlElement("vlrCmpAnoCal")]
        public double VlrCmpAnoCal { get; set; }

        [XmlElement("vlrCmpAnoAnt")]
        public double VlrCmpAnoAnt { get; set; }

        [XmlElement("vlrRendSusp")]
        public double VlrRendSusp { get; set; }

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

        public bool ShouldSerializeVlrNRetido() => VlrNRetido > 0;
        
        public bool ShouldSerializeVlrDepJud() => VlrDepJud > 0;
        
        public bool ShouldSerializeVlrCmpAnoCal() => VlrCmpAnoCal > 0;
        
        public bool ShouldSerializeVlrCmpAnoAnt() => VlrCmpAnoAnt > 0;
       
        public bool ShouldSerializeVlrRendSusp() => VlrRendSusp > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DedSusp")]
    [ComVisible(true)]
#endif
    public class DedSusp
    {
        [XmlElement("indTpDeducao")]
        public IndicativoTipoDeducao IndTpDeducao { get; set; }

        [XmlElement("vlrDedSusp")]
        public double VlrDedSusp { get; set; }

        [XmlElement("cnpjEntidPC")]
        public string CnpjEntidPC { get; set; }

        [XmlElement("vlrPatrocFunp")]
        public double VlrPatrocFunp { get; set; }

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

        public bool ShouldSerializeVlrDedSusp() => VlrDedSusp > 0;

        public bool ShouldSereializeCnpjEntidPC() => !string.IsNullOrEmpty(CnpjEntidPC);

        public bool ShouldSerializeVlrPatrocFunp() => VlrPatrocFunp > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BenefPen")]
    [ComVisible(true)]
#endif
    public class BenefPen
    {
        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        [XmlElement("vlrDepenSusp")]
        public double VlrDepenSusp { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PlanSaude")]
    [ComVisible(true)]
#endif
    public class PlanSaude
    {
        [XmlElement("cnpjOper")]
        public string CnpjOper { get; set; }

        [XmlElement("regANS")]
        public string RegANS { get; set; }

        [XmlElement("vlrSaudeTit")]
        public double VlrSaudeTit { get; set; }

        [XmlElement("infoDepSau")]
        public List<InfoDepSau> InfoDepSau { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoDepSau(InfoDepSau item)
        {
            if (InfoDepSau == null)
            {
                InfoDepSau = new List<InfoDepSau>();
            }

            InfoDepSau.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoDepSau (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoDepSau</returns>
        public InfoDepSau GetInfoDepSau(int index)
        {
            if ((InfoDepSau?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoDepSau[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoDepSau
        /// </summary>
        public int GetInfoDepSauCount => (InfoDepSau != null ? InfoDepSau.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSereializeRegANS() => !string.IsNullOrEmpty(RegANS);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoDepSau")]
    [ComVisible(true)]
#endif
    public class InfoDepSau
    {
        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        [XmlElement("vlrSaudeDep")]
        public double VlrSaudeDep { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoReembMed")]
    [ComVisible(true)]
#endif
    public class InfoReembMed
    {
        [XmlElement("indOrgReemb")]
        public IndicativoOrigemReembolso IndOrgReemb { get; set; }

        [XmlElement("cnpjOper")]
        public string CnpjOper { get; set; }

        [XmlElement("regANS")]
        public string RegANS { get; set; }

        [XmlElement("detReembTit")]
        public List<DetReembTit> DetReembTit { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDetReembTit(DetReembTit item)
        {
            if (DetReembTit == null)
            {
                DetReembTit = new List<DetReembTit>();
            }

            DetReembTit.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DetReembTit (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetReembTit</returns>
        public DetReembTit GetDetReembTit(int index)
        {
            if ((DetReembTit?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetReembTit[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetReembTit
        /// </summary>
        public int GetDetReembTitCount => (DetReembTit != null ? DetReembTit.Count : 0);
#endif

        [XmlElement("infoReembDep")]
        public List<InfoReembDepESocial1210> InfoReembDep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoReembDep(InfoReembDepESocial1210 item)
        {
            if (InfoReembDep == null)
            {
                InfoReembDep = new List<InfoReembDepESocial1210>();
            }

            InfoReembDep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoReembDep (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoReembDep</returns>
        public InfoReembDepESocial1210 GetInfoReembDep(int index)
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

        #region ShouldSerialize

        public bool ShouldSereializeCnpjOper() => !string.IsNullOrEmpty(CnpjOper);
        
        public bool ShouldSereializeRegANS() => !string.IsNullOrEmpty(RegANS);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DetReembTit")]
    [ComVisible(true)]
#endif
    public class DetReembTit
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("vlrReemb")]
        public double VlrReemb { get; set; }

        [XmlElement("vlrReembAnt")]
        public double VlrReembAnt { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVlrReemb() => VlrReemb > 0;
       
        public bool ShouldSerializeVlrReembAnt() => VlrReembAnt > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoReembDepESocial1210")]
    [ComVisible(true)]
#endif
    public class InfoReembDepESocial1210
    {
        [XmlElement("cpfBenef")]
        public string CpfBenef { get; set; }

        [XmlElement("detReembDep")]
        public List<DetReembDep> DetReembDep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDetReembDep(DetReembDep item)
        {
            if (DetReembDep == null)
            {
                DetReembDep = new List<DetReembDep>();
            }

            DetReembDep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DetReembDep (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetReembDep</returns>
        public DetReembDep GetDetReembDep(int index)
        {
            if ((DetReembDep?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetReembDep[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetReembDep
        /// </summary>
        public int GetDetReembDepCount => (DetReembDep != null ? DetReembDep.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DetReembDep")]
    [ComVisible(true)]
#endif
    public class DetReembDep : DetReembTit { }

}
