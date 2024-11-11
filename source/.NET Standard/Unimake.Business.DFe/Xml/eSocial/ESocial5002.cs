#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial5002")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtIrrfBenef/v_S_01_02_00", IsNullable = false)]
    public class ESocial5002 : XMLBase
    {
        [XmlElement("evtIrrfBenef")]
        public EvtIrrfBenef EvtIrrfBenef { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtIrrfBenef")]
    [ComVisible(true)]
#endif
    public class EvtIrrfBenef
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEvento5002 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideTrabalhador")]
        public IdeTrabalhador5002 IdeTrabalhador { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento5002")]
    [ComVisible(true)]
#endif
    public class IdeEvento5002
    {
        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }

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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhador5002")]
    [ComVisible(true)]
#endif
    public class IdeTrabalhador5002
    {
        [XmlElement("cpfBenef")]
        public string CpfBenef { get; set; }

        [XmlElement("dmDev")]
        public List<DmDev5002> DmDev { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDmDev(DmDev5002 item)
        {
            if (DmDev == null)
            {
                DmDev = new List<DmDev5002>();
            }

            DmDev.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DmDev (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DmDev</returns>
        public DmDev5002 GetDmDev(int index)
        {
            if ((DmDev?.Count ?? 0) == 0)
            {
                return default;
            };

            return DmDev[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DmDev
        /// </summary>
        public int GetDmDevCount => (DmDev != null ? DmDev.Count : 0);
#endif

        [XmlElement("infoIRComplem")]
        public InfoIRComplem5002 InfoIRComplem { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DmDev5002")]
    [ComVisible(true)]
#endif
    public class DmDev5002
    {
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

        [XmlElement("tpPgto")]
        public TipoPagamentoESocial TpPgto { get; set; }

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

        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        [XmlElement("infoIR")]
        public List<InfoIR5002> InfoIR { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoIR(InfoIR5002 item)
        {
            if (InfoIR == null)
            {
                InfoIR = new List<InfoIR5002>();
            }

            InfoIR.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoIR (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoIR</returns>
        public InfoIR5002 GetInfoIR(int index)
        {
            if ((InfoIR?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoIR[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoIR
        /// </summary>
        public int GetInfoIRCount => (InfoIR != null ? InfoIR.Count : 0);
#endif

        [XmlElement("totApurMen")]
        public List<TotApurMen5002> TotApurMen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurMen(TotApurMen5002 item)
        {
            if (TotApurMen == null)
            {
                TotApurMen = new List<TotApurMen5002>();
            }

            TotApurMen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurMen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurMen</returns>
        public TotApurMen5002 GetTotApurMen(int index)
        {
            if ((TotApurMen?.Count ?? 0) == 0)
            {
                return default;
            };

            return TotApurMen[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TotApurMen
        /// </summary>
        public int GetTotApurMenCount => (TotApurMen != null ? TotApurMen.Count : 0);
#endif

        [XmlElement("totApurDia")]
        public List<TotApurDia5002> TotApurDia { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurDia(TotApurDia5002 item)
        {
            if (TotApurDia == null)
            {
                TotApurDia = new List<TotApurDia5002>();
            }

            TotApurDia.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurDia (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurDia</returns>
        public TotApurDia5002 GetTotApurDia(int index)
        {
            if ((TotApurDia?.Count ?? 0) == 0)
            {
                return default;
            };

            return TotApurDia[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TotApurDia
        /// </summary>
        public int GetTotApurDiaCount => (TotApurDia != null ? TotApurDia.Count : 0);
#endif

        [XmlElement("infoRRA")]
        public InfoRRA5002 InfoRRA { get; set; }

        [XmlElement("infoPgtoExt")]
        public InfoPgtoExt5002 InfoPgtoExt { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoIR5002")]
    [ComVisible(true)]
#endif
    public class InfoIR5002
    {
        [XmlElement("tpInfoIR")]
        public string TpInfoIR { get; set; }

        /// <summary>
        /// Composição do valor do rendimento tributável, não tributável,
        /// retenção, dedução ou isenção do IRRF, de acordo com a
        /// classificação apresentada no campo tpInfoIR.
        /// </summary>
        [XmlIgnore]
        public double Valor { get; set; }
        [XmlElement("valor")]
        public string ValorField
        {
            get => Valor.ToString("F2", CultureInfo.InvariantCulture);
            set => Valor = Converter.ToDouble(value);
        }

        [XmlElement("infoProcJudRub")]
        public List<InfoProcJudRub> InfoProcJudRub { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProcJudRub(InfoProcJudRub item)
        {
            if (InfoProcJudRub == null)
            {
                InfoProcJudRub = new List<InfoProcJudRub>();
            }

            InfoProcJudRub.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProcJudRub (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProcJudRub</returns>
        public InfoProcJudRub GetInfoProcJudRub(int index)
        {
            if ((InfoProcJudRub?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoProcJudRub[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoProcJudRub
        /// </summary>
        public int GetInfoProcJudRubCount => (InfoProcJudRub != null ? InfoProcJudRub.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcJudRub")]
    [ComVisible(true)]
#endif
    public class InfoProcJudRub
    {
        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        [XmlElement("ufVara")]
        public UFBrasil UfVara { get; set; }

        [XmlElement("codMunic")]
        public string CodMunic { get; set; }

        [XmlElement("idVara")]
        public string IdVara { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TotApurMen5002")]
    [ComVisible(true)]
#endif
    public class TotApurMen5002
    {
        [XmlElement("CRMen")]
        public string CRMen { get; set; }

        /// <summary>
        /// Valor relativo ao Imposto de Renda Retido
        /// na Fonte sobre rendimentos do trabalho.
        /// </summary>
        [XmlIgnore]
        public double VlrCRMen { get; set; }
        [XmlElement("vlrCRMen")]
        public string VlrCRMenField
        {
            get => VlrCRMen.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrCRMen = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TotApurDia5002")]
    [ComVisible(true)]
#endif
    public class TotApurDia5002
    {
        [XmlIgnore]
#if INTEROP
        public DateTime PerApurDia { get; set; }
#else
        public DateTimeOffset PerApurDia { get; set; }
#endif

        [XmlElement("perApurDia")]
        public string PerApurDiaField
        {
            get => PerApurDia.ToString("dd");
#if INTEROP
            set => PerApurDia = DateTime.Parse(value);
#else
            set => PerApurDia = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("CRDia")]
        public string CRDia { get; set; }

        /// <summary>
        /// Valor relativo ao Imposto de Renda Retido na Fonte
        /// sobre rendimentos do trabalho pagos a residente,
        /// para fins fiscais, no exterior.
        /// </summary>
        [XmlIgnore]
        public double VlrCRDia { get; set; }
        [XmlElement("vlrCRDia")]
        public string VlrCRDiaField
        {
            get => VlrCRDia.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrCRDia = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRRA5002")]
    [ComVisible(true)]
#endif
    public class InfoRRA5002
    {
        [XmlElement("tpProcRRA")]
        public TipoProcesso TpProcRRA { get; set; }

        [XmlElement("nrProcRRA")]
        public string NrProcRRA { get; set; }

        [XmlElement("descRRA")]
        public string DescRRA { get; set; }

        [XmlElement("qtdMesesRRA")]
        public string QtdMesesRRA { get; set; }

        [XmlElement("despProcJud")]
        public DespProcJud5002 DespProcJud { get; set; }

        [XmlElement("ideAdv")]
        public List<IdeAdv5002> IdeAdv { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeAdv(IdeAdv5002 item)
        {
            if (IdeAdv == null)
            {
                IdeAdv = new List<IdeAdv5002>();
            }

            IdeAdv.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeAdv (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeAdv</returns>
        public IdeAdv5002 GetIdeAdv(int index)
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DespProcJud5002")]
    [ComVisible(true)]
#endif
    public class DespProcJud5002
    {
        [XmlElement("vlrDespCustas")]
        public double VlrDespCustas { get; set; }

        [XmlElement("vlrDespAdvogados")]
        public double VlrDespAdvogados { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeAdv5002")]
    [ComVisible(true)]
#endif
    public class IdeAdv5002
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Valor da despesa com o advogado, se houver.
        /// </summary>
        [XmlIgnore]
        public double VlrAdv { get; set; }
        [XmlElement("vlrAdv")]
        public string VlrAdvField
        {
            get => VlrAdv.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrAdv = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShoulSerializeVlrAdvField() => VlrAdv > 0;

        #endregion ShouldSerialize

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPgtoExt5002")]
    [ComVisible(true)]
#endif
    public class InfoPgtoExt5002
    {
        [XmlElement("paisResidExt")]
        public string PaisResidExt { get; set; }

        [XmlElement("indNIF")]
        public IndicativoNIF IndNIF { get; set; }

        [XmlElement("nifBenef")]
        public string NifBenef { get; set; }

        [XmlElement("frmTribut")]
        public FrmTribut FrmTribut { get; set; }

        [XmlElement("endExt")]
        public EndExt5002 EndExt { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EndExt5002")]
    [ComVisible(true)]
#endif
    public class EndExt5002 : EndExt1210 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoIRComplem5002")]
    [ComVisible(true)]
#endif
    public class InfoIRComplem5002
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

        [XmlElement("ideAdv")]
        public List<IdeDep5002> IdeDep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeAdv(IdeDep5002 item)
        {
            if (IdeDep == null)
            {
                IdeDep = new List<IdeDep5002>();
            }

            IdeDep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeDep (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeDep</returns>
        public IdeDep5002 GetIdeDep(int index)
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

        [XmlElement("infoIRCR")]
        public List<InfoIRCR5002> InfoIRCR { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoIRCR(InfoIRCR5002 item)
        {
            if (InfoIRCR == null)
            {
                InfoIRCR = new List<InfoIRCR5002>();
            }

            InfoIRCR.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoIRCR (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoIRCR</returns>
        public InfoIRCR5002 GetInfoIRCR(int index)
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeDep5002")]
    [ComVisible(true)]
#endif
    public class IdeDep5002
    {
        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        [XmlElement("depIRRF")]
        public SimNaoLetra DepIRRF { get; set; }

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

        [XmlElement("tpDep")]
        public string TpDep { get; set; }

        [XmlElement("descrDep")]
        public string DescrDep { get; set; }

        [XmlElement("planSaude")]
        public List<PlanSaude1210> PlanSaude { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPlanSaude(PlanSaude1210 item)
        {
            if (PlanSaude == null)
            {
                PlanSaude = new List<PlanSaude1210>();
            }

            PlanSaude.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PlanSaude1210 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PlanSaude</returns>
        public PlanSaude1210 GetPlanSaude(int index)
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PlanSaude5502")]
    [ComVisible(true)]
#endif
    public class PlanSaude5502 : PlanSaude1210 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoIRCR5002")]
    [ComVisible(true)]
#endif
    public class InfoIRCR5002
    {
        [XmlElement("tpCR")]
        public TpCR TpCR { get; set; }

        [XmlElement("dedDepen")]
        public List<DedDepen5002> DedDepen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDedDepen(DedDepen5002 item)
        {
            if (DedDepen == null)
            {
                DedDepen = new List<DedDepen5002>();
            }

            DedDepen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DedDepen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DedDepen</returns>
        public DedDepen5002 GetDedDepen(int index)
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
        public List<PenAlim5002> PenAlim { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPenAlim(PenAlim5002 item)
        {
            if (PenAlim == null)
            {
                PenAlim = new List<PenAlim5002>();
            }

            PenAlim.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PenAlim (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PenAlim</returns>
        public PenAlim5002 GetPenAlim(int index)
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
        public List<PrevidCompl5002> PrevidCompl { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPrevidCompl(PrevidCompl5002 item)
        {
            if (PrevidCompl == null)
            {
                PrevidCompl = new List<PrevidCompl5002>();
            }

            PrevidCompl.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PrevidCompl5002 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PrevidCompl</returns>
        public PrevidCompl5002 GetPrevidCompl(int index)
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
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PrevidCompl5002")]
    [ComVisible(true)]
#endif
    public class PrevidCompl5002 : PrevidCompl1210 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DedDepen5002")]
    [ComVisible(true)]
#endif
    public class DedDepen5002
    {
        [XmlElement("tpRend")]
        public TipoDeRendimento TpRend { get; set; }

        [XmlElement("cpfDep")]
        public int CpfDep { get; set; }

        /// <summary>
        /// Preencher com o valor da dedução da base de cálculo.
        /// </summary>
        [XmlIgnore]
        public double VlrDedDep { get; set; }
        [XmlElement("vlrDedDep")]
        public string VlrDedDepField
        {
            get => VlrDedDep.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDedDep = Converter.ToDouble(value);
        }
    }

    public class PenAlim5002
    {
        [XmlElement("tpRend")]
        public TipoDeRendimento TpRend { get; set; }

        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        [XmlIgnore]
        public double VlrDedPenAlim { get; set; }

        [XmlElement("vlrDedPenAlim")]
        public string VlrDedPenAlimField
        {
            get => VlrDedPenAlim.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDedPenAlim = Converter.ToDouble(value);
        }
    }
}
