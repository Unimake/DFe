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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1202")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtRmnRPPS/v_S_01_02_00", IsNullable = false)]
    public class ESocial1202 : XMLBase
    {
        [XmlElement("evtRmnRPPS")]
        public EvtRmnRPPS EvtRmnRPPS { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtRmnRPPS")]
    [ComVisible(true)]
#endif
    public class EvtRmnRPPS
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEventoESocial1202 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideTrabalhador")]
        public IdeTrabalhadorESocial1202 IdeTrabalhador { get; set; }

        [XmlElement("dmDev")]
        public List<DmDevESocial1202> DmDev { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDmDev(DmDevESocial1202 item)
        {
            if (DmDev == null)
            {
                DmDev = new List<DmDevESocial1202>();
            }

            DmDev.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DmDevESocial1202 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DmDevESocial1202</returns>
        public DmDevESocial1202 GetDmDev(int index)
        {
            if ((DmDev?.Count ?? 0) == 0)
            {
                return default;
            };

            return DmDev[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DmDevESocial1202
        /// </summary>
        public int GetDmDevCount => (DmDev != null ? DmDev.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEventoESocial1202")]
    [ComVisible(true)]
#endif
    public class IdeEventoESocial1202
    {
        [XmlElement("indRetif")]
        public IndicativoRetificacao IndRetif { get; set; }

        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

        [XmlElement("indApuracao")]
        public IndApuracao IndApuracao { get; set; }

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
        public ProcEmiESocial ProcEmi { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNrRecibo() => !string.IsNullOrEmpty(NrRecibo);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhadorESocial1202")]
    [ComVisible(true)]
#endif
    public class IdeTrabalhadorESocial1202
    {
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        [XmlElement("infoComplem")]
        public InfoComplemESocial1202 InfoComplem { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComplemESocial1202")]
    [ComVisible(true)]
#endif
    public class InfoComplemESocial1202
    {
        [XmlElement("nmTrab")]
        public string NmTrab { get; set; }

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

        [XmlElement("sucessaoVinc")]
        public SucessaoVinc SucessaoVinc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SucessaoVinc")]
    [ComVisible(true)]
#endif
    public class SucessaoVinc
    {
        [XmlElement("cnpjOrgaoAnt")]
        public string CnpjOrgaoAnt { get; set; }

        [XmlElement("matricAnt")]
        public string MatricAnt { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtExercicio { get; set; }
#else
        public DateTimeOffset DtExercicio { get; set; }
#endif

        [XmlElement("dtExercicio")]
        public string DtExercicioField
        {
            get => DtExercicio.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtExercicio = DateTime.Parse(value);
#else
            set => DtExercicio = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("observacao")]
        public string Observacao { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeMatricAnt() => !string.IsNullOrEmpty(MatricAnt);

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DmDevESocial1202")]
    [ComVisible(true)]
#endif
    public class DmDevESocial1202
    {
        [XmlElement("ideDmDev")]
        public string IdeDmDev { get; set; }

        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        [XmlElement("indRRA")]
#if INTEROP
        public SimNaoLetra IndRRA { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra ? IndRRA { get; set; }
#endif

        [XmlElement("infoRRA")]
        public InfoRRA InfoRRA { get; set; }

        [XmlElement("infoPerApur")]
        public InfoPerApurEsocial1202 InfoPerApur { get; set; }

        [XmlElement("infoPerAnt")]
        public InfoPerAnt InfoPerAnt { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndRRA() => IndRRA != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndRRA() => IndRRA != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRRA")]
    [ComVisible(true)]
#endif
    public class InfoRRA
    {
        [XmlElement("tpProcRRA")]
        public TipoProcesso TpProcRRA { get; set; }

        [XmlElement("nrProcRRA")]
        public string NrProcRRA { get; set; }

        [XmlElement("descRRA")]
        public string DescRRA { get; set; }

        [XmlElement("qtdMesesRRA")]
        public int QtdMesesRRA { get; set; }

        [XmlElement("despProcJud")]
        public DespProcJudESocial1202 DespProcJud { get; set; }

        [XmlElement("ideAdv")]
        public List<IdeAdvESocial1202> IdeAdv { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeAdv(IdeAdvESocial1202 item)
        {
            if (IdeAdv == null)
            {
                IdeAdv = new List<IdeAdvESocial1202>();
            }

            IdeAdv.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeAdv (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeAdv</returns>
        public IdeAdvESocial1202 GetIdeAdv(int index)
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

        #region ShouldSerialize

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(NrProcRRA);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DespProcJudESocial1202")]
    [ComVisible(true)]
#endif
    public class DespProcJudESocial1202
    {
        [XmlElement("vlrDespCustas")]
        public double VlrDespCustas { get; set; }

        [XmlElement("vlrDespAdvogados")]
        public double VlrDespAdvogados { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeAdvESocial1202")]
    [ComVisible(true)]
#endif
    public class IdeAdvESocial1202
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("vlrAdv")]
        public double VlrAdv { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVlrAdv() => VlrAdv > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPerApurEsocial1202")]
    [ComVisible(true)]
#endif
    public class InfoPerApurEsocial1202
    {
        [XmlElement("ideEstab")]
        public IdeEstabEsocial1202 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabEsocial1202")]
    [ComVisible(true)]
#endif
    public class IdeEstabEsocial1202
    {
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("remunPerApur")]
        public RemunPerApurESocial1202 RemunPerApur { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunPerApurESocial1202")]
    [ComVisible(true)]
#endif
    public class RemunPerApurESocial1202
    {
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        [XmlElement("itensRemun")]
        public ItensRemunESocial1202 ItensRemun { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ItensRemunESocial1202")]
    [ComVisible(true)]
#endif
    public class ItensRemunESocial1202
    {
        [XmlElement("codRubr")]
        public string CodRubr { get; set; }

        [XmlElement("ideTabRubr")]
        public string IdeTabRubr { get; set; }

        [XmlElement("qtdRubr")]
        public double QtdRubr { get; set; }

        [XmlElement("fatorRubr")]
        public double FatorRubr { get; set; }

        [XmlElement("vrRubr")]
        public double VrRubr { get; set; }

        [XmlElement("indApurIR")]
        public IndApurIR IndApurIR { get; set; }

        #region ShouldSerialize
        
        public bool ShouldSerializeQtdRubr() => QtdRubr > 0;
       
        public bool ShouldSerializeFatorRubr() => FatorRubr > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPerAnt")]
    [ComVisible(true)]
#endif
    public class InfoPerAnt
    {
        [XmlElement("remunOrgSuc")]
        public SimNaoOpcionalLetra RemunOrgSuc { get; set; }

        [XmlElement("idePeriodo")]
        public List<IdePeriodoEsocial1202> IdePeriodo { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdePeriodo(IdePeriodoEsocial1202 item)
        {
            if (IdePeriodo == null)
            {
                IdePeriodo = new List<IdePeriodoEsocial1202>();
            }

            IdePeriodo.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdePeriodoEsocial1202 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdePeriodoEsocial1202</returns>
        public IdePeriodoEsocial1202 GetIdePeriodo(int index)
        {
            if ((IdePeriodo?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdePeriodo[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdePeriodoEsocial1202
        /// </summary>
        public int GetIdePeriodoCount => (IdePeriodo != null ? IdePeriodo.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdePeriodoEsocial1202")]
    [ComVisible(true)]
#endif
    public class IdePeriodoEsocial1202
    {
        [XmlIgnore]
        public DateTimeOffset PerRef { get; set; }

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

        [XmlElement("ideEstab")]
        public List<IdeEstabESocial1202> IdeEstab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstab(IdeEstabESocial1202 item)
        {
            if (IdeEstab == null)
            {
                IdeEstab = new List<IdeEstabESocial1202>();
            }

            IdeEstab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstabESocial1202 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstabESocial1202</returns>
        public IdeEstabESocial1202 GetIdeEstab(int index)
        {
            if ((IdeEstab?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeEstab[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeEstabESocial1202
        /// </summary>
        public int GetIdeEstabCount => (IdeEstab != null ? IdeEstab.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabESocial1202")]
    [ComVisible(true)]
#endif
    public class IdeEstabESocial1202
    {
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("remunPerAnt")]
        public List<RemunPerAnt> RemunPerAnt { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRemunPerAnt(RemunPerAnt item)
        {
            if (RemunPerAnt == null)
            {
                RemunPerAnt = new List<RemunPerAnt>();
            }

            RemunPerAnt.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RemunPerAnt (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RemunPerAnt</returns>
        public RemunPerAnt GetRemunPerAnt(int index)
        {
            if ((RemunPerAnt?.Count ?? 0) == 0)
            {
                return default;
            };

            return RemunPerAnt[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RemunPerAnt
        /// </summary>
        public int GetRemunPerAntCount => (RemunPerAnt != null ? RemunPerAnt.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunPerAnt")]
    [ComVisible(true)]
#endif
    public class RemunPerAnt
    {
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        [XmlElement("itensRemun")]
        public List<ItensRemunESocial1202> ItensRemun { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddItensRemun(ItensRemunESocial1202 item)
        {
            if (ItensRemun == null)
            {
                ItensRemun = new List<ItensRemunESocial1202>();
            }

            ItensRemun.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ItensRemun (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ItensRemun</returns>
        public ItensRemunESocial1202 GetItensRemun(int index)
        {
            if ((ItensRemun?.Count ?? 0) == 0)
            {
                return default;
            };

            return ItensRemun[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ItensRemunESocial1202
        /// </summary>
        public int GetItensRemunCount => (ItensRemun != null ? ItensRemun.Count : 0);
#endif
    }
}
