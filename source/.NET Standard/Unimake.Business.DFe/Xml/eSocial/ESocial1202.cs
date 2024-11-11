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
        public IdeEvento1202 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideTrabalhador")]
        public IdeTrabalhador1202 IdeTrabalhador { get; set; }

        [XmlElement("dmDev")]
        public List<DmDev1202> DmDev { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDmDev(DmDev1202 item)
        {
            if (DmDev == null)
            {
                DmDev = new List<DmDev1202>();
            }

            DmDev.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DmDev1202 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DmDev1202</returns>
        public DmDev1202 GetDmDev(int index)
        {
            if ((DmDev?.Count ?? 0) == 0)
            {
                return default;
            };

            return DmDev[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DmDev1202
        /// </summary>
        public int GetDmDevCount => (DmDev != null ? DmDev.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1202")]
    [ComVisible(true)]
#endif
    public class IdeEvento1202
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhador1202")]
    [ComVisible(true)]
#endif
    public class IdeTrabalhador1202
    {
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        [XmlElement("infoComplem")]
        public InfoComplem1202 InfoComplem { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComplem1202")]
    [ComVisible(true)]
#endif
    public class InfoComplem1202
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

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DmDev1202")]
    [ComVisible(true)]
#endif
    public class DmDev1202
    {
        [XmlElement("ideDmDev")]
        public string IdeDmDev { get; set; }

        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        [XmlElement("indRRA")]
#if INTEROP
        public SimNaoLetra IndRRA { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndRRA { get; set; }
#endif

        /// <summary>
        /// Informações complementares relativas a Rendimentos Recebidos Acumuladamente - RRA.
        /// </summary>
        [XmlElement("infoRRA")]
        public InfoRRA1202 InfoRRA { get; set; }

        [XmlElement("infoPerApur")]
        public InfoPerApur1202 InfoPerApur { get; set; }

        [XmlElement("infoPerAnt")]
        public InfoPerAnt InfoPerAnt { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndRRA() => IndRRA != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndRRA() => IndRRA != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações complementares relativas a Rendimentos Recebidos Acumuladamente - RRA.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRRA1202")]
    [ComVisible(true)]
#endif
    public class InfoRRA1202
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
        public DespProcJud1202 DespProcJud { get; set; }

        [XmlElement("ideAdv")]
        public List<IdeAdv1202> IdeAdv { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeAdv(IdeAdv1202 item)
        {
            if (IdeAdv == null)
            {
                IdeAdv = new List<IdeAdv1202>();
            }

            IdeAdv.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeAdv (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeAdv</returns>
        public IdeAdv1202 GetIdeAdv(int index)
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

        public bool ShouldSerializeNrProcRRA() => !string.IsNullOrEmpty(NrProcRRA);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DespProcJud1202")]
    [ComVisible(true)]
#endif
    public class DespProcJud1202
    {
        [XmlIgnore]
        public double VlrDespCustas { get; set; }

        [XmlElement("vlrDespCustas")]
        public string VlrDespCustasField
        {
            get => VlrDespCustas.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDespCustas = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VlrDespAdvogados { get; set; }

        [XmlElement("vlrDespAdvogados")]
        public string VlrDespAdvogadosField
        {
            get => VlrDespAdvogados.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDespAdvogados = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeAdv1202")]
    [ComVisible(true)]
#endif
    public class IdeAdv1202
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlIgnore]
        public double VlrAdv { get; set; }

        [XmlElement("vlrAdv")]
        public string VlrAdvField
        {
            get => VlrAdv.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrAdv = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrAdvField() => VlrAdv > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPerApur1202")]
    [ComVisible(true)]
#endif
    public class InfoPerApur1202
    {
        [XmlElement("ideEstab")]
        public IdeEstab1202 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunPerApur1202")]
    [ComVisible(true)]
#endif
    public class RemunPerApur1202 : RemunPerApur1200 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ItensRemun1202")]
    [ComVisible(true)]
#endif
    public class ItensRemun1202 : ItensRemun { }

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
        public List<IdePeriodo1202> IdePeriodo { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdePeriodo(IdePeriodo1202 item)
        {
            if (IdePeriodo == null)
            {
                IdePeriodo = new List<IdePeriodo1202>();
            }

            IdePeriodo.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdePeriodo1202 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdePeriodo1202</returns>
        public IdePeriodo1202 GetIdePeriodo(int index)
        {
            if ((IdePeriodo?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdePeriodo[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdePeriodo1202
        /// </summary>
        public int GetIdePeriodoCount => (IdePeriodo != null ? IdePeriodo.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdePeriodo1202")]
    [ComVisible(true)]
#endif
    public class IdePeriodo1202
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
        public List<IdeEstab1202> IdeEstab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstab(IdeEstab1202 item)
        {
            if (IdeEstab == null)
            {
                IdeEstab = new List<IdeEstab1202>();
            }

            IdeEstab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstab1202 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstab1202</returns>
        public IdeEstab1202 GetIdeEstab(int index)
        {
            if ((IdeEstab?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeEstab[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeEstab1202
        /// </summary>
        public int GetIdeEstabCount => (IdeEstab != null ? IdeEstab.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstab1202")]
    [ComVisible(true)]
#endif
    public class IdeEstab1202
    {
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("remunPerAnt")]
        public List<RemunPerAnt> RemunPerAnt { get; set; }

        [XmlElement("remunPerApur")]
        public RemunPerApur1202 RemunPerApur { get; set; }

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
        public List<ItensRemun1202> ItensRemun { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddItensRemun(ItensRemun1202 item)
        {
            if (ItensRemun == null)
            {
                ItensRemun = new List<ItensRemun1202>();
            }

            ItensRemun.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ItensRemun (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ItensRemun</returns>
        public ItensRemun1202 GetItensRemun(int index)
        {
            if ((ItensRemun?.Count ?? 0) == 0)
            {
                return default;
            };

            return ItensRemun[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ItensRemun1202
        /// </summary>
        public int GetItensRemunCount => (ItensRemun != null ? ItensRemun.Count : 0);
#endif
    }
}
