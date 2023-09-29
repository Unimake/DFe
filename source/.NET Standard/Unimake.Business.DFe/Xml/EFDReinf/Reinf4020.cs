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
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf4020")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4020PagtoBeneficiarioPJ/v2_01_02", IsNullable = false)]
    public class Reinf4020 : XMLBase
    {
        [XmlElement("evtRetPJ")]
        public EvtRetPJ EvtRetPJ { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtRetPJ")]
    [ComVisible(true)]
#endif
    public class EvtRetPJ : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEventoReinf4020 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContriReinf4020 IdeContri { get; set; }

        [XmlElement("ideEstab")]
        public IdeEstabReinf4020 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEventoReinf4020")]
    [ComVisible(true)]
#endif
    public class IdeEventoReinf4020 : IdeEventoReinf4010 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeContriReinf4020")]
    [ComVisible(true)]
#endif
    public class IdeContriReinf4020 : IdeContriReinf4010 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstabReinf4020")]
    [ComVisible(true)]
#endif
    public class IdeEstabReinf4020 : IdeEstab
    {
        [XmlElement("ideBenef")]
        public IdeBenefReinf4020 IdeBenef { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeBenefReinf4020")]
    [ComVisible(true)]
#endif
    public class IdeBenefReinf4020
    {
        [XmlElement("cnpjBenef")]
        public string CnpjBenef { get; set; }

        [XmlElement("nmBenef")]
        public string NmBenef { get; set; }

        [XmlElement("isenImun")]
#if INTEROP
        public IsencaoEImunidade IsenImun { get; set; } = (IsencaoEImunidade)(-1);
#else
        public IsencaoEImunidade ? IsenImun { get; set; }
#endif

        [XmlElement("ideEvtAdic")]
        public string IdeEvtAdic { get; set; }

        [XmlElement("idePgto")]
        public List<IdePgtoReinf4020> IdePgto { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdePgto(IdePgtoReinf4020 item)
        {
            if (IdePgto == null)
            {
                IdePgto = new List<IdePgtoReinf4020>();
            }

            IdePgto.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdePgto (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdePgto</returns>
        public IdePgtoReinf4020 GetIdePgto(int index)
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

        #region ShouldSerialize
        public bool ShouldSereializeCnpjBenef() => !string.IsNullOrEmpty(CnpjBenef);

        public bool ShouldSereializeNmBenef() => !string.IsNullOrEmpty(NmBenef);

#if INTEROP
        public bool ShouldSerializeIsenImun() => IsenImun != (IsencaoEImunidade)(-1);
#else
        public bool ShouldSerializeIsenImun() => IsenImun != null;
#endif

        public bool ShouldSereializeIdeEvtAdic() => !string.IsNullOrEmpty(IdeEvtAdic);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdePgtoReinf4020")]
    [ComVisible(true)]
#endif
    public class IdePgtoReinf4020
    {
        [XmlElement("natRend")]
        public string NatRend { get; set; }

        [XmlElement("observ")]
        public string Observ { get; set; }

        [XmlElement("infoPgto")]
        public List<InfoPgtoReinf4020> InfoPgto { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoPgto(InfoPgtoReinf4020 item)
        {
            if (InfoPgto == null)
            {
                InfoPgto = new List<InfoPgtoReinf4020>();
            }

            InfoPgto.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoPgto (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoPgto</returns>
        public InfoPgtoReinf4020 GetInfoPgto(int index)
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

        #region ShouldSerialize

        public bool ShouldSereializeObserv() => !string.IsNullOrEmpty(Observ);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoPgtoReinf4020")]
    [ComVisible(true)]
#endif
    public class InfoPgtoReinf4020
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

        [XmlIgnore]
        public double VlrBruto { get; set; }

        [XmlElement("vlrBruto")]
        public string VlrBrutoField
        {
            get => VlrBruto.ToString("F2", CultureInfoReinf.Info);
            set => VlrBruto = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

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

        [XmlElement("retencoes")]
        public List<Retencoes> Retencoes { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRetencoes(Retencoes item)
        {
            if (Retencoes == null)
            {
                Retencoes = new List<Retencoes>();
            }

            Retencoes.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Retencoes (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Retencoes</returns>
        public Retencoes GetRetencoes(int index)
        {
            if ((Retencoes?.Count ?? 0) == 0)
            {
                return default;
            };

            return Retencoes[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Retencoes
        /// </summary>
        public int GetRetencoesCount => (Retencoes != null ? Retencoes.Count : 0);
#endif

        [XmlElement("infoProcRet")]
        public List<InfoProcRetReinf4020> InfoProcRet { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProcRet(InfoProcRetReinf4020 item)
        {
            if (InfoProcRet == null)
            {
                InfoProcRet = new List<InfoProcRetReinf4020>();
            }

            InfoProcRet.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProcRet (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProcRet</returns>
        public InfoProcRetReinf4020 GetInfoProcRet(int index)
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

        [XmlElement("infoProcJud")]
        public InfoProcJudReinf4020 InfoProcJud { get; set; }

        [XmlElement("infoPgtoExt")]
        public InfoPgtoExtReinf4020 InfoPgtoExt { get; set; }

        #region ShouldSerialize
#if INTEROP
        public bool ShouldSerializeIndFciScp() => IndFciScp != (IndicativoFundoDeInvestimento)(-1);
#else
        public bool ShouldSerializeIndFciScp() => IndFciScp != null;
#endif
        public bool ShouldSereializeIndNrInscFciScp() => !string.IsNullOrEmpty(NrInscFciScp);

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
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Retencoes")]
    [ComVisible(true)]
#endif
    public class Retencoes
    {
        [XmlIgnore]
        public double VlrBaseIR { get; set; }

        [XmlElement("vlrBaseIR")]
        public string VlrBaseIRField
        {
            get => VlrBaseIR.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseIR = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrIR { get; set; }

        [XmlElement("vlrIR")]
        public string VlrIRField
        {
            get => VlrIR.ToString("F2", CultureInfoReinf.Info);
            set => VlrIR = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrBaseAgreg { get; set; }

        [XmlElement("vlrBaseAgreg")]
        public string VlrBaseAgregField
        {
            get => VlrBaseAgreg.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseAgreg = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrAgreg { get; set; }

        [XmlElement("vlrAgreg")]
        public string VlrAgregField
        {
            get => VlrAgreg.ToString("F2", CultureInfoReinf.Info);
            set => VlrAgreg = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrBaseCSLL { get; set; }

        [XmlElement("vlrBaseCSLL")]
        public string VlrBaseCSLLField
        {
            get => VlrBaseCSLL.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseCSLL = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCSLL { get; set; }

        [XmlElement("vlrCSLL")]
        public string VlrCSLLField
        {
            get => VlrCSLL.ToString("F2", CultureInfoReinf.Info);
            set => VlrCSLL = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrBaseCofins { get; set; }

        [XmlElement("vlrBaseCofins")]
        public string VlrBaseCofinsField
        {
            get => VlrBaseCofins.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseCofins = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCofins { get; set; }

        [XmlElement("vlrCofins")]
        public string VlrCofinsField
        {
            get => VlrCofins.ToString("F2", CultureInfoReinf.Info);
            set => VlrCofins = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrBasePP { get; set; }

        [XmlElement("vlrBasePP")]
        public string VlrBasePPField
        {
            get => VlrBasePP.ToString("F2", CultureInfoReinf.Info);
            set => VlrBasePP = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrPP { get; set; }

        [XmlElement("vlrPP")]
        public string VlrPPField
        {
            get => VlrPP.ToString("F2", CultureInfoReinf.Info);
            set => VlrPP = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrBaseIR() => VlrBaseIR > 0;

        public bool ShouldSerializeVlrIR() => VlrIR > 0;

        public bool ShouldSerializeVlrBaseAgreg() => VlrBaseAgreg > 0;

        public bool ShouldSerializeVlrAgreg() => VlrAgreg > 0;

        public bool ShouldSerializeVlrBaseCSLL() => VlrBaseCSLL > 0;

        public bool ShouldSerializeVlrCSLL() => VlrCSLL > 0;

        public bool ShouldSerializeVlrBaseCofins() => VlrBaseCofins > 0;

        public bool ShouldSerializeVlrCofins() => VlrCofins > 0;

        public bool ShouldSerializeVlrBasePP() => VlrBasePP > 0;

        public bool ShouldSerializeVlrPP() => VlrPP > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcRetReinf4020")]
    [ComVisible(true)]
#endif
    public class InfoProcRetReinf4020
    {
        [XmlElement("tpProcRet")]
        public TipoProcesso TpProcRet { get; set; }

        [XmlElement("nrProcRet")]
        public string NrProcRet { get; set; }

        [XmlElement("codSusp")]
        public string CodSusp { get; set; }

        [XmlIgnore]
        public double VlrBaseSuspIR { get; set; }

        [XmlElement("vlrBaseSuspIR")]
        public string VlrBaseSuspIRField
        {
            get => VlrBaseSuspIR.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseSuspIR = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrNIR { get; set; }

        [XmlElement("vlrNIR")]
        public string VlrNIRField
        {
            get => VlrNIR.ToString("F2", CultureInfoReinf.Info);
            set => VlrNIR = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrDepIR { get; set; }

        [XmlElement("vlrDepIR")]
        public string VlrDepIRField
        {
            get => VlrDepIR.ToString("F2", CultureInfoReinf.Info);
            set => VlrDepIR = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrBaseSuspCSLL { get; set; }

        [XmlElement("vlrBaseSuspCSLL")]
        public string VlrBaseSuspCSLLField
        {
            get => VlrBaseSuspCSLL.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseSuspCSLL = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrNCSLL { get; set; }

        [XmlElement("vlrNCSLL")]
        public string VlrNCSLLField
        {
            get => VlrNCSLL.ToString("F2", CultureInfoReinf.Info);
            set => VlrNCSLL = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrDepCSLL { get; set; }

        [XmlElement("vlrDepCSLL")]
        public string VlrDepCSLLField
        {
            get => VlrDepCSLL.ToString("F2", CultureInfoReinf.Info);
            set => VlrDepCSLL = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrBaseSuspCofins { get; set; }

        [XmlElement("vlrBaseSuspCofins")]
        public string VlrBaseSuspCofinsField
        {
            get => VlrBaseSuspCofins.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseSuspCofins = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrNCofins { get; set; }

        [XmlElement("vlrNCofins")]
        public string VlrNCofinsField
        {
            get => VlrNCofins.ToString("F2", CultureInfoReinf.Info);
            set => VlrNCofins = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrDepCofins { get; set; }

        [XmlElement("vlrDepCofins")]
        public string VlrDepCofinsField
        {
            get => VlrDepCofins.ToString("F2", CultureInfoReinf.Info);
            set => VlrDepCofins = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrBaseSuspPP { get; set; }

        [XmlElement("vlrBaseSuspPP")]
        public string VlrBaseSuspPPField
        {
            get => VlrBaseSuspPP.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseSuspPP = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrNPP { get; set; }

        [XmlElement("vlrNPP")]
        public string VlrNPPField
        {
            get => VlrNPP.ToString("F2", CultureInfoReinf.Info);
            set => VlrNPP = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrDepPP { get; set; }

        [XmlElement("vlrDepPP")]
        public string VlrDepPPField
        {
            get => VlrDepPP.ToString("F2", CultureInfoReinf.Info);
            set => VlrDepPP = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSereializeCodSusp() => !string.IsNullOrEmpty(CodSusp);

        public bool ShouldSerializeVlrBaseSuspIR() => VlrBaseSuspIR > 0;

        public bool ShouldSerializeVlrNIR() => VlrNIR > 0;

        public bool ShouldSerializeVlrDepIR() => VlrDepIR > 0;

        public bool ShouldSerializeVlrBaseSuspCSLL() => VlrBaseSuspCSLL > 0;

        public bool ShouldSerializeVlrNCSLL() => VlrNCSLL > 0;

        public bool ShouldSerializeVlrDepCSLL() => VlrDepCSLL > 0;

        public bool ShouldSerializeVlrBaseSuspCofins() => VlrBaseSuspCofins > 0;

        public bool ShouldSerializeVlrNCofins() => VlrNCofins > 0;

        public bool ShouldSerializeVlrDepCofins() => VlrDepCofins > 0;

        public bool ShouldSerializeVlrBaseSuspPP() => VlrBaseSuspPP > 0;

        public bool ShouldSerializeVlrNPP() => VlrNPP > 0;

        public bool ShouldSerializeVlrDepPP() => VlrDepPP > 0;
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcJudReinf4020")]
    [ComVisible(true)]
#endif
    public class InfoProcJudReinf4020 : InfoProcJudReinf4010 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoPgtoExtReinf4020")]
    [ComVisible(true)]
#endif
    public class InfoPgtoExtReinf4020
    {
        [XmlElement("indNIF")]
        public IndicativoNIF IndNIF { get; set; }

        [XmlElement("nifBenef")]
        public string NifBenef { get; set; }

        [XmlElement("relFontPg")]
        public RelacaoFontePagadora RelFontPg { get; set; }

        [XmlElement("frmTribut")]
        public FormaDeTributacao FrmTribut { get; set; }

        [XmlElement("endExt")]
        public EndExt EndExt { get; set; }
    }
}
