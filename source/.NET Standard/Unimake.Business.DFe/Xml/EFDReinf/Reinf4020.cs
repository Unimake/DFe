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
    /// R-4020 - Pagamentos/créditos a beneficiário pessoa jurídica
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf4020")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4020PagtoBeneficiarioPJ/v2_01_02", IsNullable = false)]
    public class Reinf4020 : XMLBase
    {
        /// <summary>
        /// Evento retenções na fonte PJ
        /// </summary>
        [XmlElement("evtRetPJ")]
        public EvtRetPJ EvtRetPJ { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Pagamentos/créditos a beneficiário pessoa jurídica
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtRetPJ")]
    [ComVisible(true)]
#endif
    public class EvtRetPJ : ReinfEventoBase
    {
        /// <summary>
        /// Informações de identificação do evento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento4020 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do Contribuinte
        /// </summary>
        [XmlElement("ideContri")]
        public IdeContri4020 IdeContri { get; set; }

        /// <summary>
        /// Informações de identificação do estabelecimento
        /// </summary>
        [XmlElement("ideEstab")]
        public IdeEstab4020 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEvento4020")]
    [ComVisible(true)]
#endif
    public class IdeEvento4020 : IdeEvento4010 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeContri4020")]
    [ComVisible(true)]
#endif
    public class IdeContri4020 : IdeContri4010 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstab4020")]
    [ComVisible(true)]
#endif
    public class IdeEstab4020 : IdeEstab2030
    {
        /// <summary>
        /// Identificação do beneficiário do rendimento
        /// </summary>
        [XmlElement("ideBenef")]
        public IdeBenef4020 IdeBenef { get; set; }
    }

    /// <summary>
    /// Identificação do beneficiário do rendimento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeBenef4020")]
    [ComVisible(true)]
#endif
    public class IdeBenef4020
    {
        [XmlElement("cnpjBenef")]
        public string CnpjBenef { get; set; }

        [XmlElement("nmBenef")]
        public string NmBenef { get; set; }

        [XmlElement("isenImun")]
#if INTEROP
        public IsencaoEImunidade IsenImun { get; set; } = (IsencaoEImunidade)(-1);
#else
        public IsencaoEImunidade? IsenImun { get; set; }
#endif

        [XmlElement("ideEvtAdic")]
        public string IdeEvtAdic { get; set; }

        /// <summary>
        /// Informações relativas ao rendimento pago/creditado
        /// </summary>
        [XmlElement("idePgto")]
        public List<IdePgto4020> IdePgto { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdePgto(IdePgto4020 item)
        {
            if (IdePgto == null)
            {
                IdePgto = new List<IdePgto4020>();
            }

            IdePgto.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdePgto (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdePgto</returns>
        public IdePgto4020 GetIdePgto(int index)
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
        public bool ShouldSerializeCnpjBenef() => !string.IsNullOrEmpty(CnpjBenef);

        public bool ShouldSerializeNmBenef() => !string.IsNullOrEmpty(NmBenef);

#if INTEROP
        public bool ShouldSerializeIsenImun() => IsenImun != (IsencaoEImunidade)(-1);
#else
        public bool ShouldSerializeIsenImun() => IsenImun != null;
#endif

        public bool ShouldSerializeIdeEvtAdic() => !string.IsNullOrEmpty(IdeEvtAdic);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações de identificação do pagamento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdePgto4020")]
    [ComVisible(true)]
#endif
    public class IdePgto4020
    {
        [XmlElement("natRend")]
        public string NatRend { get; set; }

        [XmlElement("observ")]
        public string Observ { get; set; }

        /// <summary>
        /// Informações relativas ao rendimento pago/creditado
        /// </summary>
        [XmlElement("infoPgto")]
        public List<InfoPgto4020> InfoPgto { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoPgto(InfoPgto4020 item)
        {
            if (InfoPgto == null)
            {
                InfoPgto = new List<InfoPgto4020>();
            }

            InfoPgto.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoPgto (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoPgto</returns>
        public InfoPgto4020 GetInfoPgto(int index)
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

        public bool ShouldSerializeObserv() => !string.IsNullOrEmpty(Observ);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações relativas ao rendimento pago/creditado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoPgto4020")]
    [ComVisible(true)]
#endif
    public class InfoPgto4020
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
        public IndicativoFundoDeInvestimento? IndFciScp { get; set; }
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

        /// <summary>
        /// Informações relativas a retenções na fonte
        /// </summary>
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
        public List<InfoProcRet4020> InfoProcRet { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProcRet(InfoProcRet4020 item)
        {
            if (InfoProcRet == null)
            {
                InfoProcRet = new List<InfoProcRet4020>();
            }

            InfoProcRet.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProcRet (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProcRet</returns>
        public InfoProcRet4020 GetInfoProcRet(int index)
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
        public InfoProcJud4020 InfoProcJud { get; set; }

        [XmlElement("infoPgtoExt")]
        public InfoPgtoExt4020 InfoPgtoExt { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVlrBrutoField() => VlrBruto > 0;

#if INTEROP
        public bool ShouldSerializeIndFciScp() => IndFciScp != (IndicativoFundoDeInvestimento)(-1);
#else
        public bool ShouldSerializeIndFciScp() => IndFciScp != null;
#endif
        public bool ShouldSerializeIndNrInscFciScp() => !string.IsNullOrEmpty(NrInscFciScp);

        public bool ShouldSerializePercSCP() => !string.IsNullOrEmpty(PercSCP);

#if INTEROP
        public bool ShouldSerializeIndJud() => IndJud != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndJud() => IndJud != null;
#endif

        public bool ShouldSerializePaisResidExt() => !string.IsNullOrEmpty(PaisResidExt);

        public bool ShouldSerializeDtEscrContField() => DtEscrCont > DateTime.MinValue;

        public bool ShouldSerializeObserv() => !string.IsNullOrEmpty(Observ);
        
        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações relativas a retenções na fonte
    /// </summary>
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

        /// <summary>
        /// Valor da base do PIS/PASEP, relativa à retenção efetivamente realizada.
        /// Validação: Informação permitida apenas se, para a natureza do rendimento
        /// informada em {natRend}, houver "PP" na coluna "Tributo" da Tabela 01.
        /// Não pode ser informado se {vlrBaseAgreg}
        /// for informado.
        /// Se informado, deve ser maior que zero.
        /// </summary>
        [XmlElement("vlrBasePP")]
        public string VlrBasePPField
        {
            get => VlrBasePP.ToString("F2", CultureInfoReinf.Info);
            set => VlrBasePP = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrPP { get; set; }

        /// <summary>
        /// Valor da retenção do PIS/PASEP.
        /// Validação: Informação obrigatória se {vlrBasePP} for informado.
        /// Informação permitida apenas se, para a natureza do rendimento informada em
        /// {natRend}, houver "PP" na coluna "Tributo" da Tabela 01.
        /// Se informado, deve ser maior que zero.
        /// </summary>
        [XmlElement("vlrPP")]
        public string VlrPPField
        {
            get => VlrPP.ToString("F2", CultureInfoReinf.Info);
            set => VlrPP = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrBaseIRField() => VlrBaseIR > 0;

        public bool ShouldSerializeVlrIRField() => VlrIR > 0;

        public bool ShouldSerializeVlrBaseAgregField() => VlrBaseAgreg > 0;

        public bool ShouldSerializeVlrAgregField() => VlrAgreg > 0;

        public bool ShouldSerializeVlrBaseCSLLField() => VlrBaseCSLL > 0;

        public bool ShouldSerializeVlrCSLLField() => VlrCSLL > 0;

        public bool ShouldSerializeVlrBaseCofinsField() => VlrBaseCofins > 0;

        public bool ShouldSerializeVlrCofinsField() => VlrCofins > 0;

        public bool ShouldSerializeVlrBasePPField() => VlrBasePP > 0;

        public bool ShouldSerializeVlrPPField() => VlrPP > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcRet4020")]
    [ComVisible(true)]
#endif
    public class InfoProcRet4020
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

        public bool ShouldSerializeCodSusp() => !string.IsNullOrEmpty(CodSusp);

        public bool ShouldSerializeVlrBaseSuspIRField() => VlrBaseSuspIR > 0;

        public bool ShouldSerializeVlrNIRField() => VlrNIR > 0;

        public bool ShouldSerializeVlrDepIRField() => VlrDepIR > 0;

        public bool ShouldSerializeVlrBaseSuspCSLLField() => VlrBaseSuspCSLL > 0;

        public bool ShouldSerializeVlrNCSLLField() => VlrNCSLL > 0;

        public bool ShouldSerializeVlrDepCSLLField() => VlrDepCSLL > 0;

        public bool ShouldSerializeVlrBaseSuspCofinsField() => VlrBaseSuspCofins > 0;

        public bool ShouldSerializeVlrNCofinsField() => VlrNCofins > 0;

        public bool ShouldSerializeVlrDepCofinsField() => VlrDepCofins > 0;

        public bool ShouldSerializeVlrBaseSuspPPField() => VlrBaseSuspPP > 0;

        public bool ShouldSerializeVlrNPPField() => VlrNPP > 0;

        public bool ShouldSerializeVlrDepPPField() => VlrDepPP > 0;
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcJud4020")]
    [ComVisible(true)]
#endif
    public class InfoProcJud4020 : InfoProcJud4010 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoPgtoExt4020")]
    [ComVisible(true)]
#endif
    public class InfoPgtoExt4020
    {
        [XmlElement("indNIF")]
        public IndicativoNIF IndNIF { get; set; }

        [XmlElement("nifBenef")]
        public string NifBenef { get; set; }

        [XmlElement("relFontPg")]
        public RelacaoFontePagadora RelFontPg { get; set; }

        [XmlElement("frmTribut")]
        public FormaDeTributacao FrmTribut { get; set; }

        /// <summary>
        /// Endereço do beneficiário residente ou domiciliado no exterior
        /// </summary>
        [XmlElement("endExt")]
        public EndExt EndExt { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNifBenef() => !string.IsNullOrEmpty(NifBenef);

        #endregion ShouldSerialize
    }
}
