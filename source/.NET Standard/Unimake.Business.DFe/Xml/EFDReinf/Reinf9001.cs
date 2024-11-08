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
    /// R-9001 - Bases e tributos - contribuição previdenciária
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf9001")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTotal/v2_01_02", IsNullable = false)]
    public class Reinf9001 : XMLBase
    {
        /// <summary>
        ///  Evento totalização 
        /// </summary>
        [XmlElement("evtTotal")]
        public EvtTotal EvtTotal { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    ///  Evento totalização 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtTotal")]
    [ComVisible(true)]
#endif
    public class EvtTotal : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEvento9001 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("ideRecRetorno")]
        public IdeRecRetorno9001 IdeRecRetorno { get; set; }

        [XmlElement("infoRecEv")]
        public InfoRecEv InfoRecEv { get; set; }

        [XmlElement("infoTotal")]
        public InfoTotal InfoTotal { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEvento9001")]
    [ComVisible(true)]
#endif
    public class IdeEvento9001
    {
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

    /// <summary>
    /// Informações do recibo de retorno
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeRecRetorno9001")]
    [ComVisible(true)]
#endif
    public class IdeRecRetorno9001
    {
        /// <summary>
        /// Situação atual do evento
        /// </summary>
        [XmlElement("ideStatus")]
        public IdeStatus IdeStatus { get; set; }
    }

    /// <summary>
    /// Situação atual do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeStatus")]
    [ComVisible(true)]
#endif
    public class IdeStatus
    {
        [XmlElement("cdRetorno")]
        public CodigoDoRetorno CdRetorno { get; set; }

        [XmlElement("descRetorno")]
        public string DescRetorno { get; set; }

        [XmlElement("regOcorrs")]
        public List<RegOcorrs> RegOcorrs { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRegOcorrs(RegOcorrs item)
        {
            if (RegOcorrs == null)
            {
                RegOcorrs = new List<RegOcorrs>();
            }

            RegOcorrs.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RegOcorrs (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RegOcorrs</returns>
        public RegOcorrs GetRegOcorrs(int index)
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RegOcorrs")]
    [ComVisible(true)]
#endif
    public class RegOcorrs
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
        public bool ShouldSerializeLocalErroAviso() => !string.IsNullOrEmpty(LocalErroAviso);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoRecEv")]
    [ComVisible(true)]
#endif
    public class InfoRecEv
    {
        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }

        [XmlElement("nrProtEntr")]
        public string NrProtEntr { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhRecepcao { get; set; }
#else
        public DateTimeOffset DhRecepcao { get; set; }
#endif

        [XmlElement("dhRecepcao")]
        public string DhRecepcaoField
        {
            get => DhRecepcao.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRecepcao = DateTime.Parse(value);
#else
            set => DhRecepcao = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DhProcess { get; set; }
#else
        public DateTimeOffset DhProcess { get; set; }
#endif

        [XmlElement("dhProcess")]
        public string DhProcessField
        {
            get => DhProcess.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhProcess = DateTime.Parse(value);
#else
            set => DhProcess = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tpEv")]
        public string TpEv { get; set; }

        [XmlElement("idEv")]
        public string IdEv { get; set; }

        [XmlElement("hash")]
        public string Hash { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeNrRecArqBase() => !string.IsNullOrEmpty(NrRecArqBase);
        public bool ShouldSerializeNrProtEntr() => !string.IsNullOrEmpty(NrProtEntr);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoTotal")]
    [ComVisible(true)]
#endif
    public class InfoTotal
    {
        [XmlElement("ideEstab")]
        public IdeEstab9001 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstab9001")]
    [ComVisible(true)]
#endif
    public class IdeEstab9001
    {
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("RTom")]
        public RTom9001 RTom { get; set; }

        [XmlElement("RPrest")]
        public RPrest9001 RPrest { get; set; }

        /// <summary>
        /// Totalizador das bases de cálculo e da contribuição previdenciária sobre
        /// recursos repassados a associações desportivas que mantenham equipe de
        /// futebol profissional, apuradas nos eventos R-2030 e R-2040
        /// </summary>
        [XmlElement("RRecRepAD")]
        public List<RRecRepAD> RRecRepAD { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRRecRepAD(RRecRepAD item)
        {
            if (RRecRepAD == null)
            {
                RRecRepAD = new List<RRecRepAD>();
            }

            RRecRepAD.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RRecRepAD (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RRecRepAD</returns>
        public RRecRepAD GetRRecRepAD(int index)
        {
            if ((RRecRepAD?.Count ?? 0) == 0)
            {
                return default;
            };

            return RRecRepAD[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RRecRepAD
        /// </summary>
        public int GetRRecRepADCount => (RRecRepAD != null ? RRecRepAD.Count : 0);
#endif

        [XmlElement("RComl")]
        public List<RComl9001> RComl { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRComl(RComl9001 item)
        {
            if (RComl == null)
            {
                RComl = new List<RComl9001>();
            }

            RComl.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RComl9001 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RComl</returns>
        public RComl9001 GetRComl(int index)
        {
            if ((RComl?.Count ?? 0) == 0)
            {
                return default;
            };

            return RComl[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RComl
        /// </summary>
        public int GetRComlCount => (RComl != null ? RComl.Count : 0);
#endif

        [XmlElement("RAquis")]
        public List<RAquis9001> RAquis { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRAquis(RAquis9001 item)
        {
            if (RAquis == null)
            {
                RAquis = new List<RAquis9001>();
            }

            RAquis.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RAquis9001 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RAquis</returns>
        public RAquis9001 GetRAquis(int index)
        {
            if ((RAquis?.Count ?? 0) == 0)
            {
                return default;
            };

            return RAquis[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RAquis
        /// </summary>
        public int GetRAquisCount => (RAquis != null ? RAquis.Count : 0);
#endif

        [XmlElement("RCPRB")]
        public List<RCPRB9001> RCPRB { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRCPRB(RCPRB9001 item)
        {
            if (RCPRB == null)
            {
                RCPRB = new List<RCPRB9001>();
            }

            RCPRB.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RCPRB9001 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RCPRB</returns>
        public RCPRB9001 GetRCPRB(int index)
        {
            if ((RCPRB?.Count ?? 0) == 0)
            {
                return default;
            };

            return RCPRB[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RCPRB
        /// </summary>
        public int GetRCPRBCount => (RCPRB != null ? RCPRB.Count : 0);
#endif

        [XmlElement("RRecEspetDesp")]
        public RRecEspetDesp RRecEspetDesp { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RTom9001")]
    [ComVisible(true)]
#endif
    public class RTom9001
    {
        [XmlElement("cnpjPrestador")]
        public string CnpjPrestador { get; set; }

        [XmlElement("cno")]
        public string Cno { get; set; }

        [XmlIgnore]
        public double VlrTotalBaseRet { get; set; }

        [XmlElement("vlrTotalBaseRet")]
        public string VlrTotalBaseRetField
        {
            get => VlrTotalBaseRet.ToString("F2", CultureInfoReinf.Info);
            set => VlrTotalBaseRet = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("infoCRTom")]
        public List<InfoCRTom> InfoCRTom { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCRTom(InfoCRTom item)
        {
            if (InfoCRTom == null)
            {
                InfoCRTom = new List<InfoCRTom>();
            }

            InfoCRTom.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCRTom (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCRTom</returns>
        public InfoCRTom GetInfoCRTom(int index)
        {
            if ((InfoCRTom?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoCRTom[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoCRTom
        /// </summary>
        public int GetInfoCRTomCount => (InfoCRTom != null ? InfoCRTom.Count : 0);
#endif

        #region ShouldSerialize
        public bool ShouldSerializeCno() => !string.IsNullOrEmpty(Cno);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoCRTom")]
    [ComVisible(true)]
#endif
    public class InfoCRTom
    {
        [XmlElement("CRTom")]
        public string CRTom { get; set; }

        [XmlIgnore]
        public double VlrCRTom { get; set; }

        [XmlElement("vlrCRTom")]
        public string VlrCRTomField
        {
            get => VlrCRTom.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRTom = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRTomSusp { get; set; }

        [XmlElement("vlrCRTomSusp")]
        public string VlrCRTomSuspField
        {
            get => VlrCRTomSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRTomSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRTomField() => VlrCRTom > 0;

        public bool ShouldSerializeVlrCRTomSuspField() => VlrCRTomSusp > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RPrest9001")]
    [ComVisible(true)]
#endif
    public class RPrest9001
    {
        [XmlElement("tpInscTomador")]
        public TipoInscricaoEstabelecimento TpInscTomador { get; set; }

        [XmlElement("nrInscTomador")]
        public string NrInscTomador { get; set; }

        [XmlIgnore]
        public double VlrTotalBaseRet { get; set; }

        [XmlElement("vlrTotalBaseRet")]
        public string VlrTotalBaseRetField
        {
            get => VlrTotalBaseRet.ToString("F2", CultureInfoReinf.Info);
            set => VlrTotalBaseRet = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrTotalRetPrinc { get; set; }

        [XmlElement("vlrTotalRetPrinc")]
        public string VlrTotalRetPrincField
        {
            get => VlrTotalRetPrinc.ToString("F2", CultureInfoReinf.Info);
            set => VlrTotalRetPrinc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrTotalRetAdic { get; set; }

        [XmlElement("vlrTotalRetAdic")]
        public string VlrTotalRetAdicField
        {
            get => VlrTotalRetAdic.ToString("F2", CultureInfoReinf.Info);
            set => VlrTotalRetAdic = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrTotalNRetPrinc { get; set; }

        [XmlElement("vlrTotalNRetPrinc")]
        public string VlrTotalNRetPrincField
        {
            get => VlrTotalNRetPrinc.ToString("F2", CultureInfoReinf.Info);
            set => VlrTotalNRetPrinc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrTotalNRetAdic { get; set; }

        [XmlElement("vlrTotalNRetAdic")]
        public string VlrTotalNRetAdicField
        {
            get => VlrTotalNRetAdic.ToString("F2", CultureInfoReinf.Info);
            set => VlrTotalNRetAdic = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrTotalRetAdicField() => VlrTotalRetAdic > 0;

        public bool ShouldSerializeVlrTotalNRetPrincField() => VlrTotalNRetPrinc > 0;

        public bool ShouldSerializeVlrTotalNRetAdicField() => VlrTotalNRetAdic > 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Totalizador das bases de cálculo e da contribuição previdenciária sobre
    /// recursos repassados a associações desportivas que mantenham equipe de
    /// futebol profissional, apuradas nos eventos R-2030 e R-2040
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RRecRepAD")]
    [ComVisible(true)]
#endif
    public class RRecRepAD
    {
        [XmlElement("cnpjAssocDesp")]
        public string CnpjAssocDesp { get; set; }

        [XmlElement("nmEmprExt")]
        public string NmEmprExt { get; set; }

        [XmlIgnore]
        public double VlrTotalRep { get; set; }

        [XmlElement("vlrTotalRep")]
        public string VlrTotalRepField
        {
            get => VlrTotalRep.ToString("F2", CultureInfoReinf.Info);
            set => VlrTotalRep = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("CRRecRepAD")]
        public string CRRecRepAD { get; set; }

        [XmlIgnore]
        public double VlrCRRecRepAD { get; set; }

        [XmlElement("vlrCRRecRepAD")]
        public string VlrCRRecRepADField
        {
            get => VlrCRRecRepAD.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRRecRepAD = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRRecRepADSusp { get; set; }

        [XmlElement("vlrCRRecRepADSusp")]
        public string VlrCRRecRepADSuspField
        {
            get => VlrCRRecRepADSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRRecRepADSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeCnpjAssocDesp() => !string.IsNullOrEmpty(CnpjAssocDesp);
        public bool ShouldSerializeNmEmprExt() => !string.IsNullOrEmpty(NmEmprExt);
        public bool ShouldSerializeVlrCRRecRepADSuspField() => VlrCRRecRepADSusp > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RComl9001")]
    [ComVisible(true)]
#endif
    public class RComl9001
    {
        [XmlElement("CRComl")]
        public string CRComl { get; set; }

        [XmlIgnore]
        public double VlrCRComl { get; set; }

        [XmlElement("vlrCRComl")]
        public string VlrCRComlField
        {
            get => VlrCRComl.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRComl = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRComlSusp { get; set; }

        [XmlElement("vlrCRComlSusp")]
        public string VlrCRComlSuspField
        {
            get => VlrCRComlSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRComlSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRComlSuspField() => VlrCRComlSusp > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RAquis9001")]
    [ComVisible(true)]
#endif
    public class RAquis9001
    {
        [XmlElement("CRAquis")]
        public string CRAquis { get; set; }

        [XmlIgnore]
        public double VlrCRAquis { get; set; }

        [XmlElement("vlrCRAquis")]
        public string VlrCRAquisField
        {
            get => VlrCRAquis.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRAquis = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRAquisSusp { get; set; }

        [XmlElement("vlrCRAquisSusp")]
        public string VlrCRAquisSuspField
        {
            get => VlrCRAquisSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRAquisSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRAquisSuspField() => VlrCRAquisSusp > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RCPRB9001")]
    [ComVisible(true)]
#endif
    public class RCPRB9001
    {
        [XmlElement("CRCPRB")]
        public string CRCPRB { get; set; }

        [XmlIgnore]
        public double VlrCRCPRB { get; set; }

        [XmlElement("vlrCRCPRB")]
        public string VlrCRCPRBField
        {
            get => VlrCRCPRB.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRCPRB = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRCPRBSusp { get; set; }

        [XmlElement("vlrCRCPRBSusp")]
        public string VlrCRCPRBSuspField
        {
            get => VlrCRCPRBSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRCPRBSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRCPRBSuspField() => VlrCRCPRBSusp > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RRecEspetDesp")]
    [ComVisible(true)]
#endif
    public class RRecEspetDesp
    {
        [XmlElement("CRRecEspetDesp")]
        public string CRRecEspetDesp { get; set; }

        [XmlIgnore]
        public double VlrReceitaTotal { get; set; }

        [XmlElement("vlrReceitaTotal")]
        public string VlrReceitaTotalField
        {
            get => VlrReceitaTotal.ToString("F2", CultureInfoReinf.Info);
            set => VlrReceitaTotal = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRRecEspetDesp { get; set; }

        [XmlElement("vlrCRRecEspetDesp")]
        public string VlrCRRecEspetDespField
        {
            get => VlrCRRecEspetDesp.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRRecEspetDesp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRRecEspetDespSusp { get; set; }

        [XmlElement("vlrCRRecEspetDespSusp")]
        public string VlrCRRecEspetDespSuspField
        {
            get => VlrCRRecEspetDespSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRRecEspetDespSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRRecEspetDespSuspField() => VlrCRRecEspetDespSusp > 0;

        #endregion ShouldSerialize
    }
}
