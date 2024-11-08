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
    /// R-9005 - Bases e tributos - retenções na fonte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf9005")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRet/v2_01_02", IsNullable = false)]
    public class Reinf9005 : XMLBase
    {
        /// <summary>
        /// Evento Retenção
        /// </summary>
        [XmlElement("evtRet")]
        public EvtRet EvtRet { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Retenção
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtRet")]
    [ComVisible(true)]
#endif
    public class EvtRet : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEvento9005 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("ideRecRetorno")]
        public IdeRecRetorno9005 IdeRecRetorno { get; set; }

        [XmlElement("infoRecEv")]
        public InfoRecEv9005 InfoRecEv { get; set; }

        [XmlElement("infoTotal")]
        public InfoTotal9005 InfoTotal { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEvento9005")]
    [ComVisible(true)]
#endif
    public class IdeEvento9005 : IdeEvento9001 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeRecRetorno9005")]
    [ComVisible(true)]
#endif
    public class IdeRecRetorno9005 : IdeRecRetorno9001 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoRecEv9005")]
    [ComVisible(true)]
#endif
    public class InfoRecEv9005
    {
        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }

        [XmlElement("nrProtLote")]
        public string NrProtLote { get; set; }

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
        public bool ShouldSerializeNrProtLote() => !string.IsNullOrEmpty(NrProtLote);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoTotal9005")]
    [ComVisible(true)]
#endif
    public class InfoTotal9005
    {
        [XmlElement("ideEstab")]
        public IdeEstab9005 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstab9005")]
    [ComVisible(true)]
#endif
    public class IdeEstab9005
    {
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("nrInscBenef")]
        public string NrInscBenef { get; set; }

        [XmlElement("nmBenef")]
        public string NmBenef { get; set; }

        [XmlElement("ideEvtAdic")]
        public string IdeEvtAdic { get; set; }

        [XmlElement("totApurMen")]
        public List<TotApurMen> TotApurMen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurMen(TotApurMen item)
        {
            if (TotApurMen == null)
            {
                TotApurMen = new List<TotApurMen>();
            }

            TotApurMen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurMen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurMen</returns>
        public TotApurMen GetTotApurMen(int index)
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

        [XmlElement("totApurQui")]
        public List<TotApurQui> TotApurQui { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurQui(TotApurQui item)
        {
            if (TotApurQui == null)
            {
                TotApurQui = new List<TotApurQui>();
            }

            TotApurQui.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurQui (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurQui</returns>
        public TotApurQui GetTotApurQui(int index)
        {
            if ((TotApurQui?.Count ?? 0) == 0)
            {
                return default;
            };

            return TotApurQui[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TotApurQui
        /// </summary>
        public int GetTotApurQuiCount => (TotApurQui != null ? TotApurQui.Count : 0);
#endif

        [XmlElement("totApurDec")]
        public List<TotApurDec> TotApurDec { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurDec(TotApurDec item)
        {
            if (TotApurDec == null)
            {
                TotApurDec = new List<TotApurDec>();
            }

            TotApurDec.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurDec (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurDec</returns>
        public TotApurDec GetTotApurDec(int index)
        {
            if ((TotApurDec?.Count ?? 0) == 0)
            {
                return default;
            };

            return TotApurDec[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TotApurDec
        /// </summary>
        public int GetTotApurDecCount => (TotApurDec != null ? TotApurDec.Count : 0);
#endif

        [XmlElement("totApurSem")]
        public List<TotApurSem> TotApurSem { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurSem(TotApurSem item)
        {
            if (TotApurSem == null)
            {
                TotApurSem = new List<TotApurSem>();
            }

            TotApurSem.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurSem (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurSem</returns>
        public TotApurSem GetTotApurSem(int index)
        {
            if ((TotApurSem?.Count ?? 0) == 0)
            {
                return default;
            };

            return TotApurSem[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TotApurSem
        /// </summary>
        public int GetTotApurSemCount => (TotApurSem != null ? TotApurSem.Count : 0);
#endif

        [XmlElement("totApurDia")]
        public List<TotApurDia> TotApurDia { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurDia(TotApurDia item)
        {
            if (TotApurDia == null)
            {
                TotApurDia = new List<TotApurDia>();
            }

            TotApurDia.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurDia (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurDia</returns>
        public TotApurDia GetTotApurDia(int index)
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

        #region ShouldSerialize

        public bool ShouldSerializeNrInscBenef() => !string.IsNullOrEmpty(NrInscBenef);

        public bool ShouldSerializeNmBenef() => !string.IsNullOrEmpty(NmBenef);

        public bool ShouldSerializeIdeEvtAdic() => !string.IsNullOrEmpty(IdeEvtAdic);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurMen")]
    [ComVisible(true)]
#endif
    public class TotApurMen
    {
        [XmlElement("CRMen")]
        public string CRMen { get; set; }

        [XmlIgnore]
        public double VlrBaseCRMen { get; set; }

        [XmlElement("vlrBaseCRMen")]
        public string VlrBaseCRMenField
        {
            get => VlrBaseCRMen.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseCRMen = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrBaseCRMenSusp { get; set; }

        [XmlElement("vlrBaseCRMenSusp")]
        public string VlrBaseCRMenSuspField
        {
            get => VlrBaseCRMenSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseCRMenSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("natRend")]
        public string NatRend { get; set; }

        [XmlElement("totApurTribMen")]
        public TotApurTribMen TotApurTribMen { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVlrBaseCRMenSuspField() => VlrBaseCRMenSusp > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurTribMen")]
    [ComVisible(true)]
#endif
    public class TotApurTribMen
    {
        [XmlIgnore]
        public double VlrCRMenInf { get; set; }

        [XmlElement("vlrCRMenInf")]
        public string VlrCRMenInfField
        {
            get => VlrCRMenInf.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRMenInf = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRMenCalc { get; set; }

        [XmlElement("vlrCRMenCalc")]
        public string VlrCRMenCalcField
        {
            get => VlrCRMenCalc.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRMenCalc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRMenSuspInf { get; set; }

        [XmlElement("vlrCRMenSuspInf")]
        public string VlrCRMenSuspInfField
        {
            get => VlrCRMenSuspInf.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRMenSuspInf = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRMenSuspCalc { get; set; }

        [XmlElement("vlrCRMenSuspCalc")]
        public string VlrCRMenSuspCalcField
        {
            get => VlrCRMenSuspCalc.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRMenSuspCalc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRMenCalcField() => VlrCRMenCalc > 0;

        public bool ShouldSerializeVlrCRMenSuspInfField() => VlrCRMenSuspInf > 0;

        public bool ShouldSerializeVlrCRMenSuspCalcField() => VlrCRMenSuspCalc > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurQui")]
    [ComVisible(true)]
#endif
    public class TotApurQui
    {
        [XmlElement("perApurQui")]
        public string PerApurQui { get; set; }

        [XmlElement("CRQui")]
        public string CRQui { get; set; }

        [XmlIgnore]
        public double VlrBaseCRQui { get; set; }

        [XmlElement("vlrBaseCRQui")]
        public string VlrBaseCRQuiField
        {
            get => VlrBaseCRQui.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseCRQui = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrBaseCRQuiSusp { get; set; }

        [XmlElement("vlrBaseCRQuiSusp")]
        public string VlrBaseCRQuiSuspField
        {
            get => VlrBaseCRQuiSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseCRQuiSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("natRend")]
        public string NatRend { get; set; }

        [XmlElement("totApurTribQui")]
        public TotApurTribQui TotApurTribQui { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVlrBaseCRQuiSuspField() => VlrBaseCRQuiSusp > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurTribQui")]
    [ComVisible(true)]
#endif
    public class TotApurTribQui
    {
        [XmlIgnore]
        public double VlrCRQuiInf { get; set; }

        [XmlElement("vlrCRQuiInf")]
        public string VlrCRQuiInfField
        {
            get => VlrCRQuiInf.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRQuiInf = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRQuiCalc { get; set; }

        [XmlElement("vlrCRQuiCalc")]
        public string VlrCRQuiCalcField
        {
            get => VlrCRQuiCalc.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRQuiCalc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRQuiSuspInf { get; set; }

        [XmlElement("vlrCRQuiSuspInf")]
        public string VlrCRQuiSuspInfField
        {
            get => VlrCRQuiSuspInf.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRQuiSuspInf = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRQuiSuspCalc { get; set; }

        [XmlElement("vlrCRQuiSuspCalc")]
        public string VlrCRQuiSuspCalcField
        {
            get => VlrCRQuiSuspCalc.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRQuiSuspCalc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRQuiCalcField() => VlrCRQuiCalc > 0;

        public bool ShouldSerializeVlrCRQuiSuspInfField() => VlrCRQuiSuspInf > 0;

        public bool ShouldSerializeVlrCRQuiSuspCalcField() => VlrCRQuiSuspCalc > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurDec")]
    [ComVisible(true)]
#endif
    public class TotApurDec
    {
        [XmlElement("perApurDec")]
        public string PerApurDec { get; set; }

        [XmlElement("CRDec")]
        public string CRDec { get; set; }

        [XmlIgnore]
        public double VlrBaseCRDec { get; set; }

        [XmlElement("vlrBaseCRDec")]
        public string VlrBaseCRDecField
        {
            get => VlrBaseCRDec.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseCRDec = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrBaseCRDecSusp { get; set; }

        [XmlElement("vlrBaseCRDecSusp")]
        public string VlrBaseCRDecSuspField
        {
            get => VlrBaseCRDecSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseCRDecSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("natRend")]
        public string NatRend { get; set; }

        [XmlElement("totApurTribDec")]
        public TotApurTribDec TotApurTribDec { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVlrBaseCRDecSuspField() => VlrBaseCRDecSusp > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurTribDec")]
    [ComVisible(true)]
#endif
    public class TotApurTribDec
    {
        [XmlIgnore]
        public double VlrCRDecInf { get; set; }

        [XmlElement("vlrCRDecInf")]
        public string VlrCRDecInfField
        {
            get => VlrCRDecInf.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDecInf = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRDecCalc { get; set; }

        [XmlElement("vlrCRDecCalc")]
        public string VlrCRDecCalcField
        {
            get => VlrCRDecCalc.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDecCalc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRDecSuspInf { get; set; }

        [XmlElement("vlrCRDecSuspInf")]
        public string VlrCRDecSuspInfField
        {
            get => VlrCRDecSuspInf.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDecSuspInf = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRDecSuspCalc { get; set; }

        [XmlElement("vlrCRDecSuspCalc")]
        public string VlrCRDecSuspCalcField
        {
            get => VlrCRDecSuspCalc.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDecSuspCalc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRDecCalcField() => VlrCRDecCalc > 0;

        public bool ShouldSerializeVlrCRDecSuspInfField() => VlrCRDecSuspInf > 0;

        public bool ShouldSerializeVlrCRDecSuspCalcField() => VlrCRDecSuspCalc > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurSem")]
    [ComVisible(true)]
#endif
    public class TotApurSem
    {
        [XmlElement("perApurSem")]
        public string PerApurSem { get; set; }

        [XmlElement("CRSem")]
        public string CRSem { get; set; }

        [XmlIgnore]
        public double VlrBaseCRSem { get; set; }

        [XmlElement("vlrBaseCRSem")]
        public string VlrBaseCRSemField
        {
            get => VlrBaseCRSem.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseCRSem = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrBaseCRSemSusp { get; set; }

        [XmlElement("vlrBaseCRSemSusp")]
        public string VlrBaseCRSemSuspField
        {
            get => VlrBaseCRSemSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseCRSemSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("natRend")]
        public string NatRend { get; set; }

        [XmlElement("totApurTribSem")]
        public TotApurTribSem TotApurTribSem { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVlrBaseCRSemSuspField() => VlrBaseCRSemSusp > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurTribSem")]
    [ComVisible(true)]
#endif
    public class TotApurTribSem
    {
        [XmlIgnore]
        public double VlrCRSemInf { get; set; }

        [XmlElement("vlrCRSemInf")]
        public string VlrCRSemInfField
        {
            get => VlrCRSemInf.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRSemInf = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRSemCalc { get; set; }

        [XmlElement("vlrCRSemCalc")]
        public string VlrCRSemCalcField
        {
            get => VlrCRSemCalc.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRSemCalc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRSemSuspInf { get; set; }

        [XmlElement("vlrCRSemSuspInf")]
        public string VlrCRSemSuspInfField
        {
            get => VlrCRSemSuspInf.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRSemSuspInf = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRSemSuspCalc { get; set; }

        [XmlElement("vlrCRSemSuspCalc")]
        public string VlrCRSemSuspCalcField
        {
            get => VlrCRSemSuspCalc.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRSemSuspCalc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRSemCalcField() => VlrCRSemCalc > 0;

        public bool ShouldSerializeVlrCRSemSuspInfField() => VlrCRSemSuspInf > 0;

        public bool ShouldSerializeVlrCRSemSuspCalcField() => VlrCRSemSuspCalc > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurDia")]
    [ComVisible(true)]
#endif
    public class TotApurDia
    {
        [XmlElement("perApurDia")]
        public string PerApurDia { get; set; }

        [XmlElement("CRDia")]
        public string CRDia { get; set; }

        [XmlIgnore]
        public double VlrBaseCRDia { get; set; }

        [XmlElement("vlrBaseCRDia")]
        public string VlrCRSemSuspCalcField
        {
            get => VlrBaseCRDia.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseCRDia = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrBaseCRDiaSusp { get; set; }

        [XmlElement("vlrBaseCRDiaSusp")]
        public string VlrBaseCRDiaSuspField
        {
            get => VlrBaseCRDiaSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseCRDiaSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("natRend")]
        public string NatRend { get; set; }

        [XmlElement("totApurTribDia")]
        public TotApurTribDia TotApurTribDia { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVlrBaseCRDiaSuspField() => VlrBaseCRDiaSusp > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurTribDia")]
    [ComVisible(true)]
#endif
    public class TotApurTribDia
    {
        [XmlIgnore]
        public double VlrCRDiaInf { get; set; }

        [XmlElement("vlrCRDiaInf")]
        public string VlrCRDiaInfField
        {
            get => VlrCRDiaInf.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDiaInf = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRDiaCalc { get; set; }

        [XmlElement("vlrCRDiaCalc")]
        public string VlrCRDiaCalcField
        {
            get => VlrCRDiaCalc.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDiaCalc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRDiaSuspInf { get; set; }

        [XmlElement("vlrCRDiaSuspInf")]
        public string VlrCRDiaSuspInfField
        {
            get => VlrCRDiaSuspInf.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDiaSuspInf = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRDiaSuspCalc { get; set; }

        [XmlElement("vlrCRDiaSuspCalc")]
        public string VlrCRDiaSuspCalcField
        {
            get => VlrCRDiaSuspCalc.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDiaSuspCalc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRDiaCalcField() => VlrCRDiaCalc > 0;

        public bool ShouldSerializeVlrCRDiaSuspInfField() => VlrCRDiaSuspInf > 0;

        public bool ShouldSerializeVlrCRDiaSuspCalcField() => VlrCRDiaSuspCalc > 0;

        #endregion ShouldSerialize
    }
}
