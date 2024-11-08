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
    /// R-9015 - Consolidação das retenções na fonte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf9015")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRetCons/v2_01_02", IsNullable = false)]
    public class Reinf9015 : XMLBase
    {
        /// <summary>
        /// Evento retenções consolidadas
        /// </summary>
        [XmlElement("evtRetCons")]
        public EvtRetCons EvtRetCons { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento retenções consolidadas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtRetCons")]
    [ComVisible(true)]
#endif
    public class EvtRetCons : ReinfEventoBase
    {
        /// <summary>
        /// Informações de identificação do evento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento9015 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do contribuinte
        /// </summary>
        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        /// <summary>
        /// Informações do recibo de retorno
        /// </summary>
        [XmlElement("ideRecRetorno")]
        public IdeRecRetorno9015 IdeRecRetorno { get; set; }

        /// <summary>
        /// Informações de processamento do evento
        /// </summary>
        [XmlElement("infoRecEv")]
        public InfoRecEv9015 InfoRecEv { get; set; }

        /// <summary>
        /// Informações relativas a totalizadores pela natureza do rendimento e código de receita
        /// </summary>
        [XmlElement("infoCR_CNR")]
        public InfoCR_CNR InfoCR_CNR { get; set; }

        /// <summary>
        /// Informações consolidadas dos tributos da empresa
        /// </summary>
        [XmlElement("infoTotalCR")]
        public InfoTotalCR InfoTotalCR { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEvento9015")]
    [ComVisible(true)]
#endif
    public class IdeEvento9015 : IdeEvento9001 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeRecRetorno9015")]
    [ComVisible(true)]
#endif
    public class IdeRecRetorno9015 : IdeRecRetorno9001 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoRecEv9015")]
    [ComVisible(true)]
#endif
    public class InfoRecEv9015
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

        [XmlElement("fechRet")]
        public IndicativoFinalidadeEvento FechRet { get; set; }

        [XmlElement("hash")]
        public string Hash { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeNrProtLote() => !string.IsNullOrEmpty(NrProtLote);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoCR_CNR")]
    [ComVisible(true)]
#endif
    public class InfoCR_CNR
    {
        [XmlElement("indExistInfo")]
        public IndicativoExistenciaTributos IndExistInfo { get; set; }

        [XmlElement("identEscritDCTF")]
        public string IdentEscritDCTF { get; set; }

        [XmlElement("totApurMen")]
        public List<TotApurMen9015> TotApurMen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurMen(TotApurMen9015 item)
        {
            if (TotApurMen == null)
            {
                TotApurMen = new List<TotApurMen9015>();
            }

            TotApurMen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurMen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurMen</returns>
        public TotApurMen9015 GetTotApurMen(int index)
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
        public List<TotApurQui9015> TotApurQui { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurQui(TotApurQui9015 item)
        {
            if (TotApurQui == null)
            {
                TotApurQui = new List<TotApurQui9015>();
            }

            TotApurQui.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurQui (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurQui</returns>
        public TotApurQui9015 GetTotApurQui(int index)
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
        public List<TotApurDec9015> TotApurDec { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurDec(TotApurDec9015 item)
        {
            if (TotApurDec == null)
            {
                TotApurDec = new List<TotApurDec9015>();
            }

            TotApurDec.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurDec (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurDec</returns>
        public TotApurDec9015 GetTotApurDec(int index)
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
        public List<TotApurSem9015> TotApurSem { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurSem(TotApurSem9015 item)
        {
            if (TotApurSem == null)
            {
                TotApurSem = new List<TotApurSem9015>();
            }

            TotApurSem.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurSem (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurSem</returns>
        public TotApurSem9015 GetTotApurSem(int index)
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
        public List<TotApurDia9015> TotApurDia { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurDia(TotApurDia9015 item)
        {
            if (TotApurDia == null)
            {
                TotApurDia = new List<TotApurDia9015>();
            }

            TotApurDia.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurDia (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurDia</returns>
        public TotApurDia9015 GetTotApurDia(int index)
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

        public bool ShouldSereializeIdentEscritDCTF() => !string.IsNullOrEmpty(IdentEscritDCTF);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurMen9015")]
    [ComVisible(true)]
#endif
    public class TotApurMen9015
    {
        [XmlElement("CRMen")]
        public string CRMen { get; set; }

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
        public double VlrCRMenDCTF { get; set; }

        [XmlElement("vlrCRMenDCTF")]
        public string VlrCRMenDCTFField
        {
            get => VlrCRMenDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRMenDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
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

        [XmlIgnore]
        public double VlrCRMenSuspDCTF { get; set; }

        [XmlElement("vlrCRMenSuspDCTF")]
        public string VlrCRMenSuspDCTFField
        {
            get => VlrCRMenSuspDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRMenSuspDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("natRend")]
        public string NatRend { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRMenCalcField() => VlrCRMenCalc > 0;

        public bool ShouldSerializeVlrCRMenSuspInfField() => VlrCRMenSuspInf > 0;

        public bool ShouldSerializeVlrCRMenSuspCalcField() => VlrCRMenSuspCalc > 0;

        public bool ShouldSerializeVlrCRMenSuspDCTFField() => VlrCRMenSuspDCTF > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurQui9015")]
    [ComVisible(true)]
#endif
    public class TotApurQui9015
    {
        [XmlElement("perApurQui")]
        public string PerApurQui { get; set; }

        [XmlElement("CRQui")]
        public string CRQui { get; set; }

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
        public double VlrCRQuiDCTF { get; set; }

        [XmlElement("vlrCRQuiDCTF")]
        public string VlrCRQuiDCTFField
        {
            get => VlrCRQuiDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRQuiDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
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
        public double VlrCRQuisSuspCalc { get; set; }

        [XmlElement("vlrCRQuisSuspCalc")]
        public string VlrCRQuisSuspCalcField
        {
            get => VlrCRQuisSuspCalc.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRQuisSuspCalc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRQuiSuspDCTF { get; set; }

        [XmlElement("vlrCRQuiSuspDCTF")]
        public string VlrCRQuiSuspDCTFField
        {
            get => VlrCRQuiSuspDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRQuiSuspDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("natRend")]
        public string NatRend { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRQuiCalcField() => VlrCRQuiCalc > 0;

        public bool ShouldSerializeVlrCRQuiSuspInfField() => VlrCRQuiSuspInf > 0;

        public bool ShouldSerializeVlrCRQuisSuspCalcField() => VlrCRQuisSuspCalc > 0;

        public bool ShouldSerializeVlrCRQuiSuspDCTFField() => VlrCRQuiSuspDCTF > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurDec9015")]
    [ComVisible(true)]
#endif
    public class TotApurDec9015
    {
        [XmlElement("perApurDec")]
        public string PerApurDec { get; set; }

        [XmlElement("CRDec")]
        public string CRDec { get; set; }

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
        public double VlrCRDecDCTF { get; set; }

        [XmlElement("vlrCRDecDCTF")]
        public string VlrCRDecDCTFField
        {
            get => VlrCRDecDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDecDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
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

        [XmlIgnore]
        public double VlrCRDecSuspDCTF { get; set; }

        [XmlElement("vlrCRDecSuspDCTF")]
        public string VlrCRDecSuspDCTFField
        {
            get => VlrCRDecSuspDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDecSuspDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("natRend")]
        public string NatRend { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRDecCalcField() => VlrCRDecCalc > 0;

        public bool ShouldSerializeVlrCRDecSuspInfField() => VlrCRDecSuspInf > 0;

        public bool ShouldSerializeVlrCRDecSuspCalcField() => VlrCRDecSuspCalc > 0;

        public bool ShouldSerializeVlrCRDecSuspDCTFField() => VlrCRDecSuspDCTF > 0;


        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurSem9015")]
    [ComVisible(true)]
#endif
    public class TotApurSem9015
    {
        [XmlElement("perApurSem")]
        public string PerApurSem { get; set; }

        [XmlElement("CRSem")]
        public string CRSem { get; set; }

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
        public string VlrBaseCRSemSuspField
        {
            get => VlrCRSemCalc.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRSemCalc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRSemDCTF { get; set; }

        [XmlElement("vlrCRSemDCTF")]
        public string VlrCRSemDCTFField
        {
            get => VlrCRSemDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRSemDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
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

        [XmlIgnore]
        public double VlrCRSemSuspDCTF { get; set; }

        [XmlElement("vlrCRSemSuspDCTF")]
        public string VlrCRSemSuspDCTFField
        {
            get => VlrCRSemSuspDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRSemSuspDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("natRend")]
        public string NatRend { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRSemCalcField() => VlrCRSemCalc > 0;

        public bool ShouldSerializeVlrCRSemSuspInfField() => VlrCRSemSuspInf > 0;

        public bool ShouldSerializeVlrCRSemSuspCalcField() => VlrCRSemSuspCalc > 0;

        public bool ShouldSerializeVlrCRSemSuspDCTFField() => VlrCRSemSuspDCTF > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurDia9015")]
    [ComVisible(true)]
#endif
    public class TotApurDia9015
    {
        [XmlElement("perApurDia")]
        public string PerApurDia { get; set; }

        [XmlElement("CRDia")]
        public string CRDia { get; set; }

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
        public double VlrCRDiaDCTF { get; set; }

        [XmlElement("vlrCRDiaDCTF")]
        public string VlrCRDiaDCTFField
        {
            get => VlrCRDiaDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDiaDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
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

        [XmlIgnore]
        public double VlrCRDiaSuspDCTF { get; set; }

        [XmlElement("vlrCRDiaSuspDCTF")]
        public string VlrCRDiaSuspDCTFField
        {
            get => VlrCRDiaSuspDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDiaSuspDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("natRend")]
        public string NatRend { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRDiaCalcField() => VlrCRDiaCalc > 0;

        public bool ShouldSerializeVlrCRDiaSuspInfField() => VlrCRDiaSuspInf > 0;

        public bool ShouldSerializeVlrCRDiaSuspCalcField() => VlrCRDiaSuspCalc > 0;

        public bool ShouldSerializeVlrCRDiaSuspDCTFField() => VlrCRDiaSuspDCTF > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoTotalCR")]
    [ComVisible(true)]
#endif
    public class InfoTotalCR
    {
        [XmlElement("totApurMen")]
        public List<TotApurMenInfoTotalCR> TotApurMen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurMen(TotApurMenInfoTotalCR item)
        {
            if (TotApurMen == null)
            {
                TotApurMen = new List<TotApurMenInfoTotalCR>();
            }

            TotApurMen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurMen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurMen</returns>
        public TotApurMenInfoTotalCR GetTotApurMen(int index)
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
        public List<TotApurQuiInfoTotalCR> TotApurQui { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurQui(TotApurQuiInfoTotalCR item)
        {
            if (TotApurQui == null)
            {
                TotApurQui = new List<TotApurQuiInfoTotalCR>();
            }

            TotApurQui.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurQui (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurQui</returns>
        public TotApurQuiInfoTotalCR GetTotApurQui(int index)
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
        public List<TotApurDecInfoTotalCR> TotApurDec { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurDec(TotApurDecInfoTotalCR item)
        {
            if (TotApurDec == null)
            {
                TotApurDec = new List<TotApurDecInfoTotalCR>();
            }

            TotApurDec.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurDec (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurDec</returns>
        public TotApurDecInfoTotalCR GetTotApurDec(int index)
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
        public List<TotApurSemInfoTotalCR> TotApurSem { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurSem(TotApurSemInfoTotalCR item)
        {
            if (TotApurSem == null)
            {
                TotApurSem = new List<TotApurSemInfoTotalCR>();
            }

            TotApurSem.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurSem (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurSem</returns>
        public TotApurSemInfoTotalCR GetTotApurSem(int index)
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
        public List<TotApurDiaInfoTotalCR> TotApurDia { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurDia(TotApurDiaInfoTotalCR item)
        {
            if (TotApurDia == null)
            {
                TotApurDia = new List<TotApurDiaInfoTotalCR>();
            }

            TotApurDia.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurSem (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurSem</returns>
        public TotApurDiaInfoTotalCR GetTotApurDia(int index)
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
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurMenInfoTotalCR")]
    [ComVisible(true)]
#endif
    public class TotApurMenInfoTotalCR
    {
        [XmlElement("CRMen")]
        public string CRMen { get; set; }

        [XmlIgnore]
        public double VlrCRMenDCTF { get; set; }

        [XmlElement("vlrCRMenDCTF")]
        public string VlrCRMenDCTFField
        {
            get => VlrCRMenDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRMenDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRMenSuspDCTF { get; set; }

        [XmlElement("vlrCRMenSuspDCTF")]
        public string VlrCRMenSuspDCTFField
        {
            get => VlrCRMenSuspDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRMenSuspDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRMenSuspDCTFField() => VlrCRMenSuspDCTF > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurQuiInfoTotalCR")]
    [ComVisible(true)]
#endif
    public class TotApurQuiInfoTotalCR
    {
        [XmlElement("perApurQui")]
        public string PerApurQui { get; set; }

        [XmlElement("CRQui")]
        public string CRQui { get; set; }

        [XmlIgnore]
        public double VlrCRQuiDCTF { get; set; }

        [XmlElement("vlrCRQuiDCTF")]
        public string VlrCRQuiDCTFField
        {
            get => VlrCRQuiDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRQuiDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRQuiSuspDCTF { get; set; }

        [XmlElement("vlrCRQuiSuspDCTF")]
        public string VlrCRQuiSuspDCTFField
        {
            get => VlrCRQuiSuspDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRQuiSuspDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRQuiSuspDCTFField() => VlrCRQuiSuspDCTF > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurDecInfoTotalCR")]
    [ComVisible(true)]
#endif
    public class TotApurDecInfoTotalCR
    {
        [XmlElement("perApurDec")]
        public string PerApurDec { get; set; }

        [XmlElement("CRDec")]
        public string CRDec { get; set; }

        [XmlIgnore]
        public double VlrCRDecDCTF { get; set; }

        [XmlElement("vlrCRDecDCTF")]
        public string VlrCRDecDCTFField
        {
            get => VlrCRDecDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDecDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRDecSuspDCTF { get; set; }

        [XmlElement("vlrCRDecSuspDCTF")]
        public string VlrCRDecSuspDCTFField
        {
            get => VlrCRDecSuspDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDecSuspDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRDecSuspDCTFField() => VlrCRDecSuspDCTF > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurSemInfoTotalCR")]
    [ComVisible(true)]
#endif
    public class TotApurSemInfoTotalCR
    {
        [XmlElement("perApurSem")]
        public string PerApurSem { get; set; }

        [XmlElement("CRSem")]
        public string CRSem { get; set; }

        [XmlIgnore]
        public double VlrCRSemDCTF { get; set; }

        [XmlElement("vlrCRSemDCTF")]
        public string VlrCRSemDCTFField
        {
            get => VlrCRSemDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRSemDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRSemSuspDCTF { get; set; }

        [XmlElement("vlrCRSemSuspDCTF")]
        public string VlrCRSemSuspDCTFField
        {
            get => VlrCRSemSuspDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRSemSuspDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRSemSuspDCTFField() => VlrCRSemSuspDCTF > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TotApurDiaInfoTotalCR")]
    [ComVisible(true)]
#endif
    public class TotApurDiaInfoTotalCR
    {
        [XmlElement("perApurDia")]
        public string PerApurDia { get; set; }

        [XmlElement("CRDia")]
        public string CRDia { get; set; }

        [XmlIgnore]
        public double VlrCRDiaDCTF { get; set; }

        [XmlElement("vlrCRDiaDCTF")]
        public string VlrCRDiaDCTFField
        {
            get => VlrCRDiaDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDiaDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRDiaSuspDCTF { get; set; }

        [XmlElement("vlrCRDiaSuspDCTF")]
        public string VlrCRDiaSuspDCTFField
        {
            get => VlrCRDiaSuspDCTF.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRDiaSuspDCTF = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRDiaSuspDCTFField() => VlrCRDiaSuspDCTF > 0;

        #endregion ShouldSerialize
    }
}
