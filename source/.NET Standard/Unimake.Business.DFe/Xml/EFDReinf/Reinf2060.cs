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
    /// R-2060 - Contribuição previdenciária sobre a receita bruta - CPRB
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf2060 ")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtInfoCPRB/v2_01_02", IsNullable = false)]
    public class Reinf2060 : XMLBase
    {
        /// <summary>
        /// Evento da contribuição previdenciária sobre a receita bruta - CPRB
        /// </summary>
        [XmlElement("evtCPRB")]
        public EvtCPRB EvtCPRB { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento da contribuição previdenciária sobre a receita bruta - CPRB
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtCPRB")]
    [ComVisible(true)]
#endif
    public class EvtCPRB : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEvento2060 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("infoCPRB")]
        public InfoCPRB InfoCPRB { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEvento2060")]
    [ComVisible(true)]
#endif
    public class IdeEvento2060 : IdeEvento2055 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoCPRB")]
    [ComVisible(true)]
#endif
    public class InfoCPRB
    {
        [XmlElement("ideEstab")]
        public IdeEstab2060 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstab2060")]
    [ComVisible(true)]
#endif
    public class IdeEstab2060
    {

        [XmlElement("tpInscEstab")]
        public TipoInscricaoEstabelecimento TpInscEstab { get; set; }

        [XmlElement("nrInscEstab")]
        public string NrInscEstab { get; set; }

        [XmlIgnore]
        public double VlrRecBrutaTotal { get; set; }

        [XmlElement("vlrRecBrutaTotal")]
        public string VlrRecBrutaTotalField
        {
            get => VlrRecBrutaTotal.ToString("F2", CultureInfoReinf.Info);
            set => VlrRecBrutaTotal = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCPApurTotal { get; set; }

        [XmlElement("vlrCPApurTotal")]
        public string VlrCPApurTotalField
        {
            get => VlrCPApurTotal.ToString("F2", CultureInfoReinf.Info);
            set => VlrCPApurTotal = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCPRBSuspTotal { get; set; }

        [XmlElement("vlrCPRBSuspTotal")]
        public string VlrCPRBSuspTotalField
        {
            get => VlrCPRBSuspTotal.ToString("F2", CultureInfoReinf.Info);
            set => VlrCPRBSuspTotal = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("tipoCod")]
        public List<TipoCod> TipoCod { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTipoCod(TipoCod item)
        {
            if (TipoCod == null)
            {
                TipoCod = new List<TipoCod>();
            }

            TipoCod.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TipoCod (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TipoCod</returns>
        public TipoCod GetTipoCod(int index)
        {
            if ((TipoCod?.Count ?? 0) == 0)
            {
                return default;
            };

            return TipoCod[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TipoCod
        /// </summary>
        public int GetTipoCodCount => (TipoCod != null ? TipoCod.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVlrCPRBSuspTotalField() => VlrCPRBSuspTotal > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TipoCod")]
    [ComVisible(true)]
#endif
    public class TipoCod
    {
        [XmlElement("codAtivEcon")]
        public string CodAtivEcon { get; set; }

        [XmlIgnore]
        public double VlrRecBrutaAtiv { get; set; }

        [XmlElement("vlrRecBrutaAtiv")]
        public string VlrRecBrutaAtivField
        {
            get => VlrRecBrutaAtiv.ToString("F2", CultureInfoReinf.Info);
            set => VlrRecBrutaAtiv = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrExcRecBruta { get; set; }

        [XmlElement("vlrExcRecBruta")]
        public string VlrExcRecBrutaField
        {
            get => VlrExcRecBruta.ToString("F2", CultureInfoReinf.Info);
            set => VlrExcRecBruta = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrAdicRecBruta { get; set; }

        [XmlElement("vlrAdicRecBruta")]
        public string VlrAdicRecBrutaField
        {
            get => VlrAdicRecBruta.ToString("F2", CultureInfoReinf.Info);
            set => VlrAdicRecBruta = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrBcCPRB { get; set; }

        [XmlElement("vlrBcCPRB")]
        public string VlrBcCPRBField
        {
            get => VlrBcCPRB.ToString("F2", CultureInfoReinf.Info);
            set => VlrBcCPRB = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCPRBapur { get; set; }

        [XmlElement("vlrCPRBapur")]
        public string VlrCPRBapurField
        {
            get => VlrCPRBapur.ToString("F2", CultureInfoReinf.Info);
            set => VlrCPRBapur = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("tipoAjuste")]
        public List<TipoAjuste> TipoAjuste { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTipoAjuste(TipoAjuste item)
        {
            if (TipoAjuste == null)
            {
                TipoAjuste = new List<TipoAjuste>();
            }

            TipoAjuste.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TipoAjuste (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TipoAjuste</returns>
        public TipoAjuste GetTipoAjuste(int index)
        {
            if ((TipoAjuste?.Count ?? 0) == 0)
            {
                return default;
            };

            return TipoAjuste[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TipoAjuste
        /// </summary>
        public int GetTipoAjusteCount => (TipoAjuste != null ? TipoAjuste.Count : 0);
#endif

        [XmlElement("infoProc")]
        public List<InfoProc2060> InfoProc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProc(InfoProc2060 item)
        {
            if (InfoProc == null)
            {
                InfoProc = new List<InfoProc2060>();
            }

            InfoProc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProc</returns>
        public InfoProc2060 GetInfoProc(int index)
        {
            if ((InfoProc?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoProc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoProc
        /// </summary>
        public int GetInfoProcCount => (InfoProc != null ? InfoProc.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVlrCPRBapurField() => VlrCPRBapur > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TipoAjuste")]
    [ComVisible(true)]
#endif
    public class TipoAjuste
    {
        [XmlElement("tpAjuste")]
        public TipoAjusteReinf TpAjuste { get; set; }

        [XmlElement("codAjuste")]
        public CodigoAjuste CodAjuste { get; set; }

        [XmlIgnore]
        public double VlrAjuste { get; set; }

        [XmlElement("vlrAjuste")]
        public string VlrAjusteField
        {
            get => VlrAjuste.ToString("F2", CultureInfoReinf.Info);
            set => VlrAjuste = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("descAjuste")]
        public string DescAjuste { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtAjuste { get; set; }
#else
        public DateTimeOffset DtAjuste { get; set; }
#endif

        [XmlElement("dtAjuste")]
        public string DtAjusteField
        {
            get => DtAjuste.ToString("yyyy-MM");
#if INTEROP
            set => DtAjuste = DateTime.Parse(value);
#else
            set => DtAjuste = DateTimeOffset.Parse(value);
#endif
        }


    }

#if INTEROP
        [ClassInterface(ClassInterfaceType.AutoDual)]
        [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProc2060")]
        [ComVisible(true)]
#endif
    public class InfoProc2060
    {
        [XmlElement("tpProc")]
        public TipoProcesso TpProc { get; set; }

        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        [XmlElement("codSusp")]
        public string CodSusp { get; set; }

        [XmlIgnore]
        public double VlrCPRBSusp { get; set; }

        [XmlElement("vlrCPRBSusp")]
        public string VlrCPRBSuspField
        {
            get => VlrCPRBSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrCPRBSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize
        
        public bool ShouldSerializeCodSusp() => !string.IsNullOrEmpty(CodSusp);

        #endregion ShouldSerialize
    }
}
