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
    /// R-2050 - Comercialização de produção
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf2050")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtInfoProdRural/v2_01_02", IsNullable = false)]
    public class Reinf2050 : XMLBase
    {
        /// <summary>
        /// Evento comercialização da produção
        /// </summary>
        [XmlElement("evtComProd")]
        public EvtComProd EvtComProd { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtComProd")]
    [ComVisible(true)]
#endif
    public class EvtComProd : ReinfEventoBase
    {

        [XmlElement("ideEvento")]
        public IdeEvento2050 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("infoComProd")]
        public InfoComProd InfoComProd { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEvento2050")]
    [ComVisible(true)]
#endif
    public class IdeEvento2050 : IdeEvento2010 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoComProd")]
    [ComVisible(true)]
#endif
    public class InfoComProd
    {
        [XmlElement("ideEstab")]
        public IdeEstab2050 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstab2050")]
    [ComVisible(true)]
#endif
    public class IdeEstab2050
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
        public double VlrCPApur { get; set; }

        [XmlElement("vlrCPApur")]
        public string VlrCPApurField
        {
            get => VlrCPApur.ToString("F2", CultureInfoReinf.Info);
            set => VlrCPApur = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrRatApur { get; set; }

        [XmlElement("vlrRatApur")]
        public string VlrRatApurField
        {
            get => VlrRatApur.ToString("F2", CultureInfoReinf.Info);
            set => VlrRatApur = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrSenarApur { get; set; }

        [XmlElement("vlrSenarApur")]
        public string VlrSenarApurField
        {
            get => VlrSenarApur.ToString("F2", CultureInfoReinf.Info);
            set => VlrSenarApur = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCPSuspTotal { get; set; }

        [XmlElement("vlrCPSuspTotal")]
        public string VlrCPSuspTotalField
        {
            get => VlrCPSuspTotal.ToString("F2", CultureInfoReinf.Info);
            set => VlrCPSuspTotal = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrRatSuspTotal { get; set; }

        [XmlElement("vlrRatSuspTotal")]
        public string VlrRatSuspTotalField
        {
            get => VlrRatSuspTotal.ToString("F2", CultureInfoReinf.Info);
            set => VlrRatSuspTotal = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrSenarSuspTotal { get; set; }

        [XmlElement("vlrSenarSuspTotal")]
        public string VlrSenarSuspTotalField
        {
            get => VlrSenarSuspTotal.ToString("F2", CultureInfoReinf.Info);
            set => VlrSenarSuspTotal = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("tipoCom")]
        public List<TipoCom> TipoCom { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTipoCom(TipoCom item)
        {
            if (TipoCom == null)
            {
                TipoCom = new List<TipoCom>();
            }

            TipoCom.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TipoCom (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TipoCom</returns>
        public TipoCom GetTipoCom(int index)
        {
            if ((TipoCom?.Count ?? 0) == 0)
            {
                return default;
            };

            return TipoCom[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TipoCom
        /// </summary>
        public int GetTipoComCount => (TipoCom != null ? TipoCom.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVlrCPSuspTotalField() => VlrCPSuspTotal > 0;
        
        public bool ShouldSerializeVlrRatSuspTotalField() => VlrRatSuspTotal > 0;
       
        public bool ShouldSerializeVlrSenarSuspTotalField() => VlrSenarSuspTotal > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.TipoCom")]
    [ComVisible(true)]
#endif
    public class TipoCom
    {
        [XmlElement("indCom")]
        public IndicativoComercializacao IndCom { get; set; }

        [XmlIgnore]
        public double VlrRecBruta { get; set; }

        [XmlElement("vlrRecBruta")]
        public string VlrRecBrutaField
        {
            get => VlrRecBruta.ToString("F2", CultureInfoReinf.Info);
            set => VlrRecBruta = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("infoProc")]
        public List<InfoProc2050> InfoProc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProc(InfoProc2050 item)
        {
            if (InfoProc == null)
            {
                InfoProc = new List<InfoProc2050>();
            }

            InfoProc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProc</returns>
        public InfoProc2050 GetInfoProc(int index)
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProc2050")]
    [ComVisible(true)]
#endif
    public class InfoProc2050
    {

        [XmlElement("tpProc")]
        public TipoProcesso TpProc { get; set; }

        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        [XmlElement("codSusp")]
        public string CodSusp { get; set; }

        [XmlIgnore]
        public double VlrCPSusp { get; set; }

        [XmlElement("vlrCPSusp")]
        public string VlrCPSuspField
        {
            get => VlrCPSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrCPSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrRatSusp { get; set; }

        [XmlElement("vlrRatSusp")]
        public string VlrRatSuspField
        {
            get => VlrRatSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrRatSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        } 

        [XmlIgnore]
        public double VlrSenarSusp { get; set; }

        [XmlElement("vlrSenarSusp")]
        public string VlrSenarSuspField
        {
            get => VlrSenarSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrSenarSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeCodSusp() => !string.IsNullOrEmpty(CodSusp);

        public bool ShouldSerializeVlrCPSuspField() => VlrCPSusp > 0;
        
        public bool ShouldSerializeVlrRatSuspField() => VlrRatSusp > 0;
        
        public bool ShouldSerializeVlrSenarSuspField() => VlrSenarSusp > 0;

        #endregion ShouldSerialize
    }
}
