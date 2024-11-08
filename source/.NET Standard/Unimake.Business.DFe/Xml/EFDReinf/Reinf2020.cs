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
    /// R-2020 - Retenção de contribuição previdenciária - serviços prestados
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf2020")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtPrestadorServicos/v2_01_02", IsNullable = false)]
    public class Reinf2020 : XMLBase
    {
        /// <summary>
        /// Evento serviços prestados 
        /// </summary>
        [XmlElement("evtServPrest")]
        public EvtServPrest EvtServPrest { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtServPrest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class EvtServPrest : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEvento2020 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do contribuinte
        /// </summary>
        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        /// <summary>
        /// Informações relativas aos serviços prestados
        /// </summary>
        [XmlElement("infoServPrest")]
        public InfoServPrest InfoServPrest { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEvento2020")]
    [ComVisible(true)]
#endif
    public class IdeEvento2020 : IdeEvento2010 { }

    /// <summary>
    /// Informações relativas aos serviços prestados
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoServPrest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoServPrest
    {
        /// <summary>
        /// Registro que identifica o estabelecimento
        /// "prestador" dos serviços
        /// </summary>
        [XmlElement("ideEstabPrest")]
        public IdeEstabPrest IdeEstabPrest { get; set; }

    }

    /// <summary>
    /// Registro que identifica o estabelecimento
    /// "prestador" dos serviços
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstabPrest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeEstabPrest
    {
        [XmlElement("tpInscEstabPrest")]
        public TipoInscricaoEstabelecimento TpInscEstabPrest { get; set; }

        [XmlElement("nrInscEstabPrest")]
        public string NrInscEstabPrest { get; set; }

        [XmlElement("ideTomador")]
        public IdeTomador IdeTomador { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeTomador")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeTomador
    {

        [XmlElement("tpInscTomador")]
        public TipoInscricaoEstabelecimento TpInscTomador { get; set; }

        [XmlElement("nrInscTomador")]
        public string NrInscTomador { get; set; }

        [XmlElement("indObra")]
        public IndicativoObra IndObra { get; set; }

        [XmlIgnore]
        public double VlrTotalBruto { get; set; }

        [XmlElement("vlrTotalBruto")]
        public string VlrTotalBrutoField
        {
            get => VlrTotalBruto.ToString("F2", CultureInfoReinf.Info);
            set => VlrTotalBruto = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

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

        [XmlElement("nfs")]
        public List<Nfs2020> Nfs { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddNfs(Nfs2020 item)
        {
            if (Nfs == null)
            {
                Nfs = new List<Nfs2020>();
            }

            Nfs.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Nfs (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Nfs</returns>
        public Nfs2020 GetNfs(int index)
        {
            if ((Nfs?.Count ?? 0) == 0)
            {
                return default;
            };

            return Nfs[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Nfs
        /// </summary>
        public int GetNfsCount => (Nfs != null ? Nfs.Count : 0);
#endif

        [XmlElement("infoProcRetPr")]
        public List<InfoProcRetPr> InfoProcRetPr { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProcRetPr(InfoProcRetPr item)
        {
            if (InfoProcRetPr == null)
            {
                InfoProcRetPr = new List<InfoProcRetPr>();
            }

            InfoProcRetPr.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProcRetPr (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProcRetPr</returns>
        public InfoProcRetPr GetInfoProcRetPr(int index)
        {
            if ((InfoProcRetPr?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoProcRetPr[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoProcRetPr
        /// </summary>
        public int GetInfoProcRetPrCount => (InfoProcRetPr != null ? InfoProcRetPr.Count : 0);

#endif

        [XmlElement("infoProcRetAd")]
        public List<InfoProcRetAd> InfoProcRetAd { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProcRetAd(InfoProcRetAd item)
        {
            if (InfoProcRetAd == null)
            {
                InfoProcRetAd = new List<InfoProcRetAd>();
            }

            InfoProcRetAd.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProcRetAd (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProcRetAd</returns>
        public InfoProcRetAd GetInfoProcRetAd(int index)
        {
            if ((InfoProcRetAd?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoProcRetAd[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoProcRetAd
        /// </summary>
        public int GetInfoProcRetAdCount => (InfoProcRetAd != null ? InfoProcRetAd.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeVlrTotalRetAdicField() => VlrTotalRetAdic > 0;

        public bool ShouldSerializeVlrTotalNRetPrincField() => VlrTotalNRetPrinc > 0;

        public bool ShouldSerializeVlrTotalNRetAdicField() => VlrTotalNRetAdic > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Nfs2020")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Nfs2020 : Nfs2010 { }

}
