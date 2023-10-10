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
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf2020")]
    [ComVisible(true)]
#endif

    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtPrestadorServicos/v2_01_02", IsNullable = false)]
    public class Reinf2020 : XMLBase
    {

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
        public IdeEventoReinf2020 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("infoServPrest")]
        public InfoServPrest InfoServPrest { get; set; }
    }

    public class IdeEventoReinf2020
    {

        [XmlElement("indRetif")]
        public IndicativoRetificacao IndRetif { get; set; }

        /// <summary>
        /// Validação: O preenchimento é obrigatório se {indRetif} = [2]. Deve ser um recibo de entrega válido, correspondente ao arquivo objeto da retificação.
        /// </summary>
        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

        /// <summary>
        /// Informar o ano/mês de referência das informações no formato AAAA-MM. Validação: Deve ser um ano/mês válido para o qual haja informações do contribuinte encaminhadas através do evento R-1000.
        /// </summary>
        [XmlElement("perApur")]
        public string PerApur { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("procEmi")]
        public ProcessoEmissaoReinf ProcEmi { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }

        #region ShouldSerialize

        public bool ShouldSereializeNrRecibo() => !string.IsNullOrEmpty(NrRecibo);

        #endregion

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoServPrest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoServPrest
    {
        [XmlElement("ideEstabPrest")]
        public IdeEstabPrest IdeEstabPrest { get; set; }

    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstabPrest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeEstabPrest
    {
        [XmlElement("tpInscEstabPrest")]
        public TipoInscricaoEstabelecimento tpInscEstab { get; set; }

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
        public List<NfsReinf2020> Nfs { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddNfs(NfsReinf2020 item)
        {
            if (Nfs == null)
            {
                Nfs = new List<NfsReinf2020>();
            }

            Nfs.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Nfs (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Nfs</returns>
        public NfsReinf2020 GetNfs(int index)
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
        public int GetNfsReinf2020Count => (Nfs != null ? Nfs.Count : 0);
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.NfsReinf2020")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class NfsReinf2020
    {
        [XmlElement("serie")]
        public string Serie { get; set; }

        [XmlElement("numDocto")]
        public string NumDocto { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtEmissaoNF { get; set; }
#else
        public DateTimeOffset DtEmissaoNF { get; set; }
#endif

        [XmlElement("dtEmissaoNF")]
        public string DtEmissaoNFField
        {
            get => DtEmissaoNF.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtEmissaoNF = DateTime.Parse(value);
#else
            set => DtEmissaoNF = DateTimeOffset.Parse(value);
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

        [XmlElement("obs")]
        public string Obs { get; set; }

        [XmlElement("infoTpServ")]
        public List<InfoTpServ> InfoTpServ { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoTpServ(InfoTpServ item)
        {
            if (InfoTpServ == null)
            {
                InfoTpServ = new List<InfoTpServ>();
            }

            InfoTpServ.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf2020InfoTpServ (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf2020InfoTpServ</returns>
        public InfoTpServ GetInfoTpServ(int index)
        {
            if ((InfoTpServ?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoTpServ[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoTpServ
        /// </summary>
        public int GetInfoTpServCount => (InfoTpServ != null ? InfoTpServ.Count : 0);
#endif

    }

}
