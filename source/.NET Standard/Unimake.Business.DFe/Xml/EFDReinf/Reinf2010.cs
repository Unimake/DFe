#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf2010")]
    [ComVisible(true)]
#endif

    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTomadorServicos/v2_01_02", IsNullable = false)]
    public class Reinf2010 : XMLBase
    {
        [XmlElement("evtServTom")]
        public EvtServTom EvtServTom { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtServTom")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class EvtServTom : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEventoReinf2010 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("infoServTom")]
        public InfoServTom InfoServTom { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEventoReinf2010")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeEventoReinf2010
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
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoServTom")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoServTom
    {
        [XmlElement("ideEstabObra")]
        public IdeEstabObra IdeEstabObra { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstabObra")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeEstabObra
    {
        [XmlElement("tpInscEstab")]
        public TipoInscricaoEstabelecimento tpInscEstab { get; set; }

        /// <summary>
        /// Validação: A inscrição informada deve ser compatível com o {tpInscEstab}.
        /// Se {indObra} = [0], o número informado deve ser um CNPJ.Se {indObra} for igual a[1, 2] o número informado deve ser um CNO.
        /// </summary>
        [XmlElement("nrInscEstab")]
        public string NrInscEstab { get; set; }

        [XmlElement("indObra")]
        public IndicativoObra IndObra { get; set; }

        [XmlElement("idePrestServ")]
        public IdePrestServ IdePrestServ { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdePrestServ")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdePrestServ
    {
        /// <summary>
        /// Deve ser um CNPJ válido. Não pode pertencer ao declarante. 
        /// Se {indObra} for igual a [1] (empreitada total) o CNPJ do prestador terá que ser o proprietário do CNO informado no campo {nrInscEstab}.
        /// </summary>
        [XmlElement("cnpjPrestador")]
        public string CnpjPrestador { get; set; }

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

        [XmlElement("indCPRB")]
        public IndicativoCPRB IndCPRB { get; set; }

        [XmlElement("nfs")]
        public List<NfsReinf2010> Nfs { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddNfs(NfsReinf2010 item)
        {
            if (Nfs == null)
            {
                Nfs = new List<NfsReinf2010>();
            }

            Nfs.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Nfs (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Nfs</returns>
        public NfsReinf2010 GetNfsReinf2010(int index)
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
        public int GetNfsReinf2010Count => (Nfs != null ? Nfs.Count : 0);

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

        public bool ShouldSerializeVlrTotalRetAdic() => VlrTotalRetAdic > 0;
        
        public bool ShouldSerializeVlrTotalNRetPrinc() => VlrTotalNRetPrinc > 0;

        public bool ShouldSerializeVlrTotalNRetAdic() => VlrTotalNRetAdic > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.NfsReinf2010")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class NfsReinf2010
    {
        /// <summary>
        /// Informar o número de série da nota fiscal/fatura ou do Recibo Provisório de Serviço - RPS ou de outro documento fiscal válido.Preencher com 0 (zero) caso não exista número de série.
        /// </summary>
        [XmlElement("serie")]
        public string Serie { get; set; }

        /// <summary>
        /// Número da nota fiscal/fatura ou outro documento fiscal válido, como ReciboProvisório de Serviço - RPS, CT-e, entre outros.
        /// </summary>
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

        /// <summary>
        /// Preencher com o valor bruto da nota fiscal ou do Recibo Provisório de Serviço - RPS ou de outro documento fiscal válido
        /// Validação: Deve ser maior que zero.
        /// </summary>
        [XmlIgnore]
        public double VlrBruto { get; set; }

        [XmlElement("vlrBruto")]
        public string VlrBrutoField
        {
            get => VlrBruto.ToString("F2", CultureInfoReinf.Info);
            set => VlrBruto = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        /// <summary>
        /// Observações.
        /// </summary>
        [XmlElement("obs")]
        public string Obs { get; set; }

        /// <summary>
        /// Informações sobre os tipos de serviços constantes da nota fiscal.
        /// </summary>
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
        /// Retorna o elemento da lista InfoTpServ (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoTpServ</returns>
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

        #region ShouldSerialize

        public bool ShouldSerializeObs() => !string.IsNullOrEmpty(Obs);

        #endregion
    }


}
