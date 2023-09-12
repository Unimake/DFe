#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Collections.Generic;
using System.Globalization;
using Unimake.Business.DFe.Utility;

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
    public class EvtServPrest
    {
        [XmlElement("ideEvento")]
        public Reinf2020IdeEvento IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("infoServPrest")]
        public InfoServPrest InfoServPrest { get; set; }
    }

    public class Reinf2020IdeEvento
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

        [XmlElement("ideTomador")]
        public IdeTomador IdeTomador { get; set; }
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

        /// <summary>
        /// Validação: A inscrição informada deve ser compatível com o {tpInscEstab}.
        /// Se {indObra} = [0], o número informado deve ser um CNPJ.Se {indObra} for igual a[1, 2] o número informado deve ser um CNO.
        /// </summary>
        [XmlElement("nrInscEstabPrest")]
        public string NrInscEstabPrest { get; set; }
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
            get => VlrTotalBruto.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrTotalBruto = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VlrTotalBaseRet { get; set; }

        [XmlElement("vlrTotalBaseRet")]
        public string VlrTotalBaseRetField
        {
            get => VlrTotalBaseRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrTotalBaseRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VlrTotalRetPrin { get; set; }

        [XmlElement("vlrTotalRetPrin")]
        public string VlrTotalRetPrinField
        {
            get => VlrTotalRetPrin.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrTotalRetPrin = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VlrTotalRetAdic { get; set; }

        [XmlElement("vlrTotalRetAdic")]
        public string VlrTotalRetAdicField
        {
            get => VlrTotalRetAdic.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrTotalRetAdic = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VlrTotalNRetPrinc { get; set; }

        [XmlElement("vlrTotalNRetPrinc")]
        public string VlrTotalNRetPrincField
        {
            get => VlrTotalNRetPrinc.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrTotalNRetPrinc = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VlrTotalNRetAdic { get; set; }

        [XmlElement("vlrTotalNRetAdic")]
        public string VlrTotalNRetAdicField
        {
            get => VlrTotalNRetAdic.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrTotalNRetAdic = Converter.ToDouble(value);
        }

        [XmlElement("nfs")]
        public List<Reinf2020Nfs> Nfs { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddNfs(Reinf2020Nfs item)
        {
            if (Nfs == null)
            {
                Nfs = new List<Reinf2020Nfs>();
            }

            Nfs.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Nfs (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Nfs</returns>
        public Reinf2020Nfs GetNfs(int index)
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
        public int GetReinf2020NfsCount => (Nfs != null ? Nfs.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf2020Nfs")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Reinf2020Nfs
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
            get => VlrBruto.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrBruto = Converter.ToDouble(value);
        }

        [XmlElement("obs")]
        public string Obs { get; set; }

        [XmlElement("infoTpServ")]
        public List<Reinf2020InfoTpServ> InfoTpServ { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoTpServ(Reinf2020InfoTpServ item)
        {
            if (InfoTpServ == null)
            {
                InfoTpServ = new List<Reinf2020InfoTpServ>();
            }

            InfoTpServ.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf2020InfoTpServ (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf2020InfoTpServ</returns>
        public Reinf2020InfoTpServ GetInfoTpServ(int index)
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
        public int GetReinf2020InfoTpServCount => (InfoTpServ != null ? InfoTpServ.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf2020InfoTpServ")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Reinf2020InfoTpServ : InfoTpServ
    {
        /// <summary>
        /// O código informado deve existir na Tabela 06.
        /// </summary>
        [XmlElement("tpServico")]
        public string TpServico { get; set; }


        /// <summary>
        /// Valor da base de cálculo da retenção da contribuição previdenciária.
        /// </summary>
        [XmlIgnore]
        public double VlrBaseRet { get; set; }

        [XmlElement("vlrBaseRet")]
        public string VlrBaseRetField
        {
            get => VlrBaseRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrBaseRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Preencher com o valor da retenção apurada de acordo com o que determina a legislação vigente relativa aos serviços contidos na nota fiscal/fatura. 
        /// Se {indCPRB} = [0] preencher com valor correspondente a 11% de {vlrBaseRet}. 
        /// Se {indCPRB}= [1] preencher com valor correspondente a 3,5% de {vlrBaseRet}.
        /// </summary>
        [XmlIgnore]
        public double VlrRetencao { get; set; }

        [XmlElement("vlrRetencao")]
        public string VlrRetencaoField
        {
            get => VlrRetencao.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrRetencao = Converter.ToDouble(value);
        }

        /// <summary>
        /// Informar o valor da retenção destacada na nota fiscal relativo aos serviços subcontratados, se houver, desde que todos os documentos envolvidos se refiram à mesma competência e ao mesmo serviço, conforme disciplina a legislação.
        /// </summary>
        [XmlIgnore]
        public double VlrRetSub { get; set; }

        [XmlElement("vlrRetSub")]
        public string VlrRetSubField
        {
            get => VlrRetSub.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrRetSub = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da retenção principal que deixou de ser efetuada pelo contratante ou que foi depositada em juízo em decorrência de decisão judicial/administrativa.
        /// </summary>
        [XmlIgnore]
        public double VlrNRetPrinc { get; set; }

        [XmlElement("vlrNRetPrinc")]
        public string VlrNRetPrincField
        {
            get => VlrNRetPrinc.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrNRetPrinc = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor dos serviços prestados por segurados em condições especiais, cuja atividade permita concessão de aposentadoria especial após 15 anos de contribuição.
        /// </summary>
        [XmlIgnore]
        public double VlrServicos15 { get; set; }

        [XmlElement("vlrServicos15")]
        public string VlrServicos15Field
        {
            get => VlrServicos15.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrServicos15 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor dos serviços prestados por segurados em condições especiais, cuja atividade permita concessão de aposentadoria especial após 20 anos de contribuição.
        /// </summary>
        [XmlIgnore]
        public double VlrServicos20 { get; set; }

        [XmlElement("vlrServicos20")]
        public string VlrServicos20Field
        {
            get => VlrServicos20.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrServicos20 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor dos serviços prestados por segurados em condições especiais, cuja atividade permita concessão de aposentadoria especial após 25 anos de contribuição.
        /// </summary>
        [XmlIgnore]
        public double VlrServicos25 { get; set; }

        [XmlElement("vlrServicos25")]
        public string VlrServicos25Field
        {
            get => VlrServicos25.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrServicos25 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Adicional apurado de retenção da nota fiscal, caso os serviços tenham sido prestados sob condições especiais que ensejem aposentadoria especial aos trabalhadores após 15, 20, ou 25 anos de contribuição. 
        /// Preencher com o valor correspondente ao somatório de 4% sobre o {vlrServicos15} mais 3% sobre {vlrServicos20} mais 2% sobre {vlrServicos25}
        /// </summary>
        [XmlIgnore]
        public double VlrAdicional { get; set; }

        [XmlElement("vlrAdicional")]
        public string VlrAdicionalField
        {
            get => VlrAdicional.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrAdicional = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da retenção adicional que deixou de ser efetuada pelo contratante ou que foi depositada em juízo em decorrência de decisão judicial/administrativa
        /// </summary>
        [XmlIgnore]
        public double VlrNRetAdic { get; set; }

        [XmlElement("vlrNRetAdic")]
        public string vlrNRetAdicField
        {
            get => VlrNRetAdic.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrNRetAdic = Converter.ToDouble(value);
        }

        #region Should Serialize

        public bool ShouldSerializeVlrRetSubField() => VlrRetSub > 0;

        public bool ShouldSerializeVlrNRetPrincField() => VlrNRetPrinc > 0;

        public bool ShouldSerializeVlrServicos15Field() => VlrServicos15 > 0;

        public bool ShouldSerializeVlrServicos20Field() => VlrServicos20 > 0;

        public bool ShouldSerializeVlrServicos25Field() => VlrServicos25 > 0;

        public bool ShouldSerializeVlrAdicionalField() => VlrAdicional > 0;

        public bool ShouldSerializeVlrNRetAdicField() => VlrNRetAdic > 0;

        #endregion
    }
}
