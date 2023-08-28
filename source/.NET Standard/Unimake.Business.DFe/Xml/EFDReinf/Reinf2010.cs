#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Globalization;
using Unimake.Business.DFe.Utility;

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
    public class EvtServTom
    {
        [XmlElement("ideEvento")]
        public Reinf2010IdeEvento IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("infoServTom")]
        public InfoServTom InfoServTom { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf2010IdeEvento")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Reinf2010IdeEvento
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

        /// <summary>
        /// Deve corresponder à soma dos valores informados no {vlrBruto} dos registros vinculados.
        /// </summary>
        [XmlElement("vlrTotalBruto")]
        public string VlrTotalBruto { get; set; }

        /// <summary>
        /// Preencher com a soma da base de cálculo da retenção da contribuição previdenciária das notas fiscais emitidas para o contratante.
        /// Deve corresponder à soma dos valores informados no campo {vlrBaseRet} dos registros vinculados.
        /// </summary>
        [XmlElement("vlrTotalRet")]
        public string VlrTotalBaseRet { get; set; }

        /// <summary>
        /// Soma do valor da retenção sobre o valor das notas fiscais de serviço emitidas para o contratante.
        /// Deve corresponder à soma dos valores informados no campo {vlrRetencao}, subtraído da soma dos valores informados no campo {vlrRetSub} dos registros vinculados.
        /// </summary>
        [XmlElement("vlrTotalRetPrinc")]
        public string VlrTotalRetPrinc { get; set; }

        /// <summary>
        /// Deve corresponder à soma dos valores informados no campo {vlrAdicional} dos registros vinculados.
        /// </summary>
        [XmlElement("vlrTotalRetAdic")]
        public string VlrTotalRetAdic { get; set; }

        /// <summary>
        /// Valor da retenção principal que deixou de ser efetuada pelo contratante ou que foi depositada em juízo em decorrência de decisão judicial.
        /// Deve corresponder à soma dos valores informados no campo {vlrNRetPrinc} dos registros vinculados.
        /// </summary>
        [XmlElement("vlrTotalNRetPrinc")]
        public string VlrTotalNRetPrinc { get; set; }

        /// <summary>
        /// Valor da retenção adicional que deixou de ser efetuada pelo contratante ou que foi depositada em juízo em decorrência de decisão judicial.
        /// </summary>
        [XmlElement("vlrTotalNRetAdic")]
        public string VlrTotalNRetAdic { get; set; }

        [XmlElement("indCPRB")]
        public IndicativoCPRB IndCPRB { get; set; }

        [XmlElement("nfs")]
        public List<Nfs> Nfs { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddNfs(Nfs item)
        {
            if (Nfs == null)
            {
                Nfs = new List<Nfs>();
            }

            Nfs.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Nfs (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Nfs</returns>
        public Nfs GetNfs(int index)
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

        public bool ShouldSerializeVlrTotalRetAdic() => !string.IsNullOrEmpty(VlrTotalRetAdic);

        public bool ShouldSerializeVlrTotalNRetPrinc() => !string.IsNullOrEmpty(VlrTotalNRetPrinc);

        public bool ShouldSerializeVlrTotalNRetAdic() => !string.IsNullOrEmpty(VlrTotalNRetAdic);


        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Nfs")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Nfs
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
            get => VlrBruto.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrBruto = Converter.ToDouble(value);
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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoTpServ")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoTpServ
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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcRetPr")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoProcRetPr
    {
        [XmlElement("tpProcRetPrinc")]
        public TipoProcesso TpProcRetPrinc { get; set; }

        /// <summary>
        /// Informar o número do processo administrativo/judicial.
        /// </summary>
        [XmlElement("nrProcRetPrinc")]
        public string NrProcRetPrinc { get; set; }

        /// <summary>
        /// Código do indicativo da suspensão atribuído pelo contribuinte. 
        /// Este campo deve ser utilizado se, num mesmo processo, houver mais de uma matéria tributária objeto de contestação e as decisões forem diferentes para cada uma.
        /// </summary>
        [XmlElement("codSuspPrinc")]
        public string CodSuspPrinc { get; set; }

        /// <summary>
        /// Valor da retenção de contribuição previdenciária principal que deixou de ser efetuada em função de processo administrativo ou judicial.
        /// </summary>
        [XmlIgnore]
        public double ValorPrinc { get; set; }

        [XmlElement("valorPrinc")]
        public string ValorPrincField
        {
            get => ValorPrinc.ToString("F2", CultureInfo.InvariantCulture);
            set => ValorPrinc = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeCodSuspPrinc() => !string.IsNullOrEmpty(CodSuspPrinc);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcRetAd")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoProcRetAd
    {
        [XmlElement("tpProcRetAdic")]
        public TipoProcesso TpProcRetAdic { get; set; }

        [XmlElement("nrProcRetAdic")]
        public string NrProcRetAdic { get; set; }

        [XmlElement("codSuspAdic")]
        public string CodSuspAdic { get; set; }

        [XmlIgnore]
        public double ValorAdic { get; set; }

        [XmlElement("valorAdic")]
        public string ValorAdicField
        {
            get => ValorAdic.ToString("F2", CultureInfo.InvariantCulture);
            set => ValorAdic = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeCodSuspAdic() => !string.IsNullOrEmpty(CodSuspAdic);

        #endregion
    }
}
