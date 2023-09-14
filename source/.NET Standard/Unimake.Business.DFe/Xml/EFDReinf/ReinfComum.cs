#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Globalization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEvento")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeEvento
    {
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("procEmi")]
        public ProcessoEmissaoReinf ProcEmi { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeContri")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeContri
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
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
