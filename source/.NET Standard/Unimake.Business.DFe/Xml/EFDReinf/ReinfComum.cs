#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Globalization;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
    /// <summary>
    /// Configurações Cultura para o REINF
    /// </summary>
    public static class CultureInfoReinf
    {
        private static CultureInfo InfoField = new CultureInfo("pt-BR");

        public static CultureInfo Info
        {
            get
            {
                InfoField.NumberFormat.NumberDecimalSeparator = ",";

                return InfoField;
            }
        }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
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

    /// <summary>
    /// Informações de identificação do contribuinte 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeContri")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeContri
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição do contribuinte:
        /// 1 - CNPJ;
        /// 2 - CPF.
        /// </summary>
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do contribuinte de acordo com o tipo de
        /// inscrição indicado no campo { tpInsc }.
        /// Validação: Se {tpInsc
        /// } for igual a[1], deve ser um número de CNPJ válido.
        /// Se
        /// { tpInsc } for igual a[2], deve ser um CPF válido.
        /// Se for um CNPJ deve ser informada a raiz/base de oito posições, exceto se a
        /// natureza jurídica do contribuinte declarante for de Administração Pública
        /// Direta Federal, ou seja, [101-5, 104-0, 107-4, 116-3 ou 134-1], situação em
        /// que o campo deve ser informado com o CNPJ completo (14 posições).
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

    /// <summary>
    /// Informações sobre os tipos de serviços
    /// constantes da nota fiscal
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoTpServ")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoTpServ
    {
        [XmlElement("tpServico")]
        public string TpServico { get; set; }

        [XmlIgnore]
        public double VlrBaseRet { get; set; }

        [XmlElement("vlrBaseRet")]
        public string VlrBaseRetField
        {
            get => VlrBaseRet.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseRet = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrRetencao { get; set; }

        [XmlElement("vlrRetencao")]
        public string VlrRetencaoField
        {
            get => VlrRetencao.ToString("F2", CultureInfoReinf.Info);
            set => VlrRetencao = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrRetSub { get; set; }

        [XmlElement("vlrRetSub")]
        public string VlrRetSubField
        {
            get => VlrRetSub.ToString("F2", CultureInfoReinf.Info);
            set => VlrRetSub = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrNRetPrinc { get; set; }

        [XmlElement("vlrNRetPrinc")]
        public string VlrNRetPrincField
        {
            get => VlrNRetPrinc.ToString("F2", CultureInfoReinf.Info);
            set => VlrNRetPrinc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrServicos15 { get; set; }

        [XmlElement("vlrServicos15")]
        public string VlrServicos15Field
        {
            get => VlrServicos15.ToString("F2", CultureInfoReinf.Info);
            set => VlrServicos15 = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrServicos20 { get; set; }

        [XmlElement("vlrServicos20")]
        public string VlrServicos20Field
        {
            get => VlrServicos20.ToString("F2", CultureInfoReinf.Info);
            set => VlrServicos20 = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrServicos25 { get; set; }

        [XmlElement("vlrServicos25")]
        public string VlrServicos25Field
        {
            get => VlrServicos25.ToString("F2", CultureInfoReinf.Info);
            set => VlrServicos25 = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrAdicional { get; set; }

        [XmlElement("vlrAdicional")]
        public string VlrAdicionalField
        {
            get => VlrAdicional.ToString("F2", CultureInfoReinf.Info);
            set => VlrAdicional = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrNRetAdic { get; set; }

        [XmlElement("vlrNRetAdic")]
        public string VlrNRetAdicField
        {
            get => VlrNRetAdic.ToString("F2", CultureInfoReinf.Info);
            set => VlrNRetAdic = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrRetSubField() => VlrRetSub > 0;

        public bool ShouldSerializeVlrNRetPrincField() => VlrNRetPrinc > 0;

        public bool ShouldSerializeVlrServicos15Field() => VlrServicos15 > 0;

        public bool ShouldSerializeVlrServicos20Field() => VlrServicos20 > 0;

        public bool ShouldSerializeVlrServicos25Field() => VlrServicos25 > 0;

        public bool ShouldSerializeVlrAdicionalField() => VlrAdicional > 0;

        public bool ShouldSerializeVlrNRetAdicField() => VlrNRetAdic > 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações de processos relacionados a não retenção de contribuição
    /// previdenciária.
    /// Validação: A soma dos valores informados no campo {valorPrinc
    /// }
    /// deste
    /// grupo, com exceção dos valores informados para {indSusp} = [92], deve ser
    /// igual a {vlrTotalNRetPrinc}
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcRetPr")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoProcRetPr
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de processo:
        /// 1 - Administrativo;
        /// 2 - Judicial.
        /// </summary>
        [XmlElement("tpProcRetPrinc")]
        public TipoProcesso TpProcRetPrinc { get; set; }

        [XmlElement("nrProcRetPrinc")]
        public string NrProcRetPrinc { get; set; }

        [XmlElement("codSuspPrinc")]
        public string CodSuspPrinc { get; set; }

        [XmlIgnore]
        public double ValorPrinc { get; set; }

        [XmlElement("valorPrinc")]
        public string ValorPrincField
        {
            get => ValorPrinc.ToString("F2", CultureInfoReinf.Info);
            set => ValorPrinc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeCodSuspPrinc() => !string.IsNullOrEmpty(CodSuspPrinc);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações de processos relacionados a não retenção de contribuição
    /// previdenciária adicional.
    /// Validação: A soma dos valores informados no campo { valorAdic}
    /// deste
    /// grupo, com exceção dos valores informados para {indSusp
    /// } = [92], deve ser
    /// igual a { vlrTotalNRetAdic }.
    /// </summary>
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

        /// <summary>
        /// - Código do indicativo da suspensão atribuído pelo contribuinte. Este campo
        /// deve ser utilizado se, num mesmo processo, houver mais de uma matéria
        /// tributária objeto de contestação e as decisões forem diferentes para cada uma.
        /// Validação: Preenchimento obrigatório se houver mais de uma informação de
        /// indicativo de suspensão para um mesmo processo.
        /// Se informado, deve constar na tabela de processos (R-1070), campo
        /// {codSusp}
        ///e deve estar vinculado ao número do processo informado em
        ///{nrProcRetAdic}.
        /// </summary>
        [XmlElement("codSuspAdic")]
        public string CodSuspAdic { get; set; }

        [XmlIgnore]
        public double ValorAdic { get; set; }

        /// <summary>
        /// Valor da retenção de contribuição previdenciária adicional que deixou de ser
        ///efetuada em função de processo administrativo ou judicial.
        /// </summary>
        [XmlElement("valorAdic")]
        public string ValorAdicField
        {
            get => ValorAdic.ToString("F2", CultureInfoReinf.Info);
            set => ValorAdic = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeCodSuspAdic() => !string.IsNullOrEmpty(CodSuspAdic);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Endereço do beneficiário residente ou domiciliado no exterior
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EndExt")]
    [ComVisible(true)]
#endif
    public class EndExt
    {
        [XmlElement("dscLograd")]
        public string DscLograd { get; set; }

        [XmlElement("nrLograd")]
        public string NrLograd { get; set; }

        [XmlElement("complem")]
        public string Complem { get; set; }

        [XmlElement("bairro")]
        public string Bairro { get; set; }

        [XmlElement("cidade")]
        public string Cidade { get; set; }

        [XmlElement("estado")]
        public string Estado { get; set; }

        [XmlElement("codPostal")]
        public string CodPostal { get; set; }

        [XmlElement("telef")]
        public string Telef { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDscLograd() => !string.IsNullOrEmpty(DscLograd);

        public bool ShouldSerializeNrLograd() => !string.IsNullOrEmpty(NrLograd);

        public bool ShouldSerializeComplem() => !string.IsNullOrEmpty(Complem);

        public bool ShouldSerializeBairro() => !string.IsNullOrEmpty(Bairro);

        public bool ShouldSerializeCidade() => !string.IsNullOrEmpty(Cidade);

        public bool ShouldSerializeEstado() => !string.IsNullOrEmpty(Estado);

        public bool ShouldSerializeCodPostal() => !string.IsNullOrEmpty(CodPostal);

        public bool ShouldSerializeTelef() => !string.IsNullOrEmpty(Telef);

        #endregion ShouldSerialize
    }

}
