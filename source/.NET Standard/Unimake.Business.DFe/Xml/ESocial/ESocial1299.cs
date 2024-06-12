#pragma warning disable CS1591
using System;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.GNRE;
using Unimake.Formatters;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1299")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// S-1299 - Fechamento dos Eventos Periódicos
    /// </summary>
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtFechaEvPer/v_S_01_02_00", IsNullable = false)]
    public class ESocial1299 : XMLBase
    {
        /// <summary>
        /// Evento Fechamento dos Eventos Periódicos
        /// </summary>
        [XmlElement("evtFechaEvPer")]
        public EvtFechaEvPer EvtFechaEvPer { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtFechamento")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Evento Fechamento dos Eventos Periódicos
    /// </summary>
    [Serializable()]
    public class EvtFechaEvPer
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id")]
        public string Id { get; set; }

        /// <summary>
        /// Informações de identificação do evento.
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEventoESocial1299 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador.
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Período de apuração a que se refere o fechamento.
        /// </summary>
        [XmlElement("infoFech")]
        public InfoFech IdePeriodo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEventoESocial1299")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
    [Serializable()]
    public class IdeEventoESocial1299
    {
        /// <summary>
        /// Indicativo de período de apuração.
        /// </summary>
        [XmlElement("indApuracao")]
        public IndApuracao IndApuracao { get; set; }

#if INTEROP
        public DateTime PerApur {get; set; }
#else
        /// <summary>
        /// Informar o mês/ano (formato AAAA-MM) de referência
        /// das informações, se indApuracao for igual a[1], ou apenas
        /// o ano(formato AAAA), se indApuracao for igual a[2].
        /// Validação: Deve ser um mês/ano ou ano válido, igual ou
        /// posterior ao início da obrigatoriedade dos eventos
        /// periódicos para o empregador.
        /// (yyyy-MM)
        /// </summary>
        [XmlIgnore]
        public DateTimeOffset PerApur { get; set; }
#endif

        /// <summary>
        /// Informar o mês/ano (formato AAAA-MM) de referência
        /// das informações, se indApuracao for igual a[1], ou apenas
        /// o ano(formato AAAA), se indApuracao for igual a[2].
        /// Validação: Deve ser um mês/ano ou ano válido, igual ou
        /// posterior ao início da obrigatoriedade dos eventos
        /// periódicos para o empregador.
        /// (yyyy-MM)
        /// </summary>
        [XmlElement("perApur")]
        public string PerApurField
        {
            get => PerApur.ToString("yyyy-MM");
#if INTEROP
            set => PerApur = DateTime.Parse(value);
#else
            set => PerApur = DateTimeOffset.Parse(value);
#endif
        }


        /// <summary>
        /// Indicativo do tipo de guia. Valores válidos:
        /// 1 - Documento de Arrecadação do eSocial - DAE
        /// </summary>
        ///[XmlElement("indGuia")]
        ///public IndGuia IndGuia { get; set; }

        /// <summary>
        /// Identificação do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Processo de emissão do evento.
        /// </summary>
        [XmlElement("procEmi")]
        public ProcEmiESocial ProcEmi { get; set; }

        /// <summary>
        /// Versão do processo de emissão do evento. Informar a versão do aplicativo emissor do evento.
        /// </summary>
        [XmlElement("verProc")]
        public string VerProc { get; set; }

        #region ShouldSerialize
        /*
         * Problema com versão do leitaute, por estar desatualizado (o exemplo), irei criar ShouldSerializes
         * para que não dê erro no teste unitário
         */
        //public bool ShouldSerializeIndGuiaField() => IndGuia.IsNullOrEmpty();

        /**/
        #endregion ShouldSerialize
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoFech")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações do fechamento.
    /// </summary>
    [Serializable()]
    public class InfoFech
    {
        /// <summary>
        /// Possui informações relativas a remuneração de
        /// trabalhadores ou provento/pensão de beneficiários no período de apuração?
        /// Valores válidos:
        /// S - Sim
        /// N - Não
        /// Validação: Se for igual a[S], deve existir evento de
        /// remuneração(S-1200, S-1202, S-1207, S-2299 ou S-2399)
        /// para o período de apuração, considerando o campo
        /// indGuia.Caso contrário, não deve existir evento de remuneração.
        /// </summary>
        [XmlElement("evtRemun")]
        public SimNaoESocial EvtRemun { get; set; }

        /// <summary>
        /// Possui informações de pagamento de rendimentos do
        /// trabalho no período de apuração?
        /// Valores válidos:
        /// S - Sim
        /// N - Não
        /// Validação: Se for igual a[S], deve existir o evento S-1210
        /// para o período de apuração, considerando o campo
        ///indGuia.Caso contrário, não deve existir o evento.
        /// </summary>
        [XmlElement("evtPgtos")]
        public SimNaoESocial EvtPgtos { get; set; }

        /// <summary>
        /// Possui informações de comercialização de produção?
        /// Valores válidos:
        /// S - Sim
        /// N - Não
        /// Validação: Se for igual a[S], deve existir o evento S-1260
        /// para o período de apuração, considerando o campo
        /// indGuia.Caso contrário, não deve existir o evento.
        /// </summary>
        [XmlElement("evtComProd")]
        public SimNaoESocial EvtComProd { get; set; }

        /// <summary>
        /// Contratou, por intermédio de sindicato, serviços de
        /// trabalhadores avulsos não portuários?
        /// Valores válidos:
        /// S - Sim
        /// N - Não
        /// Validação: Se for igual a[S], deve existir o evento S-1270
        /// para o período de apuração, considerando o campo
        /// indGuia.Caso contrário, não deve existir o evento.
        /// </summary>
        [XmlElement("evtContratAvNP")]
        public SimNaoESocial EvtContratAvNP { get; set; }
        /// <summary>
        /// Possui informações de desoneração de folha de
        /// pagamento ou, sendo empresa enquadrada no Simples,
        /// possui informações sobre a receita obtida em atividades
        /// cuja contribuição previdenciária incidente sobre a folha
        /// de pagamento é concomitantemente substituída e não
        /// substituída?
        /// Valores válidos:
        /// S - Sim
        /// N - Não
        /// Validação:
        /// Se for igual a [S], deve existir o evento S-1280 para o período de apuração. Caso contrário,
        /// não deve existir o evento
        /// </summary>
        [XmlElement("evtInfoComplPer")]
        public SimNaoESocial EvtInfoComplPer { get; set; }

        /// <summary>
        /// Indicativo de exclusão de apuração das aquisições de produção rural (eventos S-1250) do período
        /// de apuração.
        /// Valores Válidos:
        /// S - Sim
        /// Validação: Não informar se perApur >= [2021-07] ou se indApuracao = [2]. Preenchimento obrigatório
        /// caso o campo tenha sido informado em fechamento anterior do mesmo período de apuração.
        /// </summary>
        [XmlElement("indExcApur1250")]
        public string IndExcApur1250 { get; set; }

        /// <summary>
        /// Solicitação de transmissão imediata da DCTFWeb
        /// Valores válidos: S - Sim Validação: Não informar se perApur < [2021-10]. 
        /// Preenchimento obrigatório se perApur >= [2021-10] e (classTrib em S-1000 = [04] ou indGuia estiver informado).
        /// </summary>
        [XmlElement("transDCTFWeb")]
        public string TransDCTFWeb { get; set; }

        /// <summary>
        /// Indicativo de não validação das regras de fechamento, para que os grandes contribuintes possam reduzir o tempo
        /// de processamento do evento. O preenchimento deste campo implica a não execução da REGRA_VALIDA_FECHAMENTO_FOPAG.
        /// </summary>
        [XmlElement("naoValid")]
        public string NaoValid { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeIndExecApur1250Field() => !string.IsNullOrEmpty(IndExcApur1250);
        public bool ShouldSerializeTransDCTFWebField() => !string.IsNullOrEmpty(TransDCTFWeb);
        public bool ShouldSerializeNaoValidField() => !string.IsNullOrEmpty(NaoValid);


        #endregion ShouldSerialize
    }
}
