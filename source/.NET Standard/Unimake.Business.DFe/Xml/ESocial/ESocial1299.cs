#pragma warning disable CS1591
using System;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-1299 - Fechamento dos Eventos Periódicos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1299")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtFechaEvPer/v_S_01_03_00", IsNullable = false)]
    public class ESocial1299 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Fechamento dos Eventos Periódicos
        /// </summary>
        [XmlElement("evtFechaEvPer")]
        public EvtFechaEvPer EvtFechaEvPer { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Fechamento dos Eventos Periódicos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtFechaEvPer")]
    [ComVisible(true)]
#endif
    public class EvtFechaEvPer
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id")]
        public string ID { get; set; }

        /// <summary>
        /// Informações de identificação do evento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento1299 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações do fechamento
        /// </summary>
        [XmlElement("infoFech")]
        public InfoFech InfoFech { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1299")]
    [ComVisible(true)]
#endif
    public class IdeEvento1299 : IdeEvento1298 { }

    /// <summary>
    /// Informações do fechamento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoFech")]
    [ComVisible(true)]
#endif    
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
        public SimNaoLetra EvtRemun { get; set; }

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
        public SimNaoLetra EvtPgtos { get; set; }

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
        public SimNaoLetra EvtComProd { get; set; }

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
        public SimNaoLetra EvtContratAvNP { get; set; }

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
        public SimNaoLetra EvtInfoComplPer { get; set; }

        /// <summary>
        /// Indicativo de exclusão de apuração das aquisições de produção rural (eventos S-1250) do período
        /// de apuração.
        /// Valores Válidos:
        /// S - Sim
        /// Validação: Não informar se perApur >= [2021-07] ou se indApuracao = [2]. Preenchimento obrigatório
        /// caso o campo tenha sido informado em fechamento anterior do mesmo período de apuração.
        /// </summary>
        [XmlElement("indExcApur1250")]
#if INTEROP
        public SimNaoLetra IndExcApur1250 { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndExcApur1250 { get; set; }
#endif

        /// <summary>
        /// Solicitação de transmissão imediata da DCTFWeb
        /// Valores válidos: S - Sim Validação: Não informar se perApur menor que [2021-10]. 
        /// Preenchimento obrigatório se perApur >= [2021-10] e (classTrib em S-1000 = [04] ou indGuia estiver informado).
        /// </summary>
        [XmlElement("transDCTFWeb")]
#if INTEROP
        public SimNaoLetra TransDCTFWeb { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? TransDCTFWeb { get; set; }
#endif
        /// <summary>
        /// Indicativo de não validação das regras de fechamento, para que os grandes contribuintes possam reduzir o tempo
        /// de processamento do evento. O preenchimento deste campo implica a não execução da REGRA_VALIDA_FECHAMENTO_FOPAG.
        /// </summary>
        [XmlElement("naoValid")]
#if INTEROP
        public SimNaoLetra NaoValid { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? NaoValid { get; set; }
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndExcApur1250() => IndExcApur1250 != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndExcApur1250() => IndExcApur1250 != null;
#endif

#if INTEROP
        public bool ShouldSerializeTransDCTFWeb() => TransDCTFWeb != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeTransDCTFWeb() => TransDCTFWeb != null;
#endif

#if INTEROP
        public bool ShouldSerializeNaoValid() => NaoValid != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeNaoValid() => NaoValid != null;
#endif

        #endregion ShouldSerialize
    }
}
