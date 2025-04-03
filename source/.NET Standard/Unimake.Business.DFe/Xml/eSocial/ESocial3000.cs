#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-3000 - Exclusão de Eventos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial3000")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtExclusao/v_S_01_03_00", IsNullable = false)]
    public class ESocial3000 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Exclusão de Eventos
        /// </summary>
        [XmlElement("evtExclusao")]
        public EvtExclusao EvtExclusao { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Exclusão de Eventos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtExclusao")]
    [ComVisible(true)]
#endif
    public class EvtExclusao
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        /// <summary>
        /// Informações de identificação do evento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento3000 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informação do evento que será excluído
        /// </summary>
        [XmlElement("infoExclusao")]
        public InfoExclusao InfoExclusao { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento3000")]
    [ComVisible(true)]
#endif
    public class IdeEvento3000 : IdeEvento { }

    /// <summary>
    /// Informação do evento que será excluído
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoExclusao")]
    [ComVisible(true)]
#endif
    public class InfoExclusao
    {
        /// <summary>
        /// Preencher com o tipo de evento. Ex.: S-1200, S-2200, etc
        /// </summary>
        [XmlElement("tpEvento")]
        public string TpEvento { get; set; }

        /// <summary>
        /// Preencher com o número do recibo do evento que será excluído
        /// </summary>
        [XmlElement("nrRecEvt")]
        public string NrRecEvt { get; set; }

        /// <summary>
        /// 
        /// </summary>
        [XmlElement("ideTrabalhador")]
        public IdeTrabalhador3000 IdeTrabalhador { get; set; }

        /// <summary>
        /// Identificação do período de apuração a que se refere o evento que será excluído
        /// </summary>
        [XmlElement("ideFolhaPagto")]
        public IdeFolhaPagto IdeFolhaPagto { get; set; }
    }

    /// <summary>
    /// Grupo que identifica a qual trabalhador se refere o evento a ser excluído
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhador3000")]
    [ComVisible(true)]
#endif
    public class IdeTrabalhador3000
    {
        /// <summary>
        /// Preencher com o número do CPF do trabalhador ou do beneficiário
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }
    }

    /// <summary>
    /// Grupo que identifica a qual período de apuração pertence o evento que será excluído
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeFolhaPagto")]
    [ComVisible(true)]
#endif
    public class IdeFolhaPagto
    {
        /// <summary>
        /// Indicativo de período de apuração
        /// </summary>
        [XmlElement("indApuracao")]
#if INTEROP
        public IndApuracao IndApuracao { get; set; } = (IndApuracao)(-1);
#else
        public IndApuracao? IndApuracao { get; set; }
#endif

        /// <summary>
        /// Informar o mês/ano (formato AAAA-MM) ou apenas o ano (formato AAAA) de referência das informações
        /// </summary>
        [XmlElement("perApur")]
        public string PerApur { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndApuracao() => IndApuracao != (IndApuracao)(-1);
#else
        public bool ShouldSerializeIndApuracao() => IndApuracao != null;
#endif

        #endregion ShouldSerialize
    }
}
