#pragma warning disable CS1591

using System;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Runtime.InteropServices;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    ///  Evento 8299 - Baixa Judicial do Vínculo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1207")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtBaixa/v_S_01_03_00", IsNullable = false)]
    public class ESocial8299 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Baixa Judicial do Vínculo 
        /// </summary>
        [XmlElement("evtBaixa")]
        public EvtBaixa EvtBaixa { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Baixa Judicial do Vínculo 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtBaixa")]
    [ComVisible(true)]
#endif
    public class EvtBaixa
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
        public IdeEvento8299 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações de identificação do trabalhador e do vínculo
        /// </summary>
        [XmlElement("ideVinculo")]
        public IdeVinculo8299 IdeVinculo { get; set; }

        /// <summary>
        /// Informações relativas à baixa judicial do vínculo
        /// </summary>
        [XmlElement("infoBaixa")]
        public InfoBaixa InfoBaixa { get; set; }

    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento8299")]
    [ComVisible(true)]
#endif
    public class IdeEvento8299 : IdeEvento2190 { }

    /// <summary>
    /// Informações de identificação do trabalhador e do vínculo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeVinculo8299")]
    [ComVisible(true)]
#endif
    public class IdeVinculo8299 : IdeVinculo2206 { }

    /// <summary>
    /// Informações relativas à baixa judicial do vínculo.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBaixa")]
    [ComVisible(true)]
#endif
    public class InfoBaixa
    {
        /// <summary>
        /// Código de motivo do desligamento
        /// </summary>
        [XmlElement("mtvDeslig")]
        public MtvDeslig MtvDeslig { get; set; }

        /// <summary>
        /// Preencher com a data de desligamento do vínculo (último
        /// dia trabalhado).
        /// Validação: Deve ser uma data igual ou posterior a
        /// 24/09/2019 e igual ou anterior à data atual.No caso de
        /// empregado reintegrado e quando não se tratar de
        /// retificação do desligamento anterior à reintegração,
        /// também deve ser uma data igual ou posterior a
        /// dtEfetRetorno do evento S-2298.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtDeslig {get; set; }
#else
        public DateTimeOffset DtDeslig { get; set; }
#endif

        /// <summary>
        /// Preencher com a data de desligamento do vínculo (último
        /// dia trabalhado).
        /// Validação: Deve ser uma data igual ou posterior a
        /// 24/09/2019 e igual ou anterior à data atual.No caso de
        /// empregado reintegrado e quando não se tratar de
        /// retificação do desligamento anterior à reintegração,
        /// também deve ser uma data igual ou posterior a
        /// dtEfetRetorno do evento S-2298.
        /// </summary>
        [XmlElement("dtDeslig")]
        public string DtDesligField
        {
            get => DtDeslig.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtDeslig = DateTime.Parse(value);
#else
            set => DtDeslig = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Data projetada para o término do aviso prévio indenizado.
        ///Validação: Se preenchida, deve ser uma data posterior a dtDeslig.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtProjFimAPI {get; set; }
#else
        public DateTimeOffset DtProjFimAPI { get; set; }
#endif

        /// <summary>
        /// Data projetada para o término do aviso prévio indenizado.
        ///Validação: Se preenchida, deve ser uma data posterior a dtDeslig.
        /// </summary>
        [XmlElement("dtProjFimAPI")]
        public string DtProjFimAPIField
        {
            get => DtProjFimAPI.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtProjFimAPI = DateTime.Parse(value);
#else
            set => DtProjFimAPI = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Número que identifica o processo judicial onde a baixa do
        /// vínculo foi determinada.
        /// Validação: Deve ser um processo judicial válido, com 20
        /// (vinte) algarismos.
        /// </summary>
        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }

        /// <summary>
        /// Observação relevante sobre o desligamento do
        /// trabalhador, que não esteja consignada em outros campos
        /// </summary>
        [XmlElement("observacao")]
        public string Observacao { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDtProjFimAPIField() => DtProjFimAPI > DateTimeOffset.MinValue;
        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

        #endregion ShouldSerialize
    }

}