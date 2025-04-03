#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2416 - Cadastro de Benefício - Entes Públicos - Alteração
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2416")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenAlt/v_S_01_03_00", IsNullable = false)]
    public class ESocial2416 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Cadastro de Benefício - Alteração
        /// </summary>
        [XmlElement("evtCdBenAlt")]
        public EvtCdBenAlt EvtCdBenAlt { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Cadastro de Benefício - Alteração
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtCdBenAlt")]
    [ComVisible(true)]
#endif
    public class EvtCdBenAlt
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
        public IdeEvento2416 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do beneficiário e do benefício
        /// </summary>
        [XmlElement("ideBeneficio")]
        public IdeBeneficio2416 IdeBeneficio { get; set; }

        /// <summary>
        /// Informações do benefício - Alteração
        /// </summary>
        [XmlElement("infoBenAlteracao")]
        public InfoBenAlteracao InfoBenAlteracao { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2416")]
    [ComVisible(true)]
#endif
    public class IdeEvento2416 : IdeEvento2205 { }

    /// <summary>
    /// Identificação do beneficiário e do benefício
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeBeneficio2416")]
    [ComVisible(true)]
#endif
    public class IdeBeneficio2416
    {
        /// <summary>
        /// Informar o CPF do beneficiário
        /// </summary>
        [XmlElement("cpfBenef")]
        public string CpfBenef { get; set; }

        /// <summary>
        /// Número do benefício
        /// </summary>
        [XmlElement("nrBeneficio")]
        public string NrBeneficio { get; set; }
    }

    /// <summary>
    /// Informações do benefício - Alteração
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBenAlteracao")]
    [ComVisible(true)]
#endif
    public class InfoBenAlteracao
    {
        /// <summary>
        /// Data de alteração das informações relativas ao benefício
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAltBeneficio { get; set; }
#else
        public DateTimeOffset DtAltBeneficio { get; set; }
#endif

        [XmlElement("dtAltBeneficio")]
        public string DtAltBeneficioField
        {
            get => DtAltBeneficio.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAltBeneficio = DateTime.Parse(value);
#else
            set => DtAltBeneficio = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Dados relativos ao benefício
        /// </summary>
        [XmlElement("dadosBeneficio")]
        public DadosBeneficio2416 DadosBeneficio { get; set; }
    }

    /// <summary>
    /// Dados relativos ao benefício
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosBeneficio2416")]
    [ComVisible(true)]
#endif
    public class DadosBeneficio2416
    {
        /// <summary>
        /// Tipo de benefício
        /// </summary>
        [XmlElement("tpBeneficio")]
        public string TpBeneficio { get; set; }

        /// <summary>
        /// Tipo de plano de segregação da massa
        /// </summary>
        [XmlElement("tpPlanRP")]
        public PlanoSegregacaoDaMassa TpPlanRP { get; set; }

        /// <summary>
        /// Descrição do instrumento ou situação que originou o pagamento do benefício
        /// </summary>
        [XmlElement("dsc")]
        public string Dsc { get; set; }

        /// <summary>
        /// Indicativo de suspensão do benefício
        /// </summary>
        [XmlElement("indSuspensao")]
        public SimNaoLetra IndSuspensao { get; set; }

        /// <summary>
        /// Informações relativas à pensão por morte
        /// </summary>
        [XmlElement("infoPenMorte")]
        public InfoPenMorte2416 InfoPenMorte { get; set; }

        /// <summary>
        /// Informações referentes à suspensão do benefício
        /// </summary>
        [XmlElement("suspensao")]
        public Suspensao Suspensao { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDsc() => !string.IsNullOrEmpty(Dsc);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações relativas à pensão por morte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPenMorte2416")]
    [ComVisible(true)]
#endif
    public class InfoPenMorte2416
    {
        /// <summary>
        /// Tipo de pensão por morte
        /// </summary>
        [XmlElement("tpPenMorte")]
        public TpPenMorte TpPenMorte { get; set; }
    }

    /// <summary>
    /// Informações referentes à suspensão do benefício
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Suspensao")]
    [ComVisible(true)]
#endif
    public class Suspensao
    {
        /// <summary>
        /// Motivo da suspensão do benefício
        /// </summary>
        [XmlElement("mtvSuspensao")]
        public MtvSuspensao MtvSuspensao { get; set; }

        /// <summary>
        /// Descrição do motivo da suspensão do benefício
        /// </summary>
        [XmlElement("dscSuspensao")]
        public string DscSuspensao { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDscSuspensao() => !string.IsNullOrEmpty(DscSuspensao);

        #endregion ShouldSerialize
    }
}
