#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2410 - Cadastro de Benefício - Entes Públicos - Início
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2410")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenIn/v_S_01_03_00", IsNullable = false)]
    public class ESocial2410 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Cadastro de Benefício - Início
        /// </summary>
        [XmlElement("evtCdBenIn")]
        public EvtCdBenIn EvtCdBenIn { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Cadastro de Benefício - Início
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtCdBenIn")]
    [ComVisible(true)]
#endif
    public class EvtCdBenIn
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
        public IdeEvento2410 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações do beneficiário
        /// </summary>
        [XmlElement("beneficiario")]
        public Beneficiario2410 Beneficiario { get; set; }

        /// <summary>
        /// Informações do benefício - Início
        /// </summary>
        [XmlElement("infoBenInicio")]
        public InfoBenInicio InfoBenInicio { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2410")]
    [ComVisible(true)]
#endif
    public class IdeEvento2410 : IdeEvento2205 { }

    /// <summary>
    /// Informações do beneficiário
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Beneficiario2410")]
    [ComVisible(true)]
#endif
    public class Beneficiario2410
    {
        /// <summary>
        /// Preencher com o CPF do beneficiário
        /// </summary>
        [XmlElement("cpfBenef")]
        public string CpfBenef { get; set; }

        /// <summary>
        /// Matrícula do servidor/militar constante no Sistema de Administração de Recursos Humanos do órgão cujo vínculo deu ensejo ao benefício
        /// </summary>
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        /// <summary>
        /// Preencher com o CNPJ do órgão público responsável pela matrícula do servidor/militar. 
        /// Informação obrigatória se cadIni = [N], desde que haja informação de matrícula.
        /// </summary>
        [XmlElement("cnpjOrigem")]
        public string CnpjOrigem { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);

        public bool ShouldSerializeCnpjOrigem() => !string.IsNullOrEmpty(CnpjOrigem);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações do benefício - Início
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBenInicio")]
    [ComVisible(true)]
#endif
    public class InfoBenInicio
    {
        /// <summary>
        /// Indicar se a data de início do benefício é anterior à obrigatoriedade dos eventos não periódicos para o ente público no eSocial
        /// </summary>
        [XmlElement("cadIni")]
        public SimNaoLetra CadIni { get; set; }

        /// <summary>
        /// Indicar a situação do benefício no órgão declarante
        /// </summary>
        [XmlElement("indSitBenef")]
#if INTEROP
        public IndSitBenef IndSitBenef { get; set; } = (IndSitBenef)(-1);
#else
        public IndSitBenef? IndSitBenef { get; set; }
#endif

        /// <summary>
        /// Número do benefício
        /// </summary>
        [XmlElement("nrBeneficio")]
        public string NrBeneficio { get; set; }

        /// <summary>
        /// Data de início do benefício
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtIniBeneficio { get; set; }
#else
        public DateTimeOffset DtIniBeneficio { get; set; }
#endif

        [XmlElement("dtIniBeneficio")]
        public string DtIniBeneficioField
        {
            get => DtIniBeneficio.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIniBeneficio = DateTime.Parse(value);
#else
            set => DtIniBeneficio = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Informar a data de publicação da concessão do benefício, somente quando o ato concessório tiver vigência retroativa
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtPublic { get; set; }
#else
        public DateTimeOffset DtPublic { get; set; }
#endif

        [XmlElement("dtPublic")]
        public string DtPublicField
        {
            get => DtPublic.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtPublic = DateTime.Parse(value);
#else
            set => DtPublic = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Dados relativos ao benefício
        /// </summary>
        [XmlElement("dadosBeneficio")]
        public DadosBeneficio DadosBeneficio { get; set; }

        /// <summary>
        /// Grupo de informações de transferência de benefício
        /// </summary>
        [XmlElement("sucessaoBenef")]
        public SucessaoBenef SucessaoBenef { get; set; }

        /// <summary>
        /// Informações de mudança de CPF do beneficiário
        /// </summary>
        [XmlElement("mudancaCPF")]
        public MudancaCpf2410 MudancaCPF { get; set; }

        /// <summary>
        /// Informações da cessação do benefício
        /// </summary>
        [XmlElement("infoBenTermino")]
        public InfoBenTermino InfoBenTermino { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndSitBenef() => IndSitBenef != (IndSitBenef)(-1);
#else
        public bool ShouldSerializeIndSitBenef() => IndSitBenef != null;
#endif

        public bool ShouldSerializeDtPublicField() => DtPublic > DateTime.MinValue;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Dados relativos ao benefício
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosBeneficio")]
    [ComVisible(true)]
#endif
    public class DadosBeneficio
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
        /// Informar se o benefício foi concedido por determinação judicial
        /// </summary>
        [XmlElement("indDecJud")]
#if INTEROP
        public SimNaoLetra IndDecJud { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndDecJud { get; set; }
#endif

        /// <summary>
        /// Informações relativas à pensão por morte
        /// </summary>
        [XmlElement("infoPenMorte")]
        public InfoPenMorte InfoPenMorte { get; set; }

        #region ShouldSeriaize

        public bool ShouldSerializeDsc() => !string.IsNullOrEmpty(Dsc);

#if INTEROP
        public bool ShouldSerializeIndDecJud() => IndDecJud != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndDecJud() => IndDecJud != null;
#endif

        #endregion
    }

    /// <summary>
    /// Informações relativas à pensão por morte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPenMorte")]
    [ComVisible(true)]
#endif
    public class InfoPenMorte
    {
        /// <summary>
        /// Tipo de pensão por morte
        /// </summary>
        [XmlElement("tpPenMorte")]
        public TpPenMorte TpPenMorte { get; set; }

        /// <summary>
        /// Informações do instituidor da pensão por morte
        /// </summary>
        [XmlElement("instPenMorte")]
        public InstPenMorte InstPenMorte { get; set; }
    }

    /// <summary>
    /// Informações do instituidor da pensão por morte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InstPenMorte")]
    [ComVisible(true)]
#endif
    public class InstPenMorte
    {
        /// <summary>
        /// Preencher com o CPF do instituidor da pensão por morte
        /// </summary>
        [XmlElement("cpfInst")]
        public string CpfInst { get; set; }

        /// <summary>
        /// Data de óbito do instituidor da pensão por morte
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtInst { get; set; }
#else
        public DateTimeOffset DtInst { get; set; }
#endif

        [XmlElement("dtInst")]
        public string DtInstField
        {
            get => DtInst.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtInst = DateTime.Parse(value);
#else
            set => DtInst = DateTimeOffset.Parse(value);
#endif
        }
    }

    /// <summary>
    /// Grupo de informações de transferência de benefício
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SucessaoBenef")]
    [ComVisible(true)]
#endif
    public class SucessaoBenef
    {
        /// <summary>
        /// Informar o CNPJ do órgão público anterior
        /// </summary>
        [XmlElement("cnpjOrgaoAnt")]
        public string CnpjOrgaoAnt { get; set; }

        /// <summary>
        /// Número do benefício no ente público anterior
        /// </summary>
        [XmlElement("nrBeneficioAnt")]
        public string NrBeneficioAnt { get; set; }

        /// <summary>
        /// Preencher com a data da transferência do benefício para o órgão público declarante
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtTransf { get; set; }
#else
        public DateTimeOffset DtTransf { get; set; }
#endif

        [XmlElement("dtTransf")]
        public string DtTransfField
        {
            get => DtTransf.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtTransf = DateTime.Parse(value);
#else
            set => DtTransf = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Observação
        /// </summary>
        [XmlElement("observacao")]
        public string Observacao { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações de mudança de CPF do beneficiário
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.MudancaCpf2410")]
    [ComVisible(true)]
#endif
    public class MudancaCpf2410
    {
        /// <summary>
        /// Preencher com o número do CPF antigo do beneficiário
        /// </summary>
        [XmlElement("cpfAnt")]
        public string CpfAnt { get; set; }

        /// <summary>
        /// Preencher com o número do benefício anterior
        /// </summary>
        [XmlElement("nrBeneficioAnt")]
        public string NrBeneficioAnt { get; set; }

        /// <summary>
        /// Data de alteração do CPF
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAltCPF { get; set; }
#else
        public DateTimeOffset DtAltCPF { get; set; }
#endif

        [XmlElement("dtAltCPF")]
        public string DtAltCPFField
        {
            get => DtAltCPF.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAltCPF = DateTime.Parse(value);
#else
            set => DtAltCPF = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Observação
        /// </summary>
        [XmlElement("observacao")]
        public string Observacao { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações da cessação do benefício.
    /// Grupo preenchido exclusivamente caso seja necessário enviar evento de reativação de benefício cessado 
    /// antes do início dos eventos não periódicos para o ente público no eSocial ou para informação de diferenças de proventos e 
    /// pensões devidos sob a vigência dos eventos periódicos para o ente público no eSocial
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBenTermino")]
    [ComVisible(true)]
#endif
    public class InfoBenTermino
    {
        /// <summary>
        /// Data de cessação do benefício
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtTermBeneficio { get; set; }
#else
        public DateTimeOffset DtTermBeneficio { get; set; }
#endif

        [XmlElement("dtTermBeneficio")]
        public string DtTermBeneficioField
        {
            get => DtTermBeneficio.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtTermBeneficio = DateTime.Parse(value);
#else
            set => DtTermBeneficio = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Motivo da cessação do benefício
        /// </summary>
        [XmlElement("mtvTermino")]
        public string MtvTermino { get; set; }
    }
}
