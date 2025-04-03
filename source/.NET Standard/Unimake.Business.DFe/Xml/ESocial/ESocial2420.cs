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
    /// S-2420 - Cadastro de Benefício - Entes Públicos - Término
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2420")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenTerm/v_S_01_03_00", IsNullable = false)]
    public class ESocial2420 : XMLBaseESocial
    {
        /// <summary>
        /// Evento - Cadastro de Benefício - Entes Públicos - Término
        /// </summary>
        [XmlElement("evtCdBenTerm")]
        public EvtCdBenTerm EvtCdBenTerm { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento - Cadastro de Benefício - Entes Públicos - Término
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtCdBenTerm")]
    [ComVisible(true)]
#endif
    public class EvtCdBenTerm
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
        public IdeEvento2420 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do beneficiário e do benefício.
        /// </summary>
        [XmlElement("ideBeneficio")]
        public IdeBeneficio2420 IdeBeneficio { get; set; }

        /// <summary>
        /// Informações da cessação do benefício
        /// </summary>
        [XmlElement("infoBenTermino")]
        public InfoBenTermino2420 InfoBenTermino { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2420")]
    [ComVisible(true)]
#endif
    public class IdeEvento2420 : IdeEvento2206 { }

    /// <summary>
    /// Identificação do beneficiário e do benefício.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeBeneficio2420")]
    [ComVisible(true)]
#endif
    public class IdeBeneficio2420 : IdeBeneficio2416 { }

    /// <summary>
    /// Informações da cessação do benefício.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBenTermino2420")]
    [ComVisible(true)]
#endif
    public class InfoBenTermino2420
    {
        /// <summary>
        /// Data de cessação do benefício.
        /// Validação: Deve ser igual ou anterior à data atual.No
        /// caso de benefício reativado, também deve ser uma data
        /// igual ou posterior a dtEfetReativ do evento S-2418.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtTermBeneficio { get; set; }
#else
        public DateTimeOffset DtTermBeneficio { get; set; }
#endif

        /// <summary>
        /// Data de cessação do benefício.
        /// Validação: Deve ser igual ou anterior à data atual.No
        /// caso de benefício reativado, também deve ser uma data
        /// igual ou posterior a dtEfetReativ do evento S-2418.
        /// </summary>
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
        public MtvTermino MtvTermino { get; set; }

        /// <summary>
        /// Informar o CNPJ do órgão público sucessor.
        /// Validação: Preenchimento obrigatório e exclusivo se
        /// mtvTermino = [09].
        /// Deve ser um CNPJ válido e diferente da inscrição do
        /// declarante, considerando as particularidades aplicadas à
        /// informação de CNPJ de órgão público em S-1000. Além
        /// disso, deve possuir 14 (catorze) algarismos e ser diferente
        /// do CNPJ base do órgão público declarante(exceto se
        /// ideEmpregador/nrInsc tiver 14 (catorze) algarismos) e dos
        /// estabelecimentos informados através do evento S-1005
        /// </summary>
        [XmlElement("cnpjOrgaoSuc")]
        public string CnpjOrgaoSuc { get; set; }

        /// <summary>
        /// Preencher com o novo CPF do beneficiário.
        /// Validação: Preenchimento obrigatório e exclusivo se
        /// mtvTermino = [10].
        /// Deve ser um CPF válido e diferente do antigo CPF do
        /// beneficiário.
        /// </summary>
        [XmlElement("novoCPF")]
        public string NovoCPF { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCnpjOrgaoSuc() => !string.IsNullOrEmpty(CnpjOrgaoSuc);
        public bool ShouldSerializeNovoCPF() => !string.IsNullOrEmpty(NovoCPF);

        #endregion ShouldSerialize 
    }
}
