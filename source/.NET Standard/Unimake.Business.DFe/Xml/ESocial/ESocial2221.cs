#pragma warning disable CS1591
using System;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.SNCM;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2221 - Exame Toxicológico do Motorista Profissional Empregado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2221")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtToxic/v_S_01_03_00", IsNullable = false)]
    public class ESocial2221 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Exame Toxicológico do Motorista Profissional Empregado
        /// </summary>
        [XmlElement("evtToxic")]
        public EvtToxic EvtToxic { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Exame Toxicológico do Motorista Profissional Empregado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtToxic")]
    [ComVisible(true)]
#endif
    public class EvtToxic
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
        public IdeEvento2221 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações de identificação do trabalhador e do vínculo
        /// </summary>
        [XmlElement("ideVinculo")]
        public IdeVinculo2221 IdeVinculo { get; set; }

        /// <summary>
        /// Informações do exame toxicológico do motorista profissional
        /// </summary>
        [XmlElement("toxicologico")]
        public Toxicologico Toxicologico { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2221")]
    [ComVisible(true)]
#endif
    public class IdeEvento2221 : IdeEvento2205 { }

    /// <summary>
    /// Informações de identificação do trabalhador e do vínculo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeVinculo2221")]
    [ComVisible(true)]
#endif
    public class IdeVinculo2221 : IdeVinculo2206 { }

    /// <summary>
    /// Informações do exame toxicológico do motorista profissional.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Toxicologico")]
    [ComVisible(true)]
#endif
    public class Toxicologico
    {
        /// <summary>
        /// Data da realização do exame toxicológico.
        /// Validação: Deve ser uma data válida, igual ou anterior à
        /// data atual e igual ou posterior à data de início da
        /// obrigatoriedade deste evento para o empregador no eSocial.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtExame {get; set; }
#else
        public DateTimeOffset DtExame { get; set; }
#endif

        /// <summary>
        /// Data da realização do exame toxicológico.
        /// Validação: Deve ser uma data válida, igual ou anterior à
        /// data atual e igual ou posterior à data de início da
        /// obrigatoriedade deste evento para o empregador no eSocial.
        /// </summary>
        [XmlElement("dtExame")]
        public string DtExameField
        {
            get => DtExame.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtExame = DateTime.Parse(value);
#else
            set => DtExame = DateTimeOffset.Parse(value);
#endif
        }
        /// <summary>
        ///  CNPJ do laboratório responsável pela realização do
        /// exame.
        /// Validação: Deve ser um CNPJ válido, com 14 (catorze)
        /// algarismos.
        /// </summary>
        [XmlElement("cnpjLab")]
        public string CnpjLab { get; set; }

        /// <summary>
        /// Código do exame toxicológico.
        /// Validação: Deve possuir 11 (onze) caracteres, composto
        /// por duas letras(dois primeiros caracteres) e nove
        /// algarismos(últimos nove caracteres).
        /// </summary>
        [XmlElement("codSeqExame")]
        public string CodSeqExame { get; set; }

        /// <summary>
        /// Preencher com o nome do médico.
        /// </summary>
        [XmlElement("nmMed")]
        public string NmMed { get; set; }

        /// <summary>
        /// Número de inscrição do médico no Conselho Regional de
        /// Medicina - CRM.
        /// Validação: Preenchimento obrigatório, exceto se o
        /// endereço do trabalhador em S-2200 ou S-2205 vigente
        /// em dtExame for no exterior.
        /// </summary>
        [XmlElement("nrCRM")]
        public string NrCRM { get; set; }

        /// <summary>
        /// Preencher com a sigla da Unidade da Federação - UF de
        /// expedição do CRM.
        /// </summary>
        [XmlElement("ufCRM")]
#if INTEROP
        public UFBrasil UfCRM { get; set; } = (UFBrasil)(-1);
#else
        public UFBrasil? UfCRM { get; set; }
#endif

        #region ShouldSerialize
        public bool ShouldSerializeNrCRM() => !string.IsNullOrEmpty(NrCRM);

#if INTEROP
        public bool ShouldSerializeUfCRM() => UfCRM != (UFBrasil)(-1);
#else
        public bool ShouldSerializeUfCRM() => UfCRM != null;
#endif

        #endregion ShouldSerialize
    }

}
