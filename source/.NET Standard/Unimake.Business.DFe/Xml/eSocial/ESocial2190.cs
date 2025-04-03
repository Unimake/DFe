#pragma warning disable CS1591

using System;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2190 - Registro Preliminar de Trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2190")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAdmPrelim/v_S_01_03_00", IsNullable = false)]
    public class ESocial2190 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Registro Preliminar de Trabalhador
        /// </summary>
        [XmlElement("evtAdmPrelim")]
        public EvtAdmPrelim EvtAdmPrelim { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Registro Preliminar de Trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtAdmPrelim")]
    [ComVisible(true)]
#endif
    public class EvtAdmPrelim
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
        public IdeEvento2190 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações do registro preliminar do trabalhador
        /// </summary>
        [XmlElement("infoRegPrelim")]
        public InfoRegPrelim InfoRegPrelim { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2190")]
    [ComVisible(true)]
#endif
    public class IdeEvento2190
    {
        /// <summary>
        /// Informe [1] para arquivo original ou [2] para arquivo de retificação.
        /// </summary>
        [XmlElement("indRetif")]
        public IndicativoRetificacao IndRetif { get; set; }

        /// <summary>
        /// Preencher com o número do recibo do arquivo a ser retificado.
        /// </summary>
        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

        /// <summary>
        /// Identificação do ambiente.
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

        public bool ShouldSerializeNrRecibo() => !string.IsNullOrEmpty(NrRecibo);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações do registro preliminar do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRegPrelim")]
    [ComVisible(true)]
#endif
    public class InfoRegPrelim
    {
        /// <summary>
        /// Preencher com o número do CPF do trabalhador.
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        /// <summary>
        /// Preencher com a data de nascimento.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtNascto { get; set; }
#else
        public DateTimeOffset DtNascto { get; set; }
#endif

        [XmlElement("dtNascto")]
        public string DtNasctoField
        {
            get => DtNascto.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtNascto = DateTime.Parse(value);
#else
            set => DtNascto = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        ///	Preencher com a data de admissão do trabalhador (ou data de início, no caso de Trabalhador Sem Vínculo de Emprego/Estatutário - TSVE).
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAdm { get; set; }
#else
        public DateTimeOffset DtAdm { get; set; }
#endif

        [XmlElement("dtAdm")]
        public string DtAdmField
        {
            get => DtAdm.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAdm = DateTime.Parse(value);
#else
            set => DtAdm = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Matrícula atribuída ao trabalhador pela empresa.
        /// </summary>
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        /// <summary>
        /// Preencher com o código da categoria do trabalhador
        /// </summary>
        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        /// <summary>
        /// Natureza da atividade
        /// </summary>
        [XmlElement("natAtividade")]
#if INTEROP
        public NatAtividade NatAtividade { get; set; } = (NatAtividade)(-1);
#else
        public NatAtividade ? NatAtividade { get; set; }
#endif

        /// <summary>
        /// Informações referentes ao registro e à CTPS Digital
        /// </summary>
        [XmlElement("infoRegCTPS")]
        public InfoRegCTPS InfoRegCTPS { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeNatAtividade() => NatAtividade != (NatAtividade)(-1);
#else
        public bool ShouldSerializeNatAtividade() => NatAtividade != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações referentes ao registro eletrônico de empregados e à Carteira de Trabalho e Previdência Digital - CTPS Digital.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRegCTPS")]
    [ComVisible(true)]
#endif
    public class InfoRegCTPS
    {
        /// <summary>
        /// Informar a Classificação Brasileira de Ocupações - CBO relativa ao cargo.
        /// </summary>
        [XmlElement("CBOCargo")]
        public string CBOCargo { get; set; }

        /// <summary>
        /// Salário base do trabalhador, correspondente à parte fixa da remuneração
        /// </summary>
        [XmlIgnore]
        public double VrSalFx { get; set; }

        [XmlElement("vrSalFx")]
        public string VrSalFxField
        {
            get => VrSalFx.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSalFx = Converter.ToDouble(value);
        }

        /// <summary>
        /// Unidade de pagamento da parte fixa da remuneração.
        /// </summary>
        [XmlElement("undSalFixo")]
        public UndSalFixo UndSalFixo { get; set; }

        /// <summary>
        /// Tipo de contrato de trabalho
        /// </summary>
        [XmlElement("tpContr")]
        public TipoDeContratoDeTrabalho TpContr { get; set; }

        /// <summary>
        /// Data do término do contrato por prazo determinado
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtTerm { get; set; }
#else
        public DateTimeOffset DtTerm { get; set; }
#endif

        [XmlElement("dtTerm")]
        public string DtTermField
        {
            get => DtTerm.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtTerm = DateTime.Parse(value);
#else
            set => DtTerm = DateTimeOffset.Parse(value);
#endif
        }

        #region ShouldSerialize

        public bool ShouldSerializeDtTermField() => DtTerm > DateTime.MinValue;

        #endregion ShouldSerialize
    }
}
