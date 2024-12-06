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
    /// S-2210 - Comunicação de Acidente de Trabalho
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2210")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCAT/v_S_01_02_00", IsNullable = false)]
    public class ESocial2210 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Comunicação de Acidente de Trabalho
        /// </summary>
        [XmlElement("evtCAT")]
        public EvtCAT EvtCAT { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Comunicação de Acidente de Trabalho
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtCAT")]
    [ComVisible(true)]
#endif
    public class EvtCAT
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
        public IdeEvento2210 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações de identificação do trabalhador e do vínculo
        /// </summary>
        [XmlElement("ideVinculo")]
        public IdeVinculo2210 IdeVinculo { get; set; }

        /// <summary>
        /// Comunicação de Acidente de Trabalho - CAT
        /// </summary>
        [XmlElement("cat")]
        public Cat Cat { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2210")]
    [ComVisible(true)]
#endif
    public class IdeEvento2210 : IdeEvento2205 { }

    /// <summary>
    /// Informações de identificação do trabalhador e do vínculo.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeVinculo2210")]
    [ComVisible(true)]
#endif
    public class IdeVinculo2210
    {
        /// <summary>
        /// Preencher com o número do CPF do trabalhador
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        /// <summary>
        /// Preencher com o número da matrícula do trabalhador
        /// </summary>
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        /// <summary>
        /// Preencher com o código da categoria do trabalhador
        /// </summary>
        [XmlElement("codCateg")]
#if INTEROP
        public CodCateg CodCateg { get; set; } = (CodCateg)(-1);
#else
        public CodCateg? CodCateg { get; set; }
#endif
        #region ShouldSerialize

        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);

#if INTEROP
        public bool ShouldSerializeCodCateg() => CodCateg != (CodCateg)(-1);
#else
        public bool ShouldSerializeCodCateg() => CodCateg != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Comunicação de Acidente de Trabalho - CAT.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Cat")]
    [ComVisible(true)]
#endif
    public class Cat
    {
        /// <summary>
        /// Data do acidente
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAcid { get; set; }
#else
        public DateTimeOffset DtAcid { get; set; }
#endif

        [XmlElement("dtAcid")]
        public string DtAcidField
        {
            get => DtAcid.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAcid = DateTime.Parse(value);
#else
            set => DtAcid = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Tipo de acidente de trabalho
        /// </summary>
        [XmlElement("tpAcid")]
        public TipoAcidenteTrabalho TpAcid { get; set; }

        /// <summary>
        /// Hora do acidente, no formato HHMM.
        /// </summary>
        [XmlElement("hrAcid")]
        public string HrAcid { get; set; }

        /// <summary>
        /// Horas trabalhadas antes da ocorrência do acidente, no formato HHMM.
        /// </summary>
        [XmlElement("hrsTrabAntesAcid")]
        public string HrsTrabAntesAcid { get; set; }

        /// <summary>
        /// Tipo de CAT
        /// </summary>
        [XmlElement("tpCat")]
        public TipoDeCAT TpCat { get; set; }

        /// <summary>
        /// Houve óbito?
        /// </summary>
        [XmlElement("indCatObito")]
        public SimNaoLetra IndCatObito { get; set; }

        /// <summary>
        /// Data do óbito
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtObito { get; set; }
#else
        public DateTimeOffset DtObito { get; set; }
#endif

        [XmlElement("dtObito")]
        public string DtObitoField
        {
            get => DtObito.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtObito = DateTime.Parse(value);
#else
            set => DtObito = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Houve comunicação à autoridade policial?
        /// </summary>
        [XmlElement("indComunPolicia")]
        public SimNaoLetra IndComunPolicia { get; set; }

        /// <summary>
        /// Preencher com o código da situação geradora do acidente ou da doença profissional.
        /// </summary>
        [XmlElement("codSitGeradora")]
        public string CodSitGeradora { get; set; }

        /// <summary>
        /// Iniciativa da CAT
        /// </summary>
        [XmlElement("iniciatCAT")]
        public IniciativaDaCAT IniciatCAT { get; set; }

        /// <summary>
        /// Observação
        /// </summary>
        [XmlElement("obsCAT")]
        public string ObsCAT { get; set; }

        /// <summary>
        /// Último dia trabalhado
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime UltDiaTrab { get; set; }
#else
        public DateTimeOffset UltDiaTrab { get; set; }
#endif

        [XmlElement("ultDiaTrab")]
        public string UltDiaTrabField
        {
            get => UltDiaTrab.ToString("yyyy-MM-dd");
#if INTEROP
            set => UltDiaTrab = DateTime.Parse(value);
#else
            set => UltDiaTrab = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Houve afastamento?
        /// </summary>
        [XmlElement("houveAfast")]
#if INTEROP
        public SimNaoLetra HouveAfast { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? HouveAfast { get; set; }
#endif

        /// <summary>
        /// Local do acidente
        /// </summary>
        [XmlElement("localAcidente")]
        public LocalAcidente LocalAcidente { get; set; }

        /// <summary>
        /// Parte do corpo atingida
        /// </summary>
        [XmlElement("parteAtingida")]
        public ParteAtingida ParteAtingida { get; set; }

        /// <summary>
        /// Agente causador
        /// </summary>
        [XmlElement("agenteCausador")]
        public AgenteCausador AgenteCausador { get; set; }

        /// <summary>
        /// Atestado médico
        /// </summary>
        [XmlElement("atestado")]
        public Atestado Atestado { get; set; }

        /// <summary>
        /// Grupo que indica a CAT anterior, no caso de CAT de reabertura ou de comunicação de óbito
        /// </summary>
        [XmlElement("catOrigem")]
        public CatOrigem CatOrigem { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeHrAcid() => !string.IsNullOrEmpty(HrAcid);

        public bool ShouldSerializeHrsTrabAntesAcid() => !string.IsNullOrEmpty(HrsTrabAntesAcid);

        public bool ShouldSerializeDtObitoField() => DtObito > DateTime.MinValue;

        public bool ShouldSerializeObsCAT() => !string.IsNullOrEmpty(ObsCAT);

        public bool ShouldSerializeUltDiaTrabField() => UltDiaTrab > DateTime.MinValue;

#if INTEROP
        public bool ShouldSerializeHouveAfast() => HouveAfast != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeHouveAfast() => HouveAfast != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Local do acidente
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalAcidente")]
    [ComVisible(true)]
#endif
    public class LocalAcidente
    {
        /// <summary>
        /// Tipo de local do acidente
        /// </summary>
        [XmlElement("tpLocal")]
        public TipoLocalAcidente TpLocal { get; set; }

        /// <summary>
        /// Especificação do local do acidente (pátio, rampa de acesso, posto de trabalho, etc.)
        /// </summary>
        [XmlElement("dscLocal")]
        public string DscLocal { get; set; }

        /// <summary>
        /// Tipo de logradouro
        /// </summary>
        [XmlElement("tpLograd")]
        public string TpLograd { get; set; }

        /// <summary>
        /// Descrição do logradouro
        /// </summary>
        [XmlElement("dscLograd")]
        public string DscLograd { get; set; }

        /// <summary>
        /// Número do logradouro
        /// </summary>
        [XmlElement("nrLograd")]
        public string NrLograd { get; set; }

        /// <summary>
        /// Complemento do logradouro
        /// </summary>
        [XmlElement("complemento")]
        public string Complemento { get; set; }

        /// <summary>
        /// Nome do bairro/distrito.
        /// </summary>
        [XmlElement("bairro")]
        public string Bairro { get; set; }

        /// <summary>
        /// Código de Endereçamento Postal - CEP
        /// </summary>
        [XmlElement("cep")]
        public string Cep { get; set; }

        /// <summary>
        /// Preencher com o código do município, conforme tabela do IBGE.
        /// </summary>
        [XmlElement("codMunic")]
        public string CodMunic { get; set; }

        /// <summary>
        /// Preencher com a sigla da Unidade da Federação - UF.
        /// </summary>
        [XmlElement("uf")]
#if INTEROP
        public UFBrasil Uf { get; set; } = (UFBrasil)(-1);
#else
        public UFBrasil? Uf { get; set; }
#endif

        /// <summary>
        /// Preencher com o código do país.
        /// </summary>
        [XmlElement("pais")]
        public string Pais { get; set; }

        /// <summary>
        /// Código de Endereçamento Postal.
        /// </summary>
        [XmlElement("codPostal")]
        public string CodPostal { get; set; }

        /// <summary>
        /// Identificação do local onde ocorreu o acidente ou do estabelecimento ao qual o trabalhador avulso está vinculado
        /// </summary>
        [XmlElement("ideLocalAcid")]
        public IdeLocalAcid IdeLocalAcid { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDscLocal() => !string.IsNullOrEmpty(DscLocal);

        public bool ShouldSerializeTpLograd() => !string.IsNullOrEmpty(TpLograd);

        public bool ShouldSerializeComplemento() => !string.IsNullOrEmpty(Complemento);

        public bool ShouldSerializeBairro() => !string.IsNullOrEmpty(Bairro);

        public bool ShouldSerializeCep() => !string.IsNullOrEmpty(Cep);

        public bool ShouldSerializeCodMunic() => !string.IsNullOrEmpty(CodMunic);

#if INTEROP
        public bool ShouldSerializeUf() => Uf != (UFBrasil)(-1);
#else
        public bool ShouldSerializeUf() => Uf != null;
#endif

        public bool ShouldSerializePais() => !string.IsNullOrEmpty(Pais);

        public bool ShouldSerializeCodPostal() => !string.IsNullOrEmpty(CodPostal);

        #endregion
    }

    /// <summary>
    /// Identificação do local onde ocorreu o acidente ou do estabelecimento ao qual o trabalhador avulso está vinculado.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeLocalAcid")]
    [ComVisible(true)]
#endif
    public class IdeLocalAcid
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 05.
        /// </summary>
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do estabelecimento, de acordo com o tipo de inscrição indicado no campo ideLocalAcid/tpInsc.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

    /// <summary>
    /// Detalhamento da parte atingida pelo acidente de trabalho
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ParteAtingida")]
    [ComVisible(true)]
#endif
    public class ParteAtingida
    {
        /// <summary>
        /// Preencher com o código correspondente à parte atingida
        /// </summary>
        [XmlElement("codParteAting")]
        public string CodParteAting { get; set; }

        /// <summary>
        /// Lateralidade da(s) parte(s) atingida(s).
        /// </summary>
        [XmlElement("lateralidade")]
        public Lateralidade Lateralidade { get; set; }
    }

    /// <summary>
    /// Detalhamento do agente causador do acidente de trabalho
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.AgenteCausador")]
    [ComVisible(true)]
#endif
    public class AgenteCausador
    {
        /// <summary>
        /// Preencher com o código correspondente ao agente causador do acidente.
        /// </summary>
        [XmlElement("codAgntCausador")]
        public string CodAgntCausador { get; set; }
    }

    /// <summary>
    /// Atestado médico
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Atestado")]
    [ComVisible(true)]
#endif
    public class Atestado
    {
        /// <summary>
        /// Data do atendimento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAtendimento { get; set; }
#else
        public DateTimeOffset DtAtendimento { get; set; }
#endif

        [XmlElement("dtAtendimento")]
        public string DtAtendimentoField
        {
            get => DtAtendimento.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAtendimento = DateTime.Parse(value);
#else
            set => DtAtendimento = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Hora do atendimento, no formato HHMM
        /// </summary>
        [XmlElement("hrAtendimento")]
        public string HrAtendimento { get; set; }

        /// <summary>
        /// Indicativo de internação
        /// </summary>
        [XmlElement("indInternacao")]
        public SimNaoLetra IndInternacao { get; set; }

        /// <summary>
        /// Duração estimada do tratamento, em dias
        /// </summary>
        [XmlElement("durTrat")]
        public string DurTrat { get; set; }

        /// <summary>
        /// Indicativo de afastamento do trabalho durante o tratamento
        /// </summary>
        [XmlElement("indAfast")]
        public SimNaoLetra IndAfast { get; set; }

        /// <summary>
        /// Preencher com a descrição da natureza da lesão
        /// </summary>
        [XmlElement("dscLesao")]
        public string DscLesao { get; set; }

        /// <summary>
        /// Descrição complementar da lesão
        /// </summary>
        [XmlElement("dscCompLesao")]
        public string DscCompLesao { get; set; }

        /// <summary>
        /// Diagnóstico provável
        /// </summary>
        [XmlElement("diagProvavel")]
        public string DiagProvavel { get; set; }

        /// <summary>
        /// Informar o código da tabela de Classificação Internacional de Doenças - CID
        /// </summary>
        [XmlElement("codCID")]
        public string CodCID { get; set; }

        /// <summary>
        /// Observação
        /// </summary>
        [XmlElement("observacao")]
        public string Observacao { get; set; }

        /// <summary>
        /// Médico/Dentista que emitiu o atestado
        /// </summary>
        [XmlElement("emitente")]
        public Emitente Emitente { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDscCompLesao() => !string.IsNullOrEmpty(DscCompLesao);

        public bool ShouldSerializeDiagProvavel() => !string.IsNullOrEmpty(DiagProvavel);

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Médico/Dentista que emitiu o atestado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Emitente")]
    [ComVisible(true)]
#endif
    public class Emitente
    {
        /// <summary>
        /// Nome do médico/dentista que emitiu o atestado.
        /// </summary>
        [XmlElement("nmEmit")]
        public string NmEmit { get; set; }

        /// <summary>
        /// Órgão de classe
        /// </summary>
        [XmlElement("ideOC")]
        public OrgaoDeClasseMedica IdeOC { get; set; }

        /// <summary>
        /// Número de inscrição no órgão de classe ou Registro do Ministério da Saúde (RMS)
        /// </summary>
        [XmlElement("nrOC")]
        public string NrOC { get; set; }

        /// <summary>
        /// Sigla da UF do órgão de classe.
        /// </summary>
        [XmlElement("ufOC")]
#if INTEROP
        public UFBrasil UfOC { get; set; } = (UFBrasil)(-1);
#else
        public UFBrasil? UfOC { get; set; }
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeUfOC() => UfOC != (UFBrasil)(-1);
#else
        public bool ShouldSerializeUfOC() => UfOC != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Grupo que indica a CAT anterior, no caso de CAT de reabertura ou de comunicação de óbito.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.CatOrigem")]
    [ComVisible(true)]
#endif
    public class CatOrigem
    {
        /// <summary>
        /// Informar o número do recibo da última CAT referente ao mesmo acidente/doença relacionada ao trabalho, nos casos:
        /// a) de CAT de reabertura;
        /// b) de óbito, quando houver CAT anterior.
        /// </summary>
        [XmlElement("nrRecCatOrig")]
        public string NrRecCatOrig { get; set; }
    }
}
