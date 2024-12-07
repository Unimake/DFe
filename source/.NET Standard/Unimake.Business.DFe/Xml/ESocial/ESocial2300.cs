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
    /// S-2300 - Trabalhador Sem Vínculo de Emprego/Estatutário - Início
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2300")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTSVInicio/v_S_01_02_00", IsNullable = false)]
    public class ESocial2300 : XMLBaseESocial
    {
        /// <summary>
        /// Evento TSVE - Trabalhador Sem Vínculo de Emprego/Estatutário - Início
        /// </summary>
        [XmlElement("evtTSVInicio")]
        public EvtTSVInicio EvtTSVInicio { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

    }

    /// <summary>
    /// Evento TSVE - Trabalhador Sem Vínculo de Emprego/Estatutário - Início
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtTSVInicio")]
    [ComVisible(true)]
#endif
    public class EvtTSVInicio
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
        public IdeEvento2300 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Grupo de informações do trabalhador
        /// </summary>
        [XmlElement("trabalhador")]
        public Trabalhador2300 Trabalhador { get; set; }

        /// <summary>
        /// Trabalhador Sem Vínculo de Emprego/Estatutário - TSVE - Início. 
        /// </summary>
        [XmlElement("infoTSVInicio")]
        public InfoTSVInicio InfoTSVInicio { get; set; }

    }

    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2300")]
    [ComVisible(true)]
#endif
    public class IdeEvento2300 : IdeEvento2205 { }

    /// <summary>
    /// Grupo de informações do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Trabalhador2300")]
    [ComVisible(true)]
#endif
    public class Trabalhador2300 : Trabalhador2200 { }

    /// <summary>
    /// Trabalhador Sem Vínculo de Emprego/Estatutário - TSVE - Início. 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTSVInicio")]
    [ComVisible(true)]
#endif
    public class InfoTSVInicio
    {
        /// <summary>
        /// Indica se o evento se refere a cadastramento inicial (o ingresso do trabalhador no empregador declarante
        /// é anterior à data de inicio da obrigatoriedade de envio de seus eventos não periódicos) ou se refere a início de TSVE
        /// (o ingresso do trabalhador no empregador declarante é igual ou posterior à data de inicio da obrigatoriedade de 
        /// envio de seus eventos não periódicos).
        /// Valores válidos:
        /// S - Sim (Cadastramento Inicial)
        /// N - Não (Início de TSVE)
        /// </summary>
        [XmlElement("cadIni")]
        public SimNaoLetra CadIni { get; set; }

        /// <summary>
        /// Matrícula atribuída ao trabalhador pela empresa.
        /// Validação: Preenchimento obrigatório se indRetif = [1].
        /// No caso de retificação (indRetif = [2]), a matrícula deve ser preenchida caso tenha sido informada no evento original.
        /// O valor informado não pode conter a expressão 'eSocial' nas 7 (sete) primeiras posições.
        /// </summary>
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        /// <summary>
        /// Preencher com o código da categoria do trabalhador.
        /// </summary>
        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        /// <summary>
        ///         /// Data de início, que pode ser:
        /// a) Para o cooperado, a data de ingresso na cooperativa;
        /// b) Para o diretor não empregado, a data de posse no cargo;
        /// c) Para o dirigente sindical, a data de início do mandato no sindicato;
        /// d) Para o estagiário, a data de início do estágio;
        /// e) Para o trabalhador avulso, a data de ingresso no Órgão Gestor ded Mão de Obra - OGMO ou no sindicato;
        /// f) Para o servidor público exercente de cargo eletivo, a data de início do mandato;
        /// g) Para os demais trabalhadores, a data de início das atividades.
        /// Validação: Devem ser observadas as seguintes regras:
        /// a) Deve ser posterior à data de nascimento do trabalhador, não pode ser posterior a 30 (trinta) dias
        /// da data atual e deve ser igual ou anterior ao ano do óbito, se existente;
        /// b) Se cadIni = [S], deve ser igual ou posterior à data de início da obrigatoriedade dos eventos não periódicos para o empregador no eSocial;
        /// c) Se cadIni = [N], deve ser igual ou posterior à data de início da obrigatoriedade dos eventos não periódicos para o empregador no eSocial;
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtInicio { get; set; }
#else
        public DateTimeOffset DtInicio { get; set; }
#endif
        /// <summary>
        /// Data de início da prestação do serviço ou da execução da obra.
        /// </summary>
        [XmlElement("dtInicio")]
        public string DtInicioField
        {
            get => DtInicio.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtInicio = DateTime.Parse(value);
#else
            set => DtInicio = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Número que identifica o processo trabalhista, quando o início de TSVE se der por decisão judicial.
        /// Validação: Se preenchido, deve ser um processo judicial válido, com 20 (vinte) algarismos.
        /// </summary>
        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }

        /// <summary>
        /// Natureza da atividade.
        /// Validação: Preenchimento obrigatório se codCateg = [201, 202, 401, 731, 734, 738].
        /// Não deve ser preenchido se codCateg = [721, 722, 771, 901]
        /// </summary>
        [XmlElement("natAtividade")]
#if INTEROP
        public NatAtividade NatAtividade { get; set; } = (NatAtividade)(-1);
#else
        public NatAtividade? NatAtividade { get; set; }
#endif

        /// <summary>
        /// Grupo onde são fornecidas informações complementares, preenchidas conforme a categoria do TSVE
        /// </summary>
        [XmlElement("infoComplementares")]
        public InfoComplem2300 InfoComplementares { get; set; }

        /// <summary>
        /// Informações de mudança de CPF do trabalhador
        /// </summary>
        [XmlElement("mudancaCPF")]
        public MudancaCPF2300 MudancaCPF { get; set; }

        /// <summary>
        /// Informações de afastamento do TSVE
        /// </summary>
        [XmlElement("afastamento")]
        public Afastamento2300 Afastamento { get; set; }

        /// <summary>
        /// Informação do término do TSVE
        /// </summary>
        [XmlElement("termino")]
        public Termino Termino { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);
        public bool ShouldSerializeNrProcTrab() => !string.IsNullOrEmpty(NrProcTrab);
#if INTEROP
        public bool ShouldSerializeNatAtividade() => NatAtividade != (NatAtividade)(-1);
#else
        public bool ShouldSerializeNatAtividade() => NatAtividade != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações de mudança de CPF do trabalhador.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.MudancaCPF2300")]
    [ComVisible(true)]
#endif
    public class MudancaCPF2300 : MudancaCPF2200 { }

    /// <summary>
    /// Informações de afastamento do trabalhador.
    /// Preenchimento exclusivo em caso de trabalhador que permaneça afastado na data de início da 
    /// obrigatoriedade dos eventos não periódicos para o empregador no eSocial ou na data de transferência ou 
    /// alteração de CPF do empregado.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Afastamento2300")]
    [ComVisible(true)]
#endif
    public class Afastamento2300 : Afastamento2200 { }

    /// <summary>
    /// Grupo onde são fornecidas informações
    /// complementares, preenchidas conforme a categoria do TSVE.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComplem2300")]
    [ComVisible(true)]
#endif
    public class InfoComplem2300
    {
        /// <summary>
        /// Grupo que apresenta o cargo e/ou função ocupada pelo TSVE
        /// </summary>
        [XmlElement("cargoFuncao")]
        public CargoFuncao2300 CargoFuncao { get; set; }

        /// <summary>
        /// Informações da remuneração e periodicidade de pagamento
        /// </summary>
        [XmlElement("remuneracao")]
        public Remuneracao2300 Remuneracao { get; set; }

        /// <summary>
        /// Informações do Fundo de Garantia do Tempo de Serviço - FGTS
        /// </summary>
        [XmlElement("FGTS")]
        public FGTS2300 FGTS { get; set; }

        /// <summary>
        /// Informações relativas ao dirigente sindical
        /// </summary>
        [XmlElement("infoDirigenteSindical")]
        public InfoDirigenteSindical InfoDirigenteSindical { get; set; }

        /// <summary>
        /// Informações relativas ao trabalhador cedido/em exercício em outro órgão ou servidor público indicado para conselho, 
        /// preenchidas exclusivamente pelo cessionário/órgão de destino
        /// </summary>
        [XmlElement("infoTrabCedido")]
        public InfoTrabCedido InfoTrabCedido { get; set; }

        /// <summary>
        /// Informações relativas a servidor público exercente de mandato eletivo
        /// </summary>
        [XmlElement("infoMandElet")]
        public InfoMandElet2300 InfoMandElet { get; set; }

        /// <summary>
        /// Informações relativas ao estagiário ou ao beneficiário do Programa Nacional de Prestação de Serviço Civil Voluntário
        /// </summary>
        [XmlElement("infoEstagiario")]
        public InfoEstagiario2300 InfoEstagiario { get; set; }

        /// <summary>
        /// Estabelecimento (CNPJ, CNO, CAEPF) onde o trabalhador exercerá suas atividades
        /// </summary>
        [XmlElement("localTrabGeral")]
        public LocalTrabGeral2300 LocalTrabGeral { get; set; }
    }

    /// <summary>
    /// Grupo que apresenta o cargo e/ou função ocupada pelo TSVE.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.CargoFuncao2300")]
    [ComVisible(true)]
#endif
    public class CargoFuncao2300
    {
        /// <summary>
        /// Informar o nome do cargo.
        /// Validação: O preenchimento é obrigatório, exceto se for
        /// relativo a servidor nomeado em cargo em comissão(no
        /// evento S-2200, tpRegTrab = [2] e tpProv = [2]).
        /// </summary>
        [XmlElement("nmCargo")]
        public string NmCargo { get; set; }

        /// <summary>
        /// Informar a Classificação Brasileira de Ocupações - CBO
        /// relativa ao cargo.
        /// Validação: Informação obrigatória e exclusiva se nmCargo
        /// for preenchido.Se informado, deve ser um código válido e
        /// existente na tabela de CBO, com 6 (seis) posições.
        /// </summary>
        [XmlElement("CBOCargo")]
        public string CBOCargo { get; set; }

        /// <summary>
        /// Informar o nome da função de confiança/cargo em comissão.
        /// Validação: Preenchimento obrigatório se for relativo a
        /// servidor nomeado em cargo em comissão(no evento S2200, tpRegTrab = [2] e tpProv = [2]).
        /// </summary>
        [XmlElement("nmFuncao")]
        public string NmFuncao { get; set; }

        /// <summary>
        /// Informar o nome da função de confiança/cargo em comissão.
        /// Validação: Preenchimento obrigatório se for relativo a
        /// servidor nomeado em cargo em comissão(no evento S2200, tpRegTrab = [2] e tpProv = [2]).
        /// </summary>
        [XmlElement("CBOFuncao")]
        public string CBOFuncao { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNmCargo() => !string.IsNullOrEmpty(NmCargo);
        public bool ShouldSerializeCBOCargo() => !string.IsNullOrEmpty(CBOCargo);
        public bool ShouldSerializeNmFuncao() => !string.IsNullOrEmpty(NmFuncao);
        public bool ShouldSerializeCBOFuncao() => !string.IsNullOrEmpty(CBOFuncao);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações da remuneração e periodicidade de pagamento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Remuneracao2300")]
    [ComVisible(true)]
#endif
    public class Remuneracao2300 : Remuneracao2206 { }

    /// <summary>
    /// Informações do Fundo de Garantia do Tempo de Serviço - FGTS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.FGTS2300")]
    [ComVisible(true)]
#endif
    public class FGTS2300 : FGTS2200 { }

    /// <summary>
    /// Informações relativas ao dirigente sindical
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoDirigenteSindical")]
    [ComVisible(true)]
#endif
    public class InfoDirigenteSindical
    {
        /// <summary>
        /// Preencher com o código correspondente à categoria de origem do dirigente sindical.
        /// Validação: Deve ser um código válido e existente na Tabela 01, diferente de[401].
        /// </summary>
        [XmlElement("categOrig")]
        public CodCateg CategOrig { get; set; }

        /// <summary>
        /// Preencher com o código correspondente ao tipo de
        /// inscrição, conforme Tabela 05.
        /// Valores válidos:
        /// 1 - CNPJ
        /// 2 - CPF
        /// Validação: O preenchimento é obrigatório e
        /// exclusivo se infoDirigenteSindical/categOrig
        /// corresponder a "Empregado", "Agente Público",
        /// "Avulso" ou for igual a[721]
        /// </summary>
        [XmlElement("tpInsc")]
#if INTEROP
        public TpInsc TpInsc { get; set; } = (TpInsc)(-1);
#else
        public TpInsc? TpInsc { get; set; }
#endif

        /// <summary>
        /// Informar o número de inscrição do empregador de
        /// origem do dirigente sindical, de acordo com o tipo
        /// de inscrição indicado no campo
        /// infoDirigenteSindical/tpInsc.
        /// Validação: Preenchimento obrigatório e exclusivo se
        /// infoDirigenteSindical/tpInsc for informado.Se
        /// preenchido, deve ser um número de inscrição válido
        /// e diferente da inscrição do declarante, considerando
        /// as particularidades aplicadas à informação de CNPJ
        /// de órgão público em S-1000.
        /// Se infoDirigenteSindical/tpInsc = [1], deve possuir 14
        /// (catorze) algarismos e ser diferente do CNPJ base do
        /// empregador e dos estabelecimentos informados
        /// através do evento S-1005.
        /// Se infoDirigenteSindical/tpInsc = [2], deve possuir 11
        /// (onze) algarismos.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Preencher com a data de admissão (ou de início) do
        /// dirigente sindical na empresa de origem.
        /// Validação: O preenchimento é obrigatório se
        /// infoDirigenteSindical/categOrig corresponder a
        /// "Empregado", "Agente Público", "Avulso" ou for igual a [721].
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAdmOrig { get; set; }
#else
        public DateTimeOffset DtAdmOrig { get; set; }
#endif

        /// <summary>
        /// Preencher com a data de admissão (ou de início) do
        /// dirigente sindical na empresa de origem.
        /// Validação: O preenchimento é obrigatório se
        /// infoDirigenteSindical/categOrig corresponder a
        /// "Empregado", "Agente Público", "Avulso" ou for igual a [721].
        /// </summary>
        [XmlElement("dtAdmOrig")]
        public string DtAdmOrigField
        {
            get => DtAdmOrig.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAdmOrig = DateTime.Parse(value);
#else
            set => DtAdmOrig = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Preencher com a matrícula do trabalhador na
        /// empresa de origem.
        /// Validação: Preenchimento obrigatório se
        /// infoDirigenteSindical/categOrig corresponder a
        /// "Empregado" ou "Agente Público".
        /// </summary>
        [XmlElement("matricOrig")]
        public string MatricOrig { get; set; }

        /// <summary>
        /// Tipo de regime trabalhista.
        /// </summary>
        [XmlElement("tpRegTrab")]
#if INTEROP
        public TipoRegimeTrabalhista TpRegTrab { get; set; } = (TipoRegimeTrabalhista)(-1);
#else
        public TipoRegimeTrabalhista? TpRegTrab { get; set; }
#endif

        /// <summary>
        /// Tipo de regime previdenciário.
        /// </summary>
        [XmlElement("tpRegPrev")]
        public TipoRegimePrevidenciario TpRegPrev { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeTpInscField() => TpInsc != (TpInsc)(-1);
#else
        public bool ShouldSerializeTpInsc() => TpInsc != null;
#endif

        public bool ShouldSerializeNrInsc() => !string.IsNullOrEmpty(NrInsc);
        public bool ShouldSerializeDtAdmOrigField() => DtAdmOrig > DateTimeOffset.MinValue;
        public bool ShouldSerializeMatricOrig() => !string.IsNullOrEmpty(MatricOrig);

#if INTEROP
        public bool ShouldSerializeTpRegTrab() => TpRegTrab != (TipoRegimeTrabalhista)(-1);
#else
        public bool ShouldSerializeTpRegTrab() => TpRegTrab != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações relativas ao trabalhador cedido/em
    /// exercício em outro órgão ou servidor público
    /// indicado para conselho, preenchidas exclusivamente
    /// pelo cessionário/órgão de destino.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTrabCedido")]
    [ComVisible(true)]
#endif
    public class InfoTrabCedido
    {
        /// <summary>
        /// Preencher com o código correspondente à categoria
        /// de origem do trabalhador cedido ou do servidor
        /// público indicado para conselho.
        /// Validação: Deve ser um código válido e existente na
        /// Tabela 01, diferente de[305, 410].
        /// </summary>
        [XmlElement("categOrig")]
        public CodCateg CategOrig { get; set; }

        /// <summary>
        /// Informar o CNPJ do empregador/órgão público
        /// cedente/de origem.
        /// Validação: Deve ser um CNPJ diferente do CNPJ do
        /// empregador/órgão público e diferente dos
        /// estabelecimentos informados através do evento S1005.
        /// </summary>
        [XmlElement("cnpjCednt")]
        public string CnpjCednt { get; set; }

        /// <summary>
        /// Preencher com a matrícula do trabalhador no
        /// empregador/órgão público cedente/de origem.
        /// </summary>
        [XmlElement("matricCed")]
        public string MatricCed { get; set; }

        /// <summary>
        /// Preencher com a data de admissão (ou de exercício)
        /// do trabalhador no empregador/órgão público
        /// cedente/de origem.
        /// Validação: Deve ser uma data igual ou anterior a
        /// dtInicio e posterior a nascimento/dtNascto.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAdmCed { get; set; }
#else
        public DateTimeOffset DtAdmCed { get; set; }
#endif
        /// <summary>
        /// Preencher com a data de admissão (ou de exercício)
        /// do trabalhador no empregador/órgão público
        /// cedente/de origem.
        /// Validação: Deve ser uma data igual ou anterior a
        /// dtInicio e posterior a nascimento/dtNascto.
        /// </summary>
        [XmlElement("dtAdmCed")]
        public string DtAdmCedField
        {
            get => DtAdmCed.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAdmCed = DateTime.Parse(value);
#else
            set => DtAdmCed = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Tipo de regime trabalhista.
        /// </summary>
        [XmlElement("tpRegTrab")]
        public TipoRegimeTrabalhista TpRegTrab { get; set; }

        /// <summary>
        /// Tipo de regime previdenciário (ou Sistema de
        /// Proteção Social dos Militares das Forças Armadas).
        /// </summary>
        [XmlElement("tpRegPrev")]
        public TipoRegimePrevidenciario TpRegPrev { get; set; }
    }

    /// <summary>
    /// Informações relativas a servidor público exercente de mandato eletivo.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoMandElet2300")]
    [ComVisible(true)]
#endif
    public class InfoMandElet2300
    {
        /// <summary>
        /// Preencher com o código correspondente à categoria
        /// de origem do servidor.
        /// Validação: Deve ser um código válido e existente na
        /// Tabela 01, diferente de[304].
        /// </summary>
        [XmlElement("categOrig")]
        public CodCateg CategOrig { get; set; }

        /// <summary>
        /// Informar o CNPJ do órgão público de origem.
        /// </summary>
        [XmlElement("cnpjOrig")]
        public string CnpjOrig { get; set; }

        /// <summary>
        /// Preencher com a matrícula do servidor no órgão público de origem.
        /// </summary>
        [XmlElement("matricOrig")]
        public string MatricOrig { get; set; }

        /// <summary>
        /// Preencher com a data de exercício do servidor no
        /// órgão público de origem.
        /// Validação: Deve ser uma data anterior a dtInicio e
        /// igual ou posterior a 01/01/1890.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtExercOrig { get; set; }
#else

        public DateTimeOffset DtExercOrig { get; set; }
#endif

        /// <summary>
        /// Preencher com a data de exercício do servidor no
        /// órgão público de origem.
        /// Validação: Deve ser uma data anterior a dtInicio e
        /// igual ou posterior a 01/01/1890.
        /// </summary>
        [XmlElement("dtExercOrig")]
        public string DtExercOrigField
        {
            get => DtExercOrig.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtExercOrig = DateTime.Parse(value);
#else
            set => DtExercOrig = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Indicar se o servidor optou pela remuneração do cargo efetivo.
        /// </summary>
        [XmlElement("indRemunCargo")]
#if INTEROP
        public SimNaoLetra IndRemunCargo { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndRemunCargo { get; set; }
#endif

        /// <summary>
        /// Tipo de regime trabalhista.
        /// </summary>
        [XmlElement("tpRegTrab")]
        public TipoRegimeTrabalhista TpRegTrab { get; set; }

        /// <summary>
        /// Tipo de regime previdenciário.
        /// </summary>
        [XmlElement("tpRegPrev")]
        public TipoRegimePrevidenciario TpRegPrev { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndRemunCargo() => IndRemunCargo != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndRemunCargo() => IndRemunCargo != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações relativas ao estagiário ou ao beneficiário do Programa Nacional de Prestação de Serviço Civil Voluntário.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEstagiario2300")]
    [ComVisible(true)]
#endif
    public class InfoEstagiario2300
    {
        /// <summary>
        /// Natureza do estágio ou da prestação de serviço civil
        /// voluntário.
        /// Valores válidos:
        /// O - Obrigatório
        /// N - Não obrigatório
        /// Validação: Se o código de categoria for igual a[906],
        /// deve ser preenchido com[N].
        /// </summary>
        [XmlElement("natEstagio")]
        public NatEstagio NatEstagio { get; set; }

        /// <summary>
        /// Informar o nível do estágio ou da prestação de
        /// serviço civil voluntário.
        /// Validação: Preenchimento obrigatório se o código de
        /// categoria for igual a[901]. Se o código de categoria
        /// for igual a[906], não pode ser informado[9].
        /// </summary>
        [XmlElement("nivEstagio")]
#if INTEROP
        public NivEstagio NivEstagio { get; set; } = (NivEstagio)(-1);
#else
        public NivEstagio? NivEstagio { get; set; }
#endif

        /// <summary>
        /// Área de atuação do estagiário ou, no caso de
        /// prestação de serviço civil voluntário, jornada semanal
        /// do desempenho de atividades em formato decimal.
        /// </summary>
        [XmlElement("areaAtuacao")]
        public string AreaAtuacao { get; set; }

        /// <summary>
        /// Número da apólice de seguro.
        /// </summary>
        [XmlElement("nrApol")]
        public string NrApol { get; set; }

        /// <summary>
        /// Data prevista para o término do estágio ou da
        /// prestação de serviço civil voluntário.
        /// Validação: Deve ser uma data posterior à data de
        /// início do estágio ou da prestação de serviço civil
        /// voluntário.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtPrevTerm { get; set; }
#else
        public DateTimeOffset DtPrevTerm { get; set; }
#endif
        /// <summary>
        /// Data prevista para o término do estágio ou da
        /// prestação de serviço civil voluntário.
        /// Validação: Deve ser uma data posterior à data de
        /// início do estágio ou da prestação de serviço civil
        /// voluntário.
        /// </summary>
        [XmlElement("dtPrevTerm")]
        public string DtInicioField
        {
            get => DtPrevTerm.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtPrevTerm = DateTime.Parse(value);
#else
            set => DtPrevTerm = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Instituição de ensino ou entidade de formação/qualificação
        /// </summary>
        [XmlElement("instEnsino")]
        public InstEnsino InstEnsino { get; set; }

        /// <summary>
        /// Agente de integração
        /// </summary>
        [XmlElement("ageIntegracao")]
        public AgeIntegracao AgeIntegracao { get; set; }

        /// <summary>
        /// Supervisor do estágio
        /// </summary>
        [XmlElement("supervisorEstagio")]
        public SupervisorEstagio SupervisorEstagio { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeNivEstagio() => NivEstagio != (NivEstagio)(-1);
#else
        public bool ShouldSerializeNivEstagiol() => NivEstagio != null;
#endif

        public bool ShouldSerializeAreaAtuacao() => !string.IsNullOrEmpty(AreaAtuacao);
        public bool ShouldSerializeNrApol() => !string.IsNullOrEmpty(NrApol);

        #endregion ShouldSerialize
    }

    /// <summary>
    ///  Instituição de ensino ou entidade de formação/qualificação.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InstEnsino")]
    [ComVisible(true)]
#endif
    public class InstEnsino
    {
        /// <summary>
        /// Preencher com o CNPJ da instituição de ensino, no
        /// caso de estágio, ou da entidade de
        /// formação/qualificação, no caso de prestação de
        /// serviço civil voluntário.Deve ser preenchido apenas
        /// se a instituição/entidade for brasileira.
        /// Validação: Se informado, deve ser um CNPJ válido,
        /// com 14 (catorze) algarismos.
        /// </summary>
        [XmlElement("cnpjInstEnsino")]
        public string CnpjInstEnsino { get; set; }

        /// <summary>
        /// Informar a razão social.
        /// Validação: Preenchimento obrigatório e exclusivo se
        /// o campo cnpjInstEnsino não estiver preenchido.
        /// </summary>
        [XmlElement("nmRazao")]
        public string NmRazao { get; set; }

        /// <summary>
        /// Descrição do logradouro.
        /// Validação: Preenchimento obrigatório e exclusivo se
        /// o campo cnpjInstEnsino não estiver preenchido.
        /// </summary>
        [XmlElement("dscLograd")]
        public string DscLograd { get; set; }

        /// <summary>
        /// Número do logradouro.
        /// Se não houver número a ser informado, preencher
        /// com "S/N".
        /// Validação: Preenchimento obrigatório e exclusivo se
        /// o campo cnpjInstEnsino não estiver preenchido.
        /// </summary>
        [XmlElement("nrLograd")]
        public string NrLograd { get; set; }

        /// <summary>
        /// Nome do bairro/distrito.
        /// Validação: Preenchimento obrigatório e exclusivo se
        /// o campo cnpjInstEnsino não estiver preenchido.
        /// </summary>
        [XmlElement("bairro")]
        public string Bairro { get; set; }

        /// <summary>
        /// Código de Endereçamento Postal - CEP.
        /// Validação: Não informar se o campo cnpjInstEnsino
        /// estiver preenchido.Se informado, deve ser
        /// preenchido apenas com números, com 8 (oito)
        /// posições.
        /// </summary>
        [XmlElement("cep")]
        public string Cep { get; set; }

        /// <summary>
        /// Preencher com o código do município, conforme
        /// tabela do IBGE.
        /// Validação: Não informar se o campo cnpjInstEnsino
        /// estiver preenchido.Se informado, deve ser um
        /// código válido e existente na tabela do IBGE.
        /// </summary>
        [XmlElement("codMunic")]
        public string CodMunic { get; set; }

        /// <summary>
        /// Preencher com a sigla da Unidade da Federação - UF.
        /// Validação: Preenchimento obrigatório e exclusivo se
        /// o campo cnpjInstEnsino não estiver preenchido.
        /// </summary>
        [XmlElement("uf")]
#if INTEROP
        public UFBrasil Uf { get; set; } = (UFBrasil)(-1);
#else
        public UFBrasil? Uf { get; set; }
#endif

        #region ShouldSerialize
        public bool ShouldSerializeCnpjInstEnsino() => !string.IsNullOrEmpty(CnpjInstEnsino);
        public bool ShouldSerializeNmRazao() => !string.IsNullOrEmpty(NmRazao);
        public bool ShouldSerializeDscLograd() => !string.IsNullOrEmpty(DscLograd);
        public bool ShouldSerializeNrLograd() => !string.IsNullOrEmpty(NrLograd);
        public bool ShouldSerializeBairro() => !string.IsNullOrEmpty(Bairro);
        public bool ShouldSerializeCep() => !string.IsNullOrEmpty(Cep);
        public bool ShouldSerializeCodMunic() => !string.IsNullOrEmpty(CodMunic);

#if INTEROP
        public bool ShouldSerializeUf() => Uf != (UFBrasil)(-1);
#else
        public bool ShouldSerializeUf() => Uf != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Agente de integração
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.AgeIntegracao")]
    [ComVisible(true)]
#endif
    public class AgeIntegracao
    {
        /// <summary>
        /// CNPJ do agente de integração.
        /// Validação: Deve ser um CNPJ válido, com 14
        /// (catorze) algarismos.
        /// </summary>
        [XmlElement("cnpjAgntInteg")]
        public string CnpjAgntInteg { get; set; }
    }

    /// <summary>
    /// Supervisor do estágio. 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SupervisorEstagio")]
    [ComVisible(true)]
#endif
    public class SupervisorEstagio
    {
        /// <summary>
        /// CPF do responsável pela supervisão do estagiário.
        /// Validação: Deve ser um CPF válido
        /// </summary>
        [XmlElement("cpfSupervisor")]
        public string CpfSupervisor { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCpfSupervisor() => CpfSupervisor.HasOnlyNumbers();

        #endregion ShouldSerialize

    }

    /// <summary>
    /// Estabelecimento (CNPJ, CNO, CAEPF) onde o trabalhador exercerá suas atividades
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalTrabGeral2300")]
    [ComVisible(true)]
#endif
    public class LocalTrabGeral2300 : LocalTrabGeral2206 { }

    /// <summary>
    /// Informação do término do TSVE.
    /// Grupo preenchido exclusivamente caso seja necessário enviar cadastramento inicial referente a trabalhador com data de término anterior
    /// ao início dos eventos não periódicos para o empregador no eSocial (por exemplo, envio para pagamento de retiradas em meses posteriores
    /// à data de término e sob vigência dos eventos periódicos para o empregador no eSocial).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Termino")]
    [ComVisible(true)]
#endif
    public class Termino
    {
        /// <summary>
        /// Preencher com a data do término.
        /// Validação:
        /// a) Deve ser igual ou posterior à data de início do TSVE;
        /// b) Deve ser anterior à data de início da obrigatoriedade dos eventos não periódicos para o empregador.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtTerm { get; set; }
#else
        public DateTimeOffset DtTerm { get; set; }
#endif
        /// <summary>
        /// Preencher com a data do término.
        /// Validação:
        /// a) Deve ser igual ou posterior à data de início do TSVE;
        /// b) Deve ser anterior à data de início da obrigatoriedade dos eventos não periódicos para o empregador.
        /// </summary>
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
    }

}