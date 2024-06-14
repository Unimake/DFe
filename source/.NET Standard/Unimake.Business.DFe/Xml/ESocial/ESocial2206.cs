#pragma warning disable CS1591
using System;
using System.Globalization;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2206")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Evento Alteração de Contrato de Trabalho/Relação Estatutária.
    /// </summary>
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAltContratual/v_S_01_02_00", IsNullable = false)]
    public class ESocial2206 : XMLBase
    {
        /// <summary>
        /// Evento Alteração de Contrato de Trabalho
        /// </summary>
        [XmlElement("evtAltContratual")]
        public EvtAltContratual EvtAltContratual { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtAltContratual")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Evento Alteração de Contrato de Trabalho
    /// </summary>
    public class EvtAltContratual
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
        public IdeEventoESocial2206 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador.
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações de identificação do trabalhador e do vínculo.
        /// </summary>
        [XmlElement("ideVinculo")]
        public IdeVinculo IdeVinculo { get; set; }

        /// <summary>
        /// Alteração de dados contratuais
        /// </summary>
        [XmlElement("altContratual")]
        public AltContratual AltContratual { get; set; }


        #region ShouldSerialize
        /// <summary>
        /// Verifica se a tag 'ideEvento' deve ser serializada
        /// </summary>
        //public bool ShouldSerializeIdeEvento() => IdeEvento != null;

        /// <summary>
        /// Verifica se a tag 'ideEmpregador' deve ser serializada
        /// </summary>
        //public bool ShouldSerializeIdeEmpregador() => IdeEmpregador != null;

        /// <summary>
        /// Verifica se a tag 'ideVinculo' deve ser serializada
        /// </summary>
        //public bool ShouldSerializeIdeVinculo() => IdeVinculo != null;

        /// <summary>
        /// Verifica se a tag 'altContratual' deve ser serializada
        /// </summary>
        //public bool ShouldSerializeAltContratual() => AltContratual != null;
        #endregion
    }

    #region IdeEvento

#if INTEROP
        [ClassInterface(ClassInterfaceType.AutoDual)]
        [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEventoESocial2206")]
        [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
    public class IdeEventoESocial2206
    {
        /// <summary>
        /// Informe [1] para arquivo original ou [2] para arquivo de
        /// retificação.
        /// Valores válidos:
        /// 1 - Original
        /// 2 - Retificação
        /// </summary>
        [XmlElement("indRetif")]
        public sbyte IndRetif { get; set; }

        /// <summary>
        /// Preencher com o número do recibo do arquivo a ser
        /// retificado.
        /// Validação: O preenchimento é obrigatório se indRetif = [2].
        /// Deve ser um recibo de entrega válido, correspondente ao
        /// arquivo que está sendo retificado.
        /// </summary>
        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

        /// <summary>
        /// Identificação do ambiente.
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; } // Enum: 1-Produção, 2-Produção restrita

        /// <summary>
        /// Processo de emissão do evento.
        /// Valores válidos:
        /// 1 - Aplicativo do empregador
        /// 2 - Aplicativo governamental - Simplificado Pessoa Física
        /// 3 - Aplicativo governamental - Web Geral
        /// 4 - Aplicativo governamental - Simplificado Pessoa Jurídica
        /// 22 - Aplicativo governamental para dispositivos móveis - Empregador Doméstico
        /// </summary>
        [XmlElement("procEmi")]
        public ProcessoEmissao ProcEmi { get; set; } // Enum: 1-Aplicativo do empregador, 2-Aplicativo governamental - Simplificado Pessoa Física, 3-Aplicativo governamental - Web Geral, 4-Aplicativo governamental - Simplificado Pessoa Jurídica, 22-Aplicativo governamental para dispositivos móveis - Empregador Doméstico

        /// <summary>
        /// Versão do processo de emissão do evento. Informar a versão do aplicativo emissor do evento.
        /// </summary>
        [XmlElement("verProc")]
        public string VerProc { get; set; }

        #region ShouldSerialize
        /// <summary>
        /// Verifica se a tag 'nrRecibo' deve ser serializada
        /// </summary>
        public bool ShouldSerializeNrReciboField() => IndRetif == 2;
        #endregion
    }

    #endregion IdeEvento

    #region IdeVinculo

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeVinculo")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de identificação do trabalhador e do vínculo.
    /// </summary>
    [Serializable()]
    public class IdeVinculo
    {
        /// <summary>
        /// Preencher com o número do CPF do trabalhador.
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        /// <summary>
        /// Matrícula atribuída ao trabalhador pela empresa ou, no caso de servidor público, a matrícula constante no Sistema de Administração de Recursos Humanos do órgão.
        /// </summary>
        [XmlElement("matricula")]
        public string Matricula { get; set; }
    }

    #endregion IdeVinculo

    #region altContratual

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.AltContratual")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Alteração de dados contratuais
    /// </summary>
    public class AltContratual
    {
#if INTEROP
        public DateTime DtAlteracao {get; set; }
#else
        /// <summary>
        /// Preencher com a data da alteração das informações.
        ///Validação: Não pode ser posterior a 180 (cento e oitenta) dias da data atual.
        /// </summary>
        [XmlIgnore]
        public DateTimeOffset DtAlteracao { get; set; }
#endif

        /// <summary>
        /// Preencher com a data da alteração das informações.
        ///Validação: Não pode ser posterior a 180 (cento e oitenta) dias da data atual.
        /// </summary>
        [XmlElement("dtAlteracao")]
        public string DtAlteracaoField
        {
            get => DtAlteracao.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAlteracao = DateTime.Parse(value);
#else
            set => DtAlteracao = DateTimeOffset.Parse(value);
#endif
        }

#if INTEROP
        public DateTime DtEf {get; set; }
#else
        /// <summary>
        /// Data dos efeitos remuneratórios da alteração contratual.
        /// Se a alteração foi fruto de lei, acordo coletivo, convenção
        /// coletiva ou sentença normativa, informar a data a partir da
        /// qual a alteração produz efeitos remuneratórios.
        /// Validação: Deve ser uma data válida, igual ou posterior à
        /// data de admissão.
        /// </summary>
        [XmlIgnore]
        public DateTimeOffset DtEf { get; set; }
#endif

        /// <summary>
        /// Data dos efeitos remuneratórios da alteração contratual.
        /// Se a alteração foi fruto de lei, acordo coletivo, convenção
        /// coletiva ou sentença normativa, informar a data a partir da
        /// qual a alteração produz efeitos remuneratórios.
        /// Validação: Deve ser uma data válida, igual ou posterior à
        /// data de admissão.
        /// </summary>
        [XmlElement("dtEf")]
        public string DtEfField
        {
            get => DtEf.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtEf = DateTime.Parse(value);
#else
            set => DtEf = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Descrição da alteração ou do instrumento que a gerou.
        /// </summary>
        [XmlElement("dscAlt")]
        public string DscAlt { get; set; }

        /// <summary>
        /// Grupo de informações do vínculo trabalhista.
        /// </summary>
        [XmlElement("vinculo")]
        public Vinculo Vinculo { get; set; }
    }

    #region Vinculo

    /// <summary>
    /// Grupo de informações do vínculo trabalhista.
    /// </summary>
    [Serializable()]
    public class Vinculo
    {
        /// <summary>
        /// Tipo de regime previdenciário (ou Sistema de Proteção
        /// Social dos Militares das Forças Armadas).
        /// </summary>
        [XmlElement("tpRegPrev")]
        public TpRegPrev TpRegPrev { get; set; }

        /// <summary>
        /// Informações do regime trabalhista 
        /// </summary>
        [XmlElement("infoRegimeTrab")]
        public InfoRegimeTrab InfoRegimeTrab { get; set; }

        /// <summary>
        /// Informações do contrato de trabalho.
        /// </summary>
        [XmlElement("infoContrato")]
        public InfoContrato InfoContrato { get; set; }
    }

    #region InfoRegimeTrab

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRegimeTrab")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações do regime trabalhista 
    /// </summary>
    [Serializable()]
    public class InfoRegimeTrab
    {
        /// <summary>
        /// Informações de trabalhador celetista
        /// </summary>
        [XmlElement("infoCeletista")]
        public InfoCeletista InfoCeletista { get; set; }

        /// <summary>
        /// Informações de trabalhador estatutário.
        /// </summary>
        [XmlElement("infoEstatutario")]
        public InfoEstatutario InfoEstatutario { get; set; }
    }

    #region InfoCeletista

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCeletista")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de trabalhador celetista
    /// </summary>
    [Serializable()]
    public class InfoCeletista
    {
        /// <summary>
        /// Regime de jornada do empregado.
        /// Valores válidos:
        /// 1 - Submetido a horário de trabalho(Capítulo II do Título II da CLT)
        /// 2 - Atividade externa especificada no inciso I do art. 62 da CLT
        /// 3 - Função especificada no inciso II do art. 62 da CLT
        /// 4 - Teletrabalho, previsto no inciso III do art. 62 da CLT
        /// </summary>
        [XmlElement("tpRegJor")]
        public TpRegJor TpRegJor { get; set; }

        /// <summary>
        /// Natureza da atividade.
        /// </summary>
        [XmlElement("natAtividade")]
        public NatAtividade NatAtividade { get; set; }

        /// <summary>
        /// Mês relativo à data base da categoria profissional do trabalhador.
        /// </summary>
        [XmlElement("dtBase")]
        public int DtBase { get; set; }

        /// <summary>
        /// Preencher com o CNPJ do sindicato representativo da
        /// categoria(preponderante ou diferenciada).
        /// Validação: Deve ser um CNPJ válido, com 14 (catorze) algarismos.
        /// </summary>
        [XmlElement("cnpjSindCategProf")]
        public string CnpjSindCategProf { get; set; }

        /// <summary>
        /// Dados sobre trabalho temporário. Preenchimento
        /// obrigatório no caso de prorrogação de contrato de trabalhador temporário.
        /// </summary>
        [XmlElement("trabTemporario")]
        public TrabTemporario TrabTemporario { get; set; }

        /// <summary>
        /// - Informações relacionadas ao aprendiz.
        /// </summary>
        [XmlElement("aprend")]
        public Aprend Aprend { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeDtBaseField() => DtBase > 0;

        public bool ShouldSerializeTrabTemporarioField() => TrabTemporario != null;

        #endregion ShouldSerialize
    }

    #region TrabTemporario
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TrabTemporario")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Dados sobre trabalho temporário. Preenchimento
    /// obrigatório no caso de prorrogação de contrato de trabalhador temporário.
    /// </summary>
    public class TrabTemporario
    {
        /// <summary>
        /// Descrever a justificativa para a prorrogação do contrato de trabalho temporário.
        /// </summary>
        [XmlElement("justProrr")]
        public string JustProrr { get; set; }
    }
    #endregion TrabTemporario

    #region Aprendiz
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Aprend")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// - Informações relacionadas ao aprendiz.
    /// </summary>
    public class Aprend
    {
        /// <summary>
        /// Indicativo de modalidade de contratação de aprendiz.
        /// Valores válidos:
        /// 1 - Contratação direta: contratação do aprendiz efetivada
        /// pelo estabelecimento cumpridor da cota de aprendizagem
        /// 2 - Contratação indireta: contratação do aprendiz
        /// efetivada por entidades sem fins lucrativos ou por
        /// entidades de prática desportiva a serviço do
        /// estabelecimento cumpridor da cota
        /// </summary>
        [XmlElement("indAprend")]
        public IndAprend IndAprend { get; set; }

        /// <summary>
        /// Informar o número de inscrição no CNPJ da entidade
        /// qualificadora, no caso de contratação direta.
        /// Validação: Preenchimento obrigatório e exclusivo se
        /// indAprend = [1].
        /// Deve ser um CNPJ válido, com 14 (catorze) algarismos.
        /// </summary>
        [XmlElement("cnpjEntQual")]
        public string CnpjEntQual { get; set; }

        /// <summary>
        /// Preencher com o código correspondente ao tipo de
        /// inscrição do estabelecimento para o qual a contratação de
        /// aprendiz foi efetivada, no caso de contratação indireta,
        /// conforme Tabela 05.
        /// Valores válidos:
        /// 1 - CNPJ
        /// 2 - CPF
        /// Validação: Preenchimento obrigatório e exclusivo se
        /// indAprend = [2].
        /// </summary>
        [XmlElement("tpInsc")]
        public TpInsc TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do estabelecimento para
        /// o qual a contratação de aprendiz foi efetivada, no caso de
        /// contratação indireta, de acordo com o tipo de inscrição
        /// indicado no campo aprend/tpInsc.
        /// Validação: Preenchimento obrigatório e exclusivo se
        /// indAprend = [2].
        /// Deve ser um identificador válido e:
        /// a) Se aprend/tpInsc = [1], deve ser informado com 14
        /// (catorze) algarismos.Se o empregador for pessoa jurídica,
        /// a raiz do CNPJ informado deve ser diferente de
        /// ideEmpregador/nrInsc.
        /// b) Se aprend/tpInsc = [2], deve ser diferente do CPF do
        /// empregado.Se o empregador for pessoa física, também
        /// deve ser diferente do CPF do empregador.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição no CNPJ do
        /// estabelecimento onde estão sendo realizadas as
        /// atividades práticas, quando ocorrer uma das seguintes
        /// situações:
        /// a) Modalidade alternativa de cumprimento de cota de
        /// aprendizagem(neste caso, informar o CNPJ da entidade
        /// concedente da parte prática);
        /// b) Realização das atividades práticas na empresa
        /// contratante do serviço terceirizado;
        /// c) Centralização das atividades práticas em
        /// estabelecimento da própria empresa, diverso do
        /// estabelecimento responsável pelo cumprimento da cota.
        /// Validação: Deve ser um CNPJ válido, com 14 (catorze) algarismos.
        /// </summary>
        [XmlElement("cnpjPrat")]
        public string CnpjPrat { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeCnpjEntQualField() => !string.IsNullOrEmpty(CnpjEntQual);
        public bool ShouldSerializeTpInscField() => TpInsc.IsNullOrEmpty();
        public bool ShouldSerializeNrInscField() => !string.IsNullOrEmpty(NrInsc);

        public bool ShouldSerializeCnpjPratField() => !string.IsNullOrEmpty(CnpjPrat);
        #endregion ShouldSerialize

    }
    #endregion Aprendiz

    #endregion InfoCeletista

    #region InfoEstatutario

    /// <summary>
    /// Informações de trabalhador estatutário.
    /// </summary>
    public class InfoEstatutario
    {
        /// <summary>
        /// Tipo de plano de segregação da massa.
        /// Valores válidos:
        /// 0 - Sem segregação da massa
        /// 1 - Fundo em capitalização
        /// 2 - Fundo em repartição
        /// 3 - Mantido pelo Tesouro
        /// </summary>
        [XmlElement("tpPlanRP")]
        public TpPlanRP TpPlanRP { get; set; }

        /// <summary>
        /// Informar se o servidor está sujeito ao teto do RGPS pela
        /// instituição do regime de previdência complementar.
        /// Valores válidos:
        /// S - Sim
        /// N - Não
        /// </summary>
        [XmlElement("indTetoRGPS")]
        public SimNaoLetra IndTetoRGPS { get; set; }

        /// <summary>
        /// Indicar se o servidor recebe abono permanência.
        /// Valores válidos:
        /// S - Sim
        /// N - Não
        /// </summary>
        [XmlElement("indAbonoPerm")]
        public SimNaoLetra IndAbonoPerm { get; set; }
    }
    #endregion InfoEstatutario

    #endregion InfoRegimeTrab

    #region InfoContrato
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoContrato")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações do contrato de trabalho.
    /// </summary>
    public class InfoContrato
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

        /// <summary>
        /// Informar se o cargo, emprego ou função pública é acumulável.
        /// Valores válidos:
        /// S - Sim
        /// N - Não
        /// Validação: Preenchimento obrigatório se a natureza
        /// jurídica do declarante for igual a 1XX-X, 201-1 ou 203-8.
        /// </summary>
        [XmlElement("acumCargo")]
        public SimNaoLetra AcumCargo { get; set; }

        /// <summary>
        /// Preencher com o código da categoria do trabalhador.
        /// </summary>
        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        /// <summary>
        /// Informações da remuneração e periodicidade de pagamento.
        /// </summary>
        [XmlElement("remuneracao")]
        public Remuneracao Remuneracao { get; set; }

        /// <summary>
        /// Duração do contrato de trabalho.
        /// </summary>
        [XmlElement("duracao")]
        public Duracao Duracao { get; set; }


        /// <summary>
        /// Informações do local de trabalho.
        /// </summary>
        [XmlElement("localTrabalho")]
        public LocalTrabalho LocalTrabalho { get; set; }

        /// <summary>
        /// Informações do horário contratual do trabalhador.
        /// </summary>
        [XmlElement("horContratual")]
        public HorContratual HorContratual { get; set; }

        /// <summary>
        /// Informações do alvará judicial em caso de contratação de
        /// menores de 14 anos, em qualquer categoria, e de maiores
        /// de 14 e menores de 16, em categoria diferente de "Aprendiz".
        /// </summary>
        [XmlElement("alvaraJudicial")]
        public AlvaraJudicial AlvaraJudicial { get; set; }

        /// <summary>
        /// Observações do contrato de trabalho.
        /// </summary>
        [XmlElement("observacoes")]
        public Observacoes Observacoes { get; set; }

        /// <summary>
        /// Treinamentos, capacitações, exercícios simulados,
        /// autorizações ou outras anotações que devam ser anotadas
        /// no registro de empregados e/ou na CTPS, por
        /// determinação de Norma Regulamentadora - NR.
        /// </summary>
        [XmlElement("treiCap")]
        public TreiCap TreiCap { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeNmCargoField() => !string.IsNullOrEmpty(NmCargo);
        public bool ShouldSerializeCBOCargoField() => !string.IsNullOrEmpty(CBOCargo);
        public bool ShouldSerializeNmFuncaoField() => !string.IsNullOrEmpty(NmFuncao);
        public bool ShouldSerializeCBOFuncaoField() => !string.IsNullOrEmpty(CBOFuncao);
        public bool ShouldSerializeAcumCargoField() => AcumCargo.IsNullOrEmpty();
        public bool ShouldSerializeDuracaoField() => Duracao.IsNullOrEmpty();
        public bool ShouldSerializeRemuneracaoField() => Remuneracao.IsNullOrEmpty();
        public bool ShouldSerializeHorContratualField() => HorContratual.IsNullOrEmpty();
        public bool ShouldSerializeAlvaraJudicialField() => AlvaraJudicial.IsNullOrEmpty();
        public bool ShouldSerializeObservacoesField() => Observacoes.IsNullOrEmpty();
        public bool ShouldSerializeTreiCapField() => TreiCap.IsNullOrEmpty();

        #endregion ShouldSerialize
    }

    #region Remuneracao

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Remuneracao")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações da remuneração e periodicidade de pagamento.
    /// </summary>
    public class Remuneracao
    {
        /// <summary>
        /// Salário base do trabalhador, correspondente à parte fixa
        /// da remuneração.
        /// Validação: Se undSalFixo for igual a [7], preencher com 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrSalFx { get; set; }

        /// <summary>
        /// Salário base do trabalhador, correspondente à parte fixa
        /// da remuneração.
        /// Validação: Se undSalFixo for igual a [7], preencher com 0 (zero).
        /// </summary>
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
        public UnidadeSalarioFixo UnidadeSalarioFixo { get; set; }

        /// <summary>
        /// Descrição do salário por tarefa ou variável e como este é
        /// calculado.Ex.: Comissões pagas no percentual de 10%
        /// sobre as vendas.
        /// Validação: Preenchimento obrigatório se undSalFixo for igual a[6, 7].
        /// </summary>
        [XmlElement("dscSalVar")]
        public string DscSalVar { get; set; }
        #region ShouldSerialize
        public bool ShouldSerializeDscSalVarField() => !string.IsNullOrEmpty(DscSalVar);
        #endregion ShouldSerialize
    }
    #endregion Remuneracao

    #region Duracao

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Duracao")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Duração do contrato de trabalho.
    /// </summary>
    public class Duracao
    {
        /// <summary>
        /// Tipo de contrato de trabalho.
        /// Valores válidos:
        /// 1 - Prazo indeterminado
        /// 2 - Prazo determinado, definido em dias
        /// 3 - Prazo determinado, vinculado à ocorrência de um fato
        /// Validação: Se codCateg = [103] e dtAlteracao >= [2024-
        /// 04-22], deve ser informado[2].
        /// </summary>
        [XmlElement("tpContr")]
        public TipoDeContratoDeTrabalho TipoDeContratoDeTrabalho { get; set; }

#if INTEROP
        public DateTime DtTerm {get; set; }
#else
        /// <summary>
        /// Data do término do contrato por prazo determinado.
        /// Validação: O preenchimento é obrigatório se tpContr =
        /// [2].Não informar se tpContr = [1]. Se preenchido, deve ser
        /// igual ou posterior à data de admissão(no caso de
        /// transferência ou mudança de CPF, igual ou posterior a
        /// sucessaoVinc/dtTransf, transfDom/dtTransf ou dtAltCPF do
        /// evento S-2200, conforme o caso).
        /// Retornar alerta caso a data informada seja anterior a
        /// dtAlteracao.
        /// </summary>
        [XmlIgnore]
        public DateTimeOffset DtTerm { get; set; }
#endif

        /// <summary>
        /// Data do término do contrato por prazo determinado.
        /// Validação: O preenchimento é obrigatório se tpContr =
        /// [2].Não informar se tpContr = [1]. Se preenchido, deve ser
        /// igual ou posterior à data de admissão(no caso de
        /// transferência ou mudança de CPF, igual ou posterior a
        /// sucessaoVinc/dtTransf, transfDom/dtTransf ou dtAltCPF do
        /// evento S-2200, conforme o caso).
        /// Retornar alerta caso a data informada seja anterior a
        /// dtAlteracao.
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

        /// <summary>
        /// Indicação do objeto determinante da contratação por
        /// prazo determinado(obra, serviço, safra, etc.).
        /// Validação: O preenchimento é obrigatório e exclusivo se
        /// tpContr = [3].
        /// </summary>
        [XmlElement("objDet")]
        public string ObjDet { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeDtTermField () => !string.IsNullOrEmpty(DtTermField);
        public bool ShouldSerializeObjDetField() => TipoDeContratoDeTrabalho == TipoDeContratoDeTrabalho.PrazoDeterminadoOcorrencia;

        #endregion ShouldSerialize
    }
    #endregion Duracao

    #region LocalTrabalho

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalTrabalho")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações do local de trabalho.
    /// </summary>
    public class LocalTrabalho
    {
        /// <summary>
        ///  Estabelecimento (CNPJ, CNO, CAEPF) onde o trabalhador
        /// (exceto doméstico) exercerá suas atividades.Caso o
        /// trabalhador exerça suas atividades em instalações de
        /// terceiros, este campo deve ser preenchido com o
        /// estabelecimento do próprio empregador ao qual o
        /// trabalhador esteja vinculado.
        /// </summary>
        [XmlElement("localTrabGeral")]
        public LocalTrabGeral LocalTrabGeral { get; set; }

        /// <summary>
        /// Grupo preenchido exclusivamente em caso de trabalhador
        /// doméstico e trabalhador temporário, indicando o
        /// endereço onde o trabalhador exerce suas atividades.
        /// </summary>
        [XmlElement("localTempDom")]
        public LocalTempDom LocalTempDom { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeLocalTrabGeralField() => LocalTrabGeral.IsNullOrEmpty();
        public bool ShouldSerializeLocalTempDomField() => LocalTempDom.IsNullOrEmpty();


        #endregion ShouldSerialize
    }

    #region LocalTrabGeral

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalTrabGeral")]
    [ComVisible(true)]
#endif
    /// <summary>
    ///  Estabelecimento (CNPJ, CNO, CAEPF) onde o trabalhador
    /// (exceto doméstico) exercerá suas atividades.Caso o
    /// trabalhador exerça suas atividades em instalações de
    /// terceiros, este campo deve ser preenchido com o
    /// estabelecimento do próprio empregador ao qual o
    /// trabalhador esteja vinculado.
    /// </summary>
    public class LocalTrabGeral
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 05.
        /// </summary>
        [XmlElement("tpInsc")]
        public TpInsc TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do contribuinte de acordo
        /// com o tipo de inscrição indicado no campo
        /// localTrabGeral/tpInsc.
        /// Validação: Deve ser um número de inscrição válido e
        /// existente na Tabela de Estabelecimentos(S-1005), bem
        /// como compatível com localTrabGeral/tpInsc
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Descrição complementar do local de trabalho.
        /// </summary>
        [XmlElement("descComp")]
        public string DescComp { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeDescCompField() => !string.IsNullOrEmpty(DescComp);
        #endregion ShouldSerialize
    }

    #endregion LocalTrabGeral

    #region LocalTempDom

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalTempDom")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Grupo preenchido exclusivamente em caso de trabalhador
    /// doméstico e trabalhador temporário, indicando o
    /// endereço onde o trabalhador exerce suas atividades.
    /// </summary>
    public class LocalTempDom
    {
        /// <summary>
        /// Tipo de logradouro.
        /// Validação: Se informado, deve ser um código válido e
        /// existente na Tabela 20.
        /// </summary>
        [XmlElement("tpLograd")]
        public TipoLogradouro TpLograd { get; set; }

        /// <summary>
        /// Descrição do logradouro.
        /// </summary>
        [XmlElement("dscLograd")]
        public string DscLograd { get; set; }

        /// <summary>
        /// Número do logradouro.
        /// Se não houver número a ser informado, preencher com "S/N".
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
        /// Código de Endereçamento Postal - CEP.
        /// Validação: Deve ser preenchido apenas com números,
        /// com 8 (oito) posições.
        /// </summary>]
        [XmlElement("cep")]
        public string Cep { get; set; }

        /// <summary>
        /// Preencher com o código do município, conforme tabela
        /// do IBGE.
        /// Validação: Deve ser um código válido e existente na
        /// tabela do IBGE.
        /// </summary>
        [XmlElement("codMunic")]
        public string CodMunicipio { get; set; }

        /// <summary>
        /// Preencher com a sigla da Unidade da Federação - UF.
        /// Valores válidos: AC, AL, AP, AM, BA, CE, DF, ES, GO, MA,
        /// MT, MS, MG, PA, PB, PR, PE, PI, RJ, RN, RS, RO, RR, SC, SP, SE, TO
        /// /summary>
        [XmlElement("uf")]
        public UFBrasil Uf { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeTpLogradField() => TpLograd.IsNullOrEmpty();
        public bool ShouldSerializeComplementoField() => !string.IsNullOrEmpty(Complemento);
        public bool ShouldSerializeBairroField() => !string.IsNullOrEmpty(Bairro);


        #endregion ShouldSerialize
    }
    #endregion LocalTempDom

    #endregion LocalTrabalho

    #region HorContratual

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.HorContratual")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações do horário contratual do trabalhador.
    /// </summary>
    public class HorContratual
    {
        /// <summary>
        /// Quantidade média de horas relativas à jornada semanal do trabalhador.
        /// Validação: Deve ser preenchida se codCateg for diferente
        /// de[111]. Se informada, deve ser maior que 0 (zero).
        /// </summary>
        [XmlElement("qtdHrsSem")]
        public string QtdHrsSem { get; set; }

        /// <summary>
        /// Tipo de jornada
        /// Valores válidos:
        /// 2 - Jornada 12 x 36 (12 horas de trabalho seguidas de 36
        /// horas ininterruptas de descanso)
        /// 3 - Jornada com horário diário fixo e folga variável
        /// 4 - Jornada com horário diário fixo e folga fixa(no
        /// domingo)
        /// 5 - Jornada com horário diário fixo e folga fixa(exceto no
        /// domingo)
        /// 6 - Jornada com horário diário fixo e folga fixa(em outro
        /// dia da semana), com folga adicional periódica no
        /// domingo
        /// 7 - Turno ininterrupto de revezamento
        /// 9 - Demais tipos de jornada
        /// </summary>
        [XmlElement("tpJornada")]
        public TpJornada TpJornada { get; set; }

        /// <summary>
        /// Preencher com o código relativo ao tipo de contrato em
        /// tempo parcial.
        /// Valores válidos:
        /// 0 - Não é contrato em tempo parcial
        /// 1 - Limitado a 25 horas semanais
        /// 2 - Limitado a 30 horas semanais
        /// 3 - Limitado a 26 horas semanais
        /// Validação: O código [1] só é válido se codCateg = [104].
        /// Os códigos [2, 3] não são válidos se codCateg = [104].
        /// </summary>
        [XmlElement("tmpParc")]
        public TmpParc TmpParc { get; set; }

        /// <summary>
        ///  Indicar se a jornada semanal possui horário noturno (no
        /// todo ou em parte).
        /// Valores válidos:
        /// S - Sim
        /// N - Não
        /// Validação: Informação obrigatória se codCateg for
        /// diferente de[111].
        /// </summary>
        [XmlElement("horNoturno")]
        public SimNaoLetra HorNoturno { get; set; }

        /// <summary>
        /// Descrição da jornada semanal contratual, contendo os
        /// dias da semana e os respectivos horários contratuais
        /// (entrada, saída e intervalos).
        /// </summary>
        [XmlElement("dscJorn")]
        public string DscJorn { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeQtdHrsSemField () => !string.IsNullOrEmpty(QtdHrsSem);
        public bool ShouldSerializeHorNoturnoField() => HorNoturno.IsNullOrEmpty();

        #endregion ShouldSerialize
    }

    #endregion HorContratual

    #region AlvaraJudicial

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.AlvaraJudicial")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações do alvará judicial em caso de contratação de
    /// menores de 14 anos, em qualquer categoria, e de maiores
    /// de 14 e menores de 16, em categoria diferente de "Aprendiz".
    /// </summary>
    public class AlvaraJudicial
    {
        /// <summary>
        /// Preencher com o número do processo judicial.
        /// Validação: Deve ser um número de processo judicial  válido.
        /// </summary>
        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }

    }
    #endregion AlvaraJudicial

    #region Observações
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Observacoes")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Observações do contrato de trabalho.
    /// </summary>
    public class Observacoes
    {
        /// <summary>
        /// Observação relacionada ao contrato de trabalho
        /// </summary>
        [XmlElement("observacao")]
        public string Observacao { get; set; }
    }
    #endregion Observações

    #region TreiCap
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TreiCap")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Treinamentos, capacitações, exercícios simulados,
    /// autorizações ou outras anotações que devam ser anotadas
    /// no registro de empregados e/ou na CTPS, por
    /// determinação de Norma Regulamentadora - NR.
    /// </summary>
    public class TreiCap
    {
        /// <summary>
        /// Informar o código do treinamento, capacitação, exercício
        /// simulado ou outra anotação, conforme Tabela 28.
        /// Validação: Deve ser um código válido e existente na Tabela 28.
        /// </summary>
        [XmlElement("codTreiCap")]
        public CodTreiCap CodTreiCap { get; set; }
    }
    #endregion TreiCap

    #endregion InfoContrato

    #endregion Vinculo

    #endregion altContratual
}