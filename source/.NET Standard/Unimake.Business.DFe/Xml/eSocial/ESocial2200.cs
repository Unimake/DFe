#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2200 - Cadastramento Inicial do Vínculo e Admissão/Ingresso de Trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2200")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAdmissao/v_S_01_02_00", IsNullable = false)]
    public class ESocial2200 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Cadastramento Inicial do Vínculo e Admissão/Ingresso de Trabalhador
        /// </summary>
        [XmlElement("evtAdmissao")]
        public EvtAdmissao EvtAdmissao { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Cadastramento Inicial do Vínculo e Admissão/Ingresso de Trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtAdmissao")]
    [ComVisible(true)]
#endif
    public class EvtAdmissao
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
        public IdeEvento2200 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações pessoais do trabalhador
        /// </summary>
        [XmlElement("trabalhador")]
        public Trabalhador2200 Trabalhador { get; set; }

        /// <summary>
        /// Informações do vínculo
        /// </summary>
        [XmlElement("vinculo")]
        public Vinculo2200 Vinculo { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2200")]
    [ComVisible(true)]
#endif
    public class IdeEvento2200 : IdeEvento2190 { }

    /// <summary>
    /// Informações pessoais do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Trabalhador2200")]
    [ComVisible(true)]
#endif
    public class Trabalhador2200
    {
        /// <summary>
        /// Preencher com o número do CPF do trabalhador
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        /// <summary>
        /// Informar o nome do trabalhador
        /// </summary>
        [XmlElement("nmTrab")]
        public string NmTrab { get; set; }

        /// <summary>
        /// Sexo do trabalhador
        /// </summary>
        [XmlElement("sexo")]
        public TipoSexo Sexo { get; set; }

        /// <summary>
        /// Etnia e raça do trabalhador, conforme sua autoclassificação
        /// </summary>
        [XmlElement("racaCor")]
        public RacaCor RacaCor { get; set; }

        /// <summary>
        /// Estado civil do trabalhador
        /// </summary>
        [XmlElement("estCiv")]
#if INTEROP
        public EstadoCivil EstCiv { get; set; } = (EstadoCivil)(-1);
#else
        public EstadoCivil? EstCiv { get; set; }
#endif

        /// <summary>
        /// Grau de instrução do trabalhador
        /// </summary>
        [XmlElement("grauInstr")]
        public GrauDeInstrucao GrauInstr { get; set; }

        /// <summary>
        /// Nome social para travesti ou transexual
        /// </summary>
        [XmlElement("nmSoc")]
        public string NmSoc { get; set; }

        /// <summary>
        /// Grupo de informações do nascimento do trabalhador
        /// </summary>
        [XmlElement("nascimento")]
        public Nascimento Nascimento { get; set; }

        /// <summary>
        /// Endereço do trabalhador
        /// </summary>
        [XmlElement("endereco")]
        public Endereco2200 Endereco { get; set; }

        /// <summary>
        /// Informações do trabalhador imigrante
        /// </summary>
        [XmlElement("trabImig")]
        public TrabImig2200 TrabImig { get; set; }

        /// <summary>
        /// Pessoa com deficiência
        /// </summary>
        [XmlElement("infoDeficiencia")]
        public InfoDeficiencia2200 InfoDeficiencia { get; set; }

        /// <summary>
        /// Informações dos dependentes
        /// </summary>
        [XmlElement("dependente")]
        public List<Dependente2200> Dependente { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDependente(Dependente2200 item)
        {
            if (Dependente == null)
            {
                Dependente = new List<Dependente2200>();
            }

            Dependente.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Dependente2200 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Dependente</returns>
        public Dependente2200 GetDependente(int index)
        {
            if ((Dependente?.Count ?? 0) == 0)
            {
                return default;
            };

            return Dependente[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Dependente
        /// </summary>
        public int GetDependenteCount => (Dependente != null ? Dependente.Count : 0);
#endif

        /// <summary>
        /// Informações de contato
        /// </summary>
        [XmlElement("contato")]
        public Contato Contato { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeInfoMesmoMtvEstCiv() => EstCiv != (EstadoCivil)(-1);
#else
        public bool ShouldSerializeInfoMesmoMtvEstCiv() => EstCiv != null;
#endif
        public bool ShouldSerializeNmSoc() => !string.IsNullOrEmpty(NmSoc);

        #endregion
    }

    /// <summary>
    /// Grupo de informações do nascimento do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Nascimento")]
    [ComVisible(true)]
#endif
    public class Nascimento
    {
        /// <summary>
        /// Preencher com a data de nascimento
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
        /// Preencher com o código do país de nascimento do trabalhador
        /// </summary>
        [XmlElement("paisNascto")]
        public string PaisNascto { get; set; }

        /// <summary>
        /// Preencher com o código do país de nacionalidade do trabalhador
        /// </summary>
        [XmlElement("paisNac")]
        public string PaisNac { get; set; }
    }

    /// <summary>
    /// Grupo de informações do endereço do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Endereco2200")]
    [ComVisible(true)]
#endif
    public class Endereco2200 : Endereco2205 { }

    /// <summary>
    /// Informações do trabalhador imigrante
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TrabImig2200")]
    [ComVisible(true)]
#endif
    public class TrabImig2200 : TrabImig2205 { }

    /// <summary>
    /// Pessoa com deficiência
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoDeficiencia2200")]
    [ComVisible(true)]
#endif
    public class InfoDeficiencia2200 : InfoDeficiencia2205 { }

    /// <summary>
    /// Informações dos dependentes
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Dependente2200")]
    [ComVisible(true)]
#endif
    public class Dependente2200 : Dependente2205 { }

    /// <summary>
    /// Informações de contato
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Contato")]
    [ComVisible(true)]
#endif
    public class Contato
    {
        /// <summary>
        /// Número de telefone do trabalhador, com DDD
        /// </summary>
        [XmlElement("fonePrinc")]
        public string FonePrinc { get; set; }

        /// <summary>
        /// Endereço eletrônico
        /// </summary>
        [XmlElement("emailPrinc")]
        public string EmailPrinc { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFonePrinc() => FonePrinc.HasOnlyNumbers() && FonePrinc.Length >= 8;

        public bool ShouldSerializeEmailPrinc() => !string.IsNullOrEmpty(EmailPrinc) &&
                                                         EmailPrinc.Contains("@") &&
                                                         EmailPrinc.Contains(".") &&
                                                        !EmailPrinc.StartsWith("@") &&
                                                        !EmailPrinc.EndsWith("@") &&
                                                        !EmailPrinc.StartsWith(".") &&
                                                        !EmailPrinc.EndsWith(".");

        #endregion
    }

    /// <summary>
    /// Grupo de informações do vínculo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Vinculo2200")]
    [ComVisible(true)]
#endif
    public class Vinculo2200
    {
        /// <summary>
        /// Matrícula atribuída ao trabalhador pela empresa ou, no caso de servidor público
        /// </summary>
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        /// <summary>
        /// Tipo de regime trabalhista
        /// </summary>
        [XmlElement("tpRegTrab")]
        public TipoRegimeTrabalhista TpRegTrab { get; set; }

        /// <summary>
        /// Tipo de regime previdenciário
        /// </summary>
        [XmlElement("tpRegPrev")]
        public TipoRegimePrevidenciario TpRegPrev { get; set; }

        /// <summary>
        /// Indicar se o evento se refere a cadastramento inicial de vínculo
        /// </summary>
        [XmlElement("cadIni")]
        public SimNaoLetra CadIni { get; set; }

        /// <summary>
        /// Informações do regime trabalhista
        /// </summary>
        [XmlElement("infoRegimeTrab")]
        public InfoRegimeTrab2200 InfoRegimeTrab { get; set; }

        /// <summary>
        /// Informações do contrato de trabalho
        /// </summary>
        [XmlElement("infoContrato")]
        public InfoContrato2200 InfoContrato { get; set; }

        /// <summary>
        /// Grupo de informações da sucessão de vínculo trabalhista/estatutário
        /// </summary>
        [XmlElement("sucessaoVinc")]
        public SucessaoVinc2200 SucessaoVinc { get; set; }

        /// <summary>
        /// Informações do empregado doméstico transferido de outro representante da mesma unidade familiar
        /// </summary>
        [XmlElement("transfDom")]
        public TransfDom TransfDom { get; set; }

        /// <summary>
        /// Informações de mudança de CPF do trabalhador
        /// </summary>
        [XmlElement("mudancaCPF")]
        public MudancaCPF2200 MudancaCPF { get; set; }

        /// <summary>
        /// Informações de afastamento do trabalhador
        /// </summary>
        [XmlElement("afastamento")]
        public Afastamento2200 Afastamento { get; set; }

        /// <summary>
        /// Informação do desligamento do trabalhador
        /// </summary>
        [XmlElement("desligamento")]
        public Desligamento Desligamento { get; set; }

        /// <summary>
        /// Informação de cessão/exercício em outro órgão do trabalhador
        /// </summary>
        [XmlElement("cessao")]
        public Cessao Cessao { get; set; }
    }

    /// <summary>
    /// Informações do regime trabalhista
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRegimeTrab2200")]
    [ComVisible(true)]
#endif
    public class InfoRegimeTrab2200
    {
        /// <summary>
        /// Informações de trabalhador celetista
        /// </summary>
        [XmlElement("infoCeletista")]
        public InfoCeletista2200 InfoCeletista { get; set; }

        /// <summary>
        /// Informações de trabalhador estatutário
        /// </summary>
        [XmlElement("infoEstatutario")]
        public InfoEstatutario2200 InfoEstatutario { get; set; }
    }

    /// <summary>
    /// Informações de trabalhador celetista
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCeletista2200")]
    [ComVisible(true)]
#endif
    public class InfoCeletista2200
    {
        /// <summary>
        /// Preencher com a data de admissão do trabalhador
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
        /// Tipo de admissão do trabalhador
        /// </summary>
        [XmlElement("tpAdmissao")]
        public TipoAdmissaoTrabalhador TpAdmissao { get; set; }

        /// <summary>
        /// Indicativo de admissão
        /// </summary>
        [XmlElement("indAdmissao")]
        public IndicativoDeAdmissao IndAdmissao { get; set; }

        /// <summary>
        /// Número que identifica o processo trabalhista, quando a admissão se der por decisão judicial
        /// </summary>
        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }

        /// <summary>
        /// Regime de jornada do empregado
        /// </summary>
        [XmlElement("tpRegJor")]
        public RegimeJornadaEmpregado TpRegJor { get; set; }

        /// <summary>
        /// Natureza da atividade
        /// </summary>
        [XmlElement("natAtividade")]
        public NatAtividade NatAtividade { get; set; }

        /// <summary>
        /// Mês relativo à data base da categoria profissional do trabalhador
        /// </summary>
        [XmlElement("dtBase")]
        public int DtBase { get; set; }

        /// <summary>
        /// Preencher com o CNPJ do sindicato representativo da categoria (preponderante ou diferenciada)
        /// </summary>
        [XmlElement("cnpjSindCategProf")]
        public string CnpjSindCategProf { get; set; }

        /// <summary>
        /// Matrícula informada no evento S-8200
        /// </summary>
        [XmlElement("matAnotJud")]
        public string MatAnotJud { get; set; }

        /// <summary>
        /// Informações do Fundo de Garantia do Tempo de Serviço - FGTS
        /// </summary>
        [XmlElement("FGTS")]
        public FGTS2200 FGTS { get; set; }

        /// <summary>
        /// Dados sobre trabalho temporário. Preenchimento obrigatório no caso de contratação de trabalhador temporário
        /// </summary>
        [XmlElement("trabTemporario")]
        public TrabTemporario2200 TrabTemporario { get; set; }

        /// <summary>
        /// Informações relacionadas ao aprendiz
        /// </summary>
        [XmlElement("aprend")]
        public Aprend2200 Aprend { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNrProcTrab() => !string.IsNullOrEmpty(NrProcTrab);

        public bool ShouldSerializeDtBase() => DtBase > 0;

        public bool ShouldSerializeMatAnotJud() => !string.IsNullOrEmpty(MatAnotJud);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações do Fundo de Garantia do Tempo de Serviço - FGTS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.FGTS2200")]
    [ComVisible(true)]
#endif
    public class FGTS2200
    {
        /// <summary>
        /// Informar a data de opção do trabalhador pelo FGTS
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtOpcFGTS { get; set; }
#else
        public DateTimeOffset DtOpcFGTS { get; set; }
#endif

        [XmlElement("dtOpcFGTS")]
        public string DtOpcFGTSField
        {
            get => DtOpcFGTS.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtOpcFGTS = DateTime.Parse(value);
#else
            set => DtOpcFGTS = DateTimeOffset.Parse(value);
#endif
        }
    }

    /// <summary>
    /// Dados sobre trabalho temporário. Preenchimento obrigatório no caso de contratação de trabalhador temporário
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TrabTemporario2200")]
    [ComVisible(true)]
#endif
    public class TrabTemporario2200
    {
        /// <summary>
        /// Hipótese legal para contratação de trabalhador temporário
        /// </summary>
        [XmlElement("hipLeg")]
        public ContratacaoTrabalhadorTemporario HipLeg { get; set; }

        /// <summary>
        /// Descrição do fato determinado que, no caso concreto, justifica a hipótese legal para a contratação de trabalho temporário.
        /// </summary>
        [XmlElement("justContr")]
        public string JustContr { get; set; }

        /// <summary>
        /// Identificação do estabelecimento do tomador ao qual o trabalhador temporário está vinculado
        /// </summary>
        [XmlElement("ideEstabVinc")]
        public IdeEstabVinc IdeEstabVinc { get; set; }

        /// <summary>
        /// Identificação do(s) trabalhador(es) substituído(s)
        /// </summary>
        [XmlElement("ideTrabSubstituido")]
        public List<IdeTrabSubstituido> IdeTrabSubstituido { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeTrabSubstituido(IdeTrabSubstituido item)
        {
            if (IdeTrabSubstituido == null)
            {
                IdeTrabSubstituido = new List<IdeTrabSubstituido>();
            }

            IdeTrabSubstituido.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeTrabSubstituido (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeTrabSubstituido</returns>
        public IdeTrabSubstituido GetIdeTrabSubstituido(int index)
        {
            if ((IdeTrabSubstituido?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeTrabSubstituido[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeTrabSubstituido
        /// </summary>
        public int GetIdeTrabSubstituidoCount => (IdeTrabSubstituido != null ? IdeTrabSubstituido.Count : 0);
#endif
    }

    /// <summary>
    /// Identificação do estabelecimento do tomador ao qual o trabalhador temporário está vinculado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabVinc")]
    [ComVisible(true)]
#endif
    public class IdeEstabVinc
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 05
        /// </summary>
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do contratante de serviços, de acordo com o tipo de inscrição informado em ideEstabVinc/tpInsc
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

    /// <summary>
    /// Identificação do(s) trabalhador(es) substituído(s)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabSubstituido")]
    [ComVisible(true)]
#endif
    public class IdeTrabSubstituido
    {
        /// <summary>
        /// CPF do trabalhador substituído
        /// </summary>
        [XmlElement("cpfTrabSubst")]
        public string CpfTrabSubst { get; set; }
    }

    /// <summary>
    /// Informações relacionadas ao aprendiz
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Aprend2200")]
    [ComVisible(true)]
#endif
    public class Aprend2200
    {
        /// <summary>
        /// Indicativo de modalidade de contratação de aprendiz
        /// </summary>
        [XmlElement("indAprend")]
        public IndicativoContratacaoAprendiz IndAprend { get; set; }

        /// <summary>
        /// Informar o número de inscrição no CNPJ da entidade qualificadora, no caso de contratação direta
        /// </summary>
        [XmlElement("cnpjEntQual")]
        public string CnpjEntQual { get; set; }

        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição do estabelecimento para o qual a contratação de aprendiz foi efetivada, 
        /// no caso de contratação indireta, conforme Tabela 05
        /// </summary>
        [XmlElement("tpInsc")]
#if INTEROP
        public TiposInscricao TpInsc { get; set; } = (TiposInscricao)(-1);
#else
        public TiposInscricao? TpInsc { get; set; }
#endif

        /// <summary>
        /// Informar o número de inscrição do estabelecimento para o qual a contratação de aprendiz foi efetivada, no caso de contratação indireta
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição no CNPJ do estabelecimento onde estão sendo realizadas as atividades práticas,
        /// quando ocorrer uma das seguintes situações:
        /// a) Modalidade alternativa de cumprimento de cota de aprendizagem(neste caso, informar o CNPJ da entidade concedente da parte prática);
        /// b) Realização das atividades práticas na empresa contratante do serviço terceirizado;
        /// c) Centralização das atividades práticas em estabelecimento da própria empresa, diverso do estabelecimento responsável pelo cumprimento da cota.
        /// </summary>
        [XmlElement("cnpjPrat")]
        public string CnpjPrat { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCnpjEntQual() => !string.IsNullOrEmpty(CnpjEntQual);

#if INTEROP
        public bool ShouldSerializeTpInsc() => TpInsc != (TiposInscricao)(-1);
#else
        public bool ShouldSerializeTpInsc() => TpInsc != null;
#endif

        public bool ShouldSerializeNrInsc() => !string.IsNullOrEmpty(NrInsc);

        public bool ShouldSerializeCnpjPrat() => !string.IsNullOrEmpty(CnpjPrat);

        #endregion
    }

    /// <summary>
    /// Informações de trabalhador estatutário
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEstatutario2200")]
    [ComVisible(true)]
#endif
    public class InfoEstatutario2200
    {
        /// <summary>
        /// Preencher com o tipo de provimento
        /// </summary>
        [XmlElement("tpProv")]
        public TipoProvimento TpProv { get; set; }

        /// <summary>
        /// Data da entrada em exercício pelo servidor
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtExercicio { get; set; }
#else
        public DateTimeOffset DtExercicio { get; set; }
#endif

        [XmlElement("dtExercicio")]
        public string DtExercicioField
        {
            get => DtExercicio.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtExercicio = DateTime.Parse(value);
#else
            set => DtExercicio = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Tipo de plano de segregação da massa
        /// </summary>
        [XmlElement("tpPlanRP")]
#if INTEROP
        public PlanoSegregacaoDaMassa TpPlanRP { get; set; } = (PlanoSegregacaoDaMassa)(-1);
#else
        public PlanoSegregacaoDaMassa? TpPlanRP { get; set; }
#endif

        /// <summary>
        /// Informar se o servidor está sujeito ao teto do RGPS pela instituição do regime de previdência complementar
        /// </summary>
        [XmlElement("indTetoRGPS")]
#if INTEROP
        public SimNaoLetra IndTetoRGPS { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndTetoRGPS { get; set; }
#endif

        /// <summary>
        /// Indicar se o servidor recebe abono permanência
        /// </summary>
        [XmlElement("indAbonoPerm")]
#if INTEROP
        public SimNaoLetra IndAbonoPerm { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndAbonoPerm { get; set; }
#endif

        /// <summary>
        /// Informar a data de inicio do abono permanência
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtIniAbono { get; set; }
#else
        public DateTimeOffset DtIniAbono { get; set; }
#endif

        [XmlElement("dtIniAbono")]
        public string DtIniAbonoField
        {
            get => DtIniAbono.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIniAbono = DateTime.Parse(value);
#else
            set => DtIniAbono = DateTimeOffset.Parse(value);
#endif
        }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeTpPlanRP() => TpPlanRP != (PlanoSegregacaoDaMassa)(-1);
#else
        public bool ShouldSerializeTpPlanRP() => TpPlanRP != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndTetoRGPS() => IndTetoRGPS != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndTetoRGPS() => IndTetoRGPS != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndAbonoPerm() => IndAbonoPerm != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndAbonoPerm() => IndAbonoPerm != null;
#endif

        public bool ShouldSerializeDtIniAbonoField() => DtIniAbono > DateTime.MinValue;

        #endregion
    }

    /// <summary>
    /// Informações do contrato de trabalho
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoContrato2200")]
    [ComVisible(true)]
#endif
    public class InfoContrato2200
    {
        /// <summary>
        /// Informar o nome do cargo
        /// </summary>
        [XmlElement("nmCargo")]
        public string NmCargo { get; set; }

        /// <summary>
        /// Informar a Classificação Brasileira de Ocupações - CBO relativa ao cargo
        /// </summary>
        [XmlElement("CBOCargo")]
        public string CBOCargo { get; set; }

        /// <summary>
        /// Data de ingresso do servidor no cargo
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtIngrCargo { get; set; }
#else
        public DateTimeOffset DtIngrCargo { get; set; }
#endif

        [XmlElement("dtIngrCargo")]
        public string DtIngrCargoField
        {
            get => DtIngrCargo.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIngrCargo = DateTime.Parse(value);
#else
            set => DtIngrCargo = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Informar o nome da função de confiança/cargo em comissão
        /// </summary>
        [XmlElement("nmFuncao")]
        public string NmFuncao { get; set; }

        /// <summary>
        /// Informar a CBO relativa à função de confiança/cargo em comissão
        /// </summary>
        [XmlElement("CBOFuncao")]
        public string CBOFuncao { get; set; }

        /// <summary>
        /// Informar se o cargo, emprego ou função pública é acumulável
        /// </summary>
        [XmlElement("acumCargo")]
#if INTEROP
        public SimNaoLetra AcumCargo { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? AcumCargo { get; set; }
#endif
        /// <summary>
        /// Preencher com o código da categoria do trabalhador
        /// </summary>
        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        /// <summary>
        /// Informações da remuneração e periodicidade de pagamento
        /// </summary>
        [XmlElement("remuneracao")]
        public Remuneracao2200 Remuneracao { get; set; }

        /// <summary>
        /// Informações da remuneração e periodicidade de pagamento
        /// </summary>
        [XmlElement("duracao")]
        public Duracao2200 Duracao { get; set; }

        /// <summary>
        /// Informações do local de trabalho
        /// </summary>
        [XmlElement("localTrabalho")]
        public LocalTrabalho2200 LocalTrabalho { get; set; }

        /// <summary>
        /// Informações do horário contratual do trabalhador
        /// </summary>
        [XmlElement("horContratual")]
        public HorContratual2200 HorContratual { get; set; }

        /// <summary>
        /// Informações do alvará judicial em caso de contratação de menores de 14 anos, em qualquer categoria, 
        /// e de maiores de 14 e menores de 16, em categoria diferente de "Aprendiz"
        /// </summary>
        [XmlElement("alvaraJudicial")]
        public AlvaraJudicial2200 AlvaraJudicial { get; set; }

        /// <summary>
        /// Observações do contrato de trabalho
        /// </summary>
        [XmlElement("observacoes")]
        public List<Observacoes2200> Observacoes { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddObservacoes(Observacoes2200 item)
        {
            if (Observacoes == null)
            {
                Observacoes = new List<Observacoes2200>();
            }

            Observacoes.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Observacoes (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Observacoes</returns>
        public Observacoes2200 GetObservacoes(int index)
        {
            if ((Observacoes?.Count ?? 0) == 0)
            {
                return default;
            };

            return Observacoes[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Observacoes
        /// </summary>
        public int GetObservacoesCount => (Observacoes != null ? Observacoes.Count : 0);
#endif

        /// <summary>
        /// Treinamentos, capacitações, exercícios simulados, autorizações ou outras anotações que devam ser anotadas 
        /// no registro de empregados e/ou na CTPS, por determinação de Norma Regulamentadora - NR
        /// </summary>
        [XmlElement("treiCap")]
        public List<TreiCap2200> TreiCap { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTreiCap(TreiCap2200 item)
        {
            if (TreiCap == null)
            {
                TreiCap = new List<TreiCap2200>();
            }

            TreiCap.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TreiCap (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TreiCap</returns>
        public TreiCap2200 GetTreiCap(int index)
        {
            if ((TreiCap?.Count ?? 0) == 0)
            {
                return default;
            };

            return TreiCap[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TreiCap
        /// </summary>
        public int GetTreiCapCount => (TreiCap != null ? TreiCap.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeNmCargo() => !string.IsNullOrEmpty(NmCargo);

        public bool ShouldSerializeCBOCargo() => !string.IsNullOrEmpty(CBOCargo);

        public bool ShouldSerializeDtIngrCargoField() => DtIngrCargo > DateTime.MinValue;

        public bool ShouldSerializeNmFuncao() => !string.IsNullOrEmpty(NmFuncao);

        public bool ShouldSerializeCBOFuncao() => !string.IsNullOrEmpty(CBOFuncao);

#if INTEROP
        public bool ShouldSerializeAcumCargo() => AcumCargo != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeAcumCargo() => AcumCargo != null;
#endif

        #endregion
    }

    /// <summary>
    /// Informações da remuneração e periodicidade de pagamento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Remuneracao2200")]
    [ComVisible(true)]
#endif
    public class Remuneracao2200
    {
        /// <summary>
        /// Salário base do trabalhador, correspondente à parte fixa da remuneração.
        /// Validação: Se undSalFixo for igual a[7], preencher com 0 (zero).
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
        /// Unidade de pagamento da parte fixa da remuneração
        /// </summary>
        [XmlElement("undSalFixo")]
        public UndSalFixo UndSalFixo { get; set; }

        /// <summary>
        /// Descrição do salário por tarefa ou variável e como este é calculado
        /// </summary>
        [XmlElement("dscSalVar")]
        public string DscSalVar { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDscSalVar() => !string.IsNullOrEmpty(DscSalVar);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Duração do contrato de trabalho
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Duracao2200")]
    [ComVisible(true)]
#endif
    public class Duracao2200
    {
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

        /// <summary>
        /// Indicar se o contrato por prazo determinado contém cláusula assecuratória do direito recíproco de rescisão antes da data de seu término
        /// </summary>
        [XmlElement("clauAssec")]
#if INTEROP
        public SimNaoLetra ClauAssec { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? ClauAssec { get; set; }
#endif

        /// <summary>
        /// Indicação do objeto determinante da contratação por prazo determinado (obra, serviço, safra, etc.)
        /// </summary>
        [XmlElement("objDet")]
        public string ObjDet { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDtTermField() => DtTerm > DateTime.MinValue;

#if INTEROP
        public bool ShouldSerializeClauAssec() => ClauAssec != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeClauAssec() => ClauAssec != null;
#endif

        public bool ShouldSerializeObjDet() => !string.IsNullOrEmpty(ObjDet);

        #endregion
    }

    /// <summary>
    /// Informações do local de trabalho
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalTrabalho2200")]
    [ComVisible(true)]
#endif
    public class LocalTrabalho2200
    {
        /// <summary>
        /// Estabelecimento (CNPJ, CNO, CAEPF) onde o trabalhador (exceto doméstico) exercerá suas atividades. Caso o trabalhador exerça 
        /// suas atividades em instalações de terceiros, este campo deve ser preenchido com o estabelecimento do próprio empregador ao 
        /// qual o trabalhador esteja vinculado
        /// </summary>
        [XmlElement("localTrabGeral")]
        public LocalTrabGeral2200 LocalTrabGeral { get; set; }

        /// <summary>
        /// Grupo preenchido exclusivamente em caso de trabalhador doméstico e trabalhador temporário, indicando o endereço onde o 
        /// trabalhador exerce suas atividades
        /// </summary>
        [XmlElement("localTempDom")]
        public LocalTempDom2200 LocalTempDom { get; set; }
    }

    /// <summary>
    /// Estabelecimento (CNPJ, CNO, CAEPF) onde o trabalhador (exceto doméstico) exercerá suas atividades. 
    /// Caso o trabalhador exerça suas atividades em instalações de terceiros, este campo deve ser preenchido com o 
    /// estabelecimento do próprio empregador ao qual o trabalhador esteja vinculado.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalTrabGeral2200")]
    [ComVisible(true)]
#endif
    public class LocalTrabGeral2200
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 05
        /// </summary>
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do contribuinte de acordo com o tipo de inscrição indicado no campo localTrabGeral/tpInsc
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Descrição complementar do local de trabalho
        /// </summary>
        [XmlElement("descComp")]
        public string DescComp { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDescComp() => !string.IsNullOrEmpty(DescComp);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Grupo preenchido exclusivamente em caso de trabalhador doméstico e trabalhador temporário, indicando o endereço onde o trabalhador exerce suas atividades
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalTempDom2200")]
    [ComVisible(true)]
#endif
    public class LocalTempDom2200 : Brasil { }

    /// <summary>
    /// Informações do horário contratual do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.HorContratual2200")]
    [ComVisible(true)]
#endif
    public class HorContratual2200
    {
        /// <summary>
        /// Quantidade média de horas relativas à jornada semanal do trabalhador
        /// </summary>
        [XmlIgnore]
        public double QtdHrsSem { get; set; }

        [XmlElement("qtdHrsSem")]
        public string QtdHrsSemField
        {
            get => QtdHrsSem.ToString("F2", CultureInfo.InvariantCulture);
            set => QtdHrsSem = Converter.ToDouble(value);
        }

        /// <summary>
        /// Tipo de jornada
        /// </summary>
        [XmlElement("tpJornada")]
        public TpJornada TpJornada { get; set; }

        /// <summary>
        /// Preencher com o código relativo ao tipo de contrato em tempo parcial
        /// </summary>
        [XmlElement("tmpParc")]
        public TmpParc TmpParc { get; set; }

        /// <summary>
        /// Indicar se a jornada semanal possui horário noturno (no todo ou em parte).
        /// </summary>
        [XmlElement("horNoturno")]
#if INTEROP
        public SimNaoLetra HorNoturno { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? HorNoturno { get; set; }
#endif

        /// <summary>
        /// Descrição da jornada semanal contratual, contendo os dias da semana e os respectivos horários contratuais (entrada, saída e intervalos).
        /// </summary>
        [XmlElement("dscJorn")]
        public string DscJorn { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeQtdHrsSemField() => QtdHrsSem > 0;

#if INTEROP
        public bool ShouldSerializeHorNoturno() => HorNoturno != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeHorNoturno() => HorNoturno != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações do alvará judicial em caso de contratação de menores de 14 anos, em qualquer categoria, 
    /// e de maiores de 14 e menores de 16, em categoria diferente de "Aprendiz".
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.AlvaraJudicial2200")]
    [ComVisible(true)]
#endif
    public class AlvaraJudicial2200
    {
        /// <summary>
        /// Preencher com o número do processo judicial
        /// </summary>
        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }
    }

    /// <summary>
    /// Observações do contrato de trabalho
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Observacoes2200")]
    [ComVisible(true)]
#endif
    public class Observacoes2200
    {
        /// <summary>
        /// Observação relacionada ao contrato de trabalho
        /// </summary>
        [XmlElement("observacao")]
        public string Observacao { get; set; }
    }

    /// <summary>
    /// Treinamentos, capacitações, exercícios simulados, autorizações ou outras anotações que devam ser anotadas 
    /// no registro de empregados e/ou na CTPS, por determinação de Norma Regulamentadora - NR.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TreiCap2200")]
    [ComVisible(true)]
#endif
    public class TreiCap2200
    {
        /// <summary>
        /// Informar o código do treinamento, capacitação, exercício simulado ou outra anotação, conforme Tabela 28.
        /// </summary>
        [XmlElement("codTreiCap")]
        public string CodTreiCap { get; set; }
    }

    /// <summary>
    /// Grupo de informações da sucessão de vínculo trabalhista/estatutário
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SucessaoVinc2200")]
    [ComVisible(true)]
#endif
    public class SucessaoVinc2200
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 05.
        /// </summary>
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do empregador anterior, de acordo com o tipo de inscrição indicado no campo sucessaoVinc/tpInsc.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Matrícula do trabalhador no empregador anterior.
        /// </summary>
        [XmlElement("matricAnt")]
        public string MatricAnt { get; set; }

        /// <summary>
        /// Preencher com a data da transferência do empregado para o empregador declarante.
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

        public bool ShouldSerializeMatricAnt() => !string.IsNullOrEmpty(MatricAnt);

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

        #endregion
    }

    /// <summary>
    /// Informações do empregado doméstico transferido de outro representante da mesma unidade familiar
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TransfDom")]
    [ComVisible(true)]
#endif
    public class TransfDom
    {
        /// <summary>
        /// Preencher com o número do CPF do representante anterior da unidade familiar
        /// </summary>
        [XmlElement("cpfSubstituido")]
        public string CpfSubstituido { get; set; }

        /// <summary>
        /// Matrícula do trabalhador no representante anterior da unidade familiar
        /// </summary>
        [XmlElement("matricAnt")]
        public string MatricAnt { get; set; }

        /// <summary>
        /// Data da transferência do vínculo ao novo representante da unidade familiar
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

        #region ShouldSerialize

        public bool ShouldSerializeMatricAnt() => !string.IsNullOrEmpty(MatricAnt);

        #endregion
    }

    /// <summary>
    /// Informações de mudança de CPF do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.MudancaCPF2200")]
    [ComVisible(true)]
#endif
    public class MudancaCPF2200
    {
        /// <summary>
        /// Preencher com o número do CPF antigo do trabalhador
        /// </summary>
        [XmlElement("cpfAnt")]
        public string CpfAnt { get; set; }

        /// <summary>
        /// Preencher com a matrícula anterior do trabalhador
        /// </summary>
        [XmlElement("matricAnt")]
        public string MatricAnt { get; set; }

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

        #endregion
    }

    /// <summary>
    /// Informações de afastamento do trabalhador.
    /// Preenchimento exclusivo em caso de trabalhador que permaneça afastado na data de início da 
    /// obrigatoriedade dos eventos não periódicos para o empregador no eSocial ou na data de transferência ou 
    /// alteração de CPF do empregado.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Afastamento2200")]
    [ComVisible(true)]
#endif
    public class Afastamento2200
    {
        /// <summary>
        /// Data de início do afastamento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtIniAfast { get; set; }
#else
        public DateTimeOffset DtIniAfast { get; set; }
#endif

        [XmlElement("dtIniAfast")]
        public string DtIniAfastField
        {
            get => DtIniAfast.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIniAfast = DateTime.Parse(value);
#else
            set => DtIniAfast = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Preencher com o código do motivo de afastamento temporário
        /// </summary>
        [XmlElement("codMotAfast")]
        public string CodMotAfast { get; set; }
    }

    /// <summary>
    /// Informação do desligamento do trabalhador.
    /// Grupo preenchido exclusivamente caso seja necessário enviar cadastramento inicial referente a trabalhador que 
    /// já tenha sido desligado da empresa antes do início dos eventos não periódicos para o empregador no eSocial
    /// (por exemplo, envio para pagamento de diferenças salariais - acordo/dissídio/convenção coletiva - em meses posteriores ao 
    /// desligamento e sob vigência dos eventos periódicos para o empregador no eSocial) ou no caso de desligamento em data anterior 
    /// à transferência do empregado.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Desligamento")]
    [ComVisible(true)]
#endif
    public class Desligamento
    {
        /// <summary>
        /// Preencher com a data de desligamento do vínculo (último dia trabalhado)
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtDeslig { get; set; }
#else
        public DateTimeOffset DtDeslig { get; set; }
#endif

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
    }

    /// <summary>
    /// Informação de cessão/exercício em outro órgão do trabalhador.
    /// Preenchimento exclusivo em caso de trabalhador que permaneça cedido/em exercício em outro órgão na data de 
    /// início da obrigatoriedade dos eventos não periódicos para o empregador/ente público no eSocial ou na data de 
    /// transferência ou alteração de CPF do empregado.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Cessao")]
    [ComVisible(true)]
#endif
    public class Cessao
    {
        /// <summary>
        /// Data de início da cessão/exercício em outro órgão
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtIniCessao { get; set; }
#else
        public DateTimeOffset DtIniCessao { get; set; }
#endif

        [XmlElement("dtIniCessao")]
        public string DtIniCessaoField
        {
            get => DtIniCessao.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIniCessao = DateTime.Parse(value);
#else
            set => DtIniCessao = DateTimeOffset.Parse(value);
#endif
        }
    }
}
