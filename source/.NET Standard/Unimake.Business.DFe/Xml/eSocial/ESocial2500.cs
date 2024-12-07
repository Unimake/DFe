#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2500 - Processo Trabalhista
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2500")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtProcTrab/v_S_01_02_00", IsNullable = false)]
    public class ESocial2500 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Processo Trabalhista
        /// </summary>
        [XmlElement("evtProcTrab")]
        public EvtProcTrab EvtProcTrab { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Processo Trabalhista
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtProcTrab")]
    [ComVisible(true)]
#endif
    public class EvtProcTrab
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
        public IdeEvento2500 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador ou do contribuinte que está prestando a informação
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador2500 IdeEmpregador { get; set; }

        /// <summary>
        /// Informações do processo judicial ou de demanda submetida à CCP ou ao NINTER
        /// </summary>
        [XmlElement("infoProcesso")]
        public InfoProcesso InfoProcesso { get; set; }

        /// <summary>
        /// Informações do trabalhador
        /// </summary>
        [XmlElement("ideTrab")]
        public IdeTrab IdeTrab { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2500")]
    [ComVisible(true)]
#endif
    public class IdeEvento2500 : IdeEvento2205 { }

    /// <summary>
    /// Informações de identificação do empregador ou do contribuinte que está prestando a informação
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador2500")]
    [ComVisible(true)]
#endif
    public class IdeEmpregador2500 : IdeEmpregador
    {
        [XmlElement("ideResp")]
        public IdeResp IdeResp { get; set; }
    }

    /// <summary>
    /// Identificação do contribuinte, caso tenha havido imposição de responsabilidade indireta
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeResp")]
    [ComVisible(true)]
#endif
    public class IdeResp : IdeEmpregador
    {
        /// <summary>
        /// Preencher com a data de admissão do trabalhador no empregador de origem(responsável direto) quando se tratar de vínculo que não foi informado no eSocial. Em caso de TSVE sem informação de matrícula no evento S-2300, informar a data de início.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAdmRespDir { get; set; }
#else
        public DateTimeOffset DtAdmRespDir { get; set; }
#endif

        [XmlElement("dtAdmRespDir")]
        public string DtAdmRespDirField
        {
            get => DtAdmRespDir.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAdmRespDir = DateTime.Parse(value);
#else
            set => DtAdmRespDir = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        ///  Informar a matrícula no empregador de origem (responsável direto).
        /// </summary>
        [XmlElement("matRespDir")]
        public string MatRespDir { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDtAdmRespDirField() => DtAdmRespDir > DateTime.MinValue;
        public bool ShouldSerializeMatRespDir() => !string.IsNullOrEmpty(MatRespDir);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações do processo judicial ou de demanda submetida à CCP ou ao NINTER
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcesso")]
    [ComVisible(true)]
#endif
    public class InfoProcesso
    {
        /// <summary>
        /// Informar a origem do processo/demanda
        /// </summary>
        [XmlElement("origem")]
        public Origem Origem { get; set; }

        /// <summary>
        /// Número do processo trabalhista, da ata ou número de identificação da conciliação
        /// </summary>
        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }

        /// <summary>
        /// Observações relacionadas ao processo judicial ou à demanda submetida à CCP ou ao NINTER
        /// </summary>
        [XmlElement("obsProcTrab")]
        public string ObsProcTrab { get; set; }

        /// <summary>
        /// Informações complementares do processo ou da demanda
        /// </summary>
        [XmlElement("dadosCompl")]
        public DadosCompl DadosCompl { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeObsProcTrab() => !string.IsNullOrEmpty(ObsProcTrab);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações complementares do processo ou da demanda
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosCompl")]
    [ComVisible(true)]
#endif
    public class DadosCompl
    {
        /// <summary>
        /// Informações complementares do processo judicial
        /// </summary>
        [XmlElement("infoProcJud")]
        public InfoProcJud InfoProcJud { get; set; }

        /// <summary>
        /// Informações complementares da demanda submetida à CCP ou ao NINTER
        /// </summary>
        [XmlElement("infoCCP")]
        public InfoCCP InfoCCP { get; set; }
    }

    /// <summary>
    /// Informações complementares do processo judicial
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcJud")]
    [ComVisible(true)]
#endif
    public class InfoProcJud
    {
        /// <summary>
        /// Informar a data da:
        /// a) Determinação judicial para o cumprimento da decisão líquida transitada em julgado;
        /// b) Homologação de acordo judicial; ou
        /// c) Decisão que determinar o cumprimento antecipado de obrigação
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtSent { get; set; }
#else
        public DateTimeOffset DtSent { get; set; }
#endif

        [XmlElement("dtSent")]
        public string DtSentField
        {
            get => DtSent.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtSent = DateTime.Parse(value);
#else
            set => DtSent = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Preencher com a sigla da Unidade da Federação onde está localizada a Vara em que o processo tramitou
        /// </summary>
        [XmlElement("ufVara")]
        public UFBrasil UfVara { get; set; }

        /// <summary>
        /// Preencher com o código do município, conforme tabela do IBGE.
        /// </summary>
        [XmlElement("codMunic")]
        public string CodMunic { get; set; }

        /// <summary>
        /// Código de identificação da Vara em que o processo tramitou
        /// </summary>
        [XmlElement("idVara")]
        public string IdVara { get; set; }
    }

    /// <summary>
    /// Informações complementares da demanda submetida à CCP ou ao NINTER
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCCP")]
    [ComVisible(true)]
#endif
    public class InfoCCP
    {
        /// <summary>
        /// Data da celebração do acordo celebrado perante CCP ou NINTER
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtCCP { get; set; }
#else
        public DateTimeOffset DtCCP { get; set; }
#endif

        [XmlElement("dtCCP")]
        public string DtCCPField
        {
            get => DtCCP.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtCCP = DateTime.Parse(value);
#else
            set => DtCCP = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Indicar o âmbito de celebração do acordo
        /// </summary>
        [XmlElement("tpCCP")]
        public TpCCP TpCCP { get; set; }

        /// <summary>
        /// Identificar o CNPJ do sindicato representativo do trabalhador, no âmbito da CCP ou NINTER
        /// </summary>
        [XmlElement("cnpjCCP")]
        public string CnpjCCP { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCnpjCCP() => !string.IsNullOrEmpty(CnpjCCP);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrab")]
    [ComVisible(true)]
#endif
    public class IdeTrab
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
        /// Informações do contrato de trabalho
        /// </summary>
        [XmlElement("infoContr")]
        public List<InfoContr> InfoContr { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoContr(InfoContr item)
        {
            if (InfoContr == null)
            {
                InfoContr = new List<InfoContr>();
            }

            InfoContr.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoContr (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoContr</returns>
        public InfoContr GetInfoContr(int index)
        {
            if ((InfoContr?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoContr[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoContr
        /// </summary>
        public int GetInfoContrCount => (InfoContr != null ? InfoContr.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeNmTrab() => !string.IsNullOrEmpty(NmTrab);

        public bool ShouldSerializeDtNasctoField() => DtNascto > DateTime.MinValue;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações do contrato de trabalho
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoContr")]
    [ComVisible(true)]
#endif
    public class InfoContr
    {
        /// <summary>
        /// Tipo de contrato a que se refere o processo judicial ou a demanda submetida à CCP ou ao NINTER
        /// </summary>
        [XmlElement("tpContr")]
        public TpContr TpContr { get; set; }

        /// <summary>
        /// Indicativo se o contrato possui informação no evento S-2190, S-2200 ou S-2300 no declarante
        /// </summary>
        [XmlElement("indContr")]
        public SimNaoLetra IndContr { get; set; }

        /// <summary>
        /// Preencher com a data de admissão original do vínculo (data de admissão antes da alteração)
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAdmOrig { get; set; }
#else
        public DateTimeOffset DtAdmOrig { get; set; }
#endif

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
        /// Indicativo de reintegração do empregado
        /// </summary>
        [XmlElement("indReint")]
#if INTEROP
        public SimNaoLetra IndReint { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndReint { get; set; }
#endif

        /// <summary>
        /// Indicativo se houve reconhecimento de categoria do trabalhador diferente da informada (no eSocial ou na GFIP) pelo declarante
        /// </summary>
        [XmlElement("indCateg")]
        public SimNaoLetra IndCateg { get; set; }

        /// <summary>
        /// Indicativo se houve reconhecimento de natureza da atividade diferente da cadastrada pelo declarante
        /// </summary>
        [XmlElement("indNatAtiv")]
        public SimNaoLetra IndNatAtiv { get; set; }

        /// <summary>
        /// Indicativo se houve reconhecimento de motivo de desligamento diferente do informado pelo declarante
        /// </summary>
        [XmlElement("indMotDeslig")]
        public SimNaoLetra IndMotDeslig { get; set; }

        /// <summary>
        /// Matrícula atribuída ao trabalhador pela empresa ou, no caso de servidor público, a matrícula constante no Sistema de Administração de Recursos Humanos do órgão
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

        /// <summary>
        /// Data de início de TSVE, que pode ser:
        /// a) Para o cooperado, a data de ingresso na cooperativa;
        /// b) Para o diretor não empregado, a data de posse no cargo;
        /// c) Para o dirigente sindical, a data de início do mandato no sindicato;
        /// d) Para o estagiário, a data de início do estágio;
        /// e) Para o trabalhador avulso, a data de ingresso no Órgão Gestor de Mão de Obra - OGMO ou no sindicato;
        /// f) Para o servidor público exercente de cargo eletivo, a data de início do mandato;
        /// g) Para os demais trabalhadores, a data de início das atividades
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtInicio { get; set; }
#else
        public DateTimeOffset DtInicio { get; set; }
#endif

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
        /// Informações complementares do contrato de trabalho
        /// </summary>
        [XmlElement("infoCompl")]
        public InfoCompl InfoCompl { get; set; }

        /// <summary>
        /// Informação do novo código de categoria e/ou da nova natureza da atividade, no caso de reconhecimento judicial nesse sentido
        /// </summary>
        [XmlElement("mudCategAtiv")]
        public List<MudCategAtiv> MudCategAtiv { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddMudCategAtiv(MudCategAtiv item)
        {
            if (MudCategAtiv == null)
            {
                MudCategAtiv = new List<MudCategAtiv>();
            }

            MudCategAtiv.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista MudCategAtiv (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da MudCategAtiv</returns>
        public MudCategAtiv GetMudCategAtiv(int index)
        {
            if ((MudCategAtiv?.Count ?? 0) == 0)
            {
                return default;
            };

            return MudCategAtiv[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista MudCategAtiv
        /// </summary>
        public int GetMudCategAtivCount => (MudCategAtiv != null ? MudCategAtiv.Count : 0);
#endif

        /// <summary>
        /// Informações dos vínculos/contratos incorporados, no caso de reconhecimento de unicidade contratual
        /// </summary>
        [XmlElement("unicContr")]
        public List<UnicContr> UnicContr { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddUnicContr(UnicContr item)
        {
            if (UnicContr == null)
            {
                UnicContr = new List<UnicContr>();
            }

            UnicContr.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista UnicContr (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da UnicContr</returns>
        public UnicContr GetUnicContr(int index)
        {
            if ((UnicContr?.Count ?? 0) == 0)
            {
                return default;
            };

            return UnicContr[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista UnicContr
        /// </summary>
        public int GetUnicContrCount => (UnicContr != null ? UnicContr.Count : 0);
#endif

        /// <summary>
        /// Identificação do estabelecimento responsável pelo pagamento ao trabalhador dos valores informados neste evento
        /// </summary>
        [XmlElement("ideEstab")]
        public IdeEstab2500 IdeEstab { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDtAdmOrigField() => DtAdmOrig > DateTime.MinValue;

#if INTEROP
        public bool ShouldSerializeIndReint() => IndReint != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndReint() => IndReint != null;
#endif

        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);

#if INTEROP
        public bool ShouldSerializeCodCateg() => CodCateg != (CodCateg)(-1);
#else
        public bool ShouldSerializeCodCateg() => CodCateg != null;
#endif
        public bool ShouldSerializeDtInicioField() => DtInicio > DateTime.MinValue;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações complementares do contrato de trabalho
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCompl")]
    [ComVisible(true)]
#endif
    public class InfoCompl
    {
        /// <summary>
        /// Classificação Brasileira de Ocupações - CBO
        /// </summary>
        [XmlElement("codCBO")]
        public string CodCBO { get; set; }

        /// <summary>
        /// Natureza da atividade
        /// </summary>
        [XmlElement("natAtividade")]
#if INTEROP
        public NatAtividade NatAtividade { get; set; } = (NatAtividade)(-1);
#else
        public NatAtividade? NatAtividade { get; set; }
#endif

        /// <summary>
        /// Informações da remuneração e periodicidade de pagamento
        /// </summary>
        [XmlElement("remuneracao")]
        public List<Remuneracao2500> Remuneracao { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRemuneracao(Remuneracao2500 item)
        {
            if (Remuneracao == null)
            {
                Remuneracao = new List<Remuneracao2500>();
            }

            Remuneracao.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Remuneracao (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Remuneracao</returns>
        public Remuneracao2500 GetRemuneracao(int index)
        {
            if ((Remuneracao?.Count ?? 0) == 0)
            {
                return default;
            };

            return Remuneracao[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Remuneracao
        /// </summary>
        public int GetRemuneracaoCount => (Remuneracao != null ? Remuneracao.Count : 0);
#endif

        /// <summary>
        /// Informações sobre o vínculo trabalhista
        /// </summary>
        [XmlElement("infoVinc")]
        public InfoVinc InfoVinc { get; set; }

        /// <summary>
        /// Informações de término de TSVE
        /// </summary>
        [XmlElement("infoTerm")]
        public InfoTerm InfoTerm { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCodCBO() => !string.IsNullOrEmpty(CodCBO);

#if INTEROP
        public bool ShouldSerializeNatAtividade() => NatAtividade != (NatAtividade)(-1);
#else
        public bool ShouldSerializeNatAtividade() => NatAtividade != null;
#endif

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Remuneracao2500")]
    [ComVisible(true)]
#endif
    public class Remuneracao2500
    {
        /// <summary>
        /// Data a partir da qual as informações de remuneração e periodicidade de pagamento estão vigentes
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtRemun { get; set; }
#else
        public DateTimeOffset DtRemun { get; set; }
#endif

        [XmlElement("dtRemun")]
        public string DtRemunField
        {
            get => DtRemun.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtRemun = DateTime.Parse(value);
#else
            set => DtRemun = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Salário base do trabalhador, correspondente à parte fixa da remuneração em dtRemun
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
        /// Descrição do salário por tarefa ou variável e como este é calculado. Ex.: Comissões pagas no percentual de 10% sobre as vendas
        /// </summary>
        [XmlElement("dscSalVar")]
        public string DscSalVar { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDscSalVar() => !string.IsNullOrEmpty(DscSalVar);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações sobre o vínculo 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoVinc")]
    [ComVisible(true)]
#endif
    public class InfoVinc
    {
        /// <summary>
        /// Tipo de regime trabalhista
        /// </summary>
        [XmlElement("tpRegTrab")]
        public TipoRegimeTrabalhista TpRegTrab { get; set; }

        /// <summary>
        /// 1 - Regime Geral de Previdência Social - RGPS
        /// 2 - Regime Próprio de Previdência Social - RPPS
        /// 3 - Regime de Previdência Social no exterior
        /// </summary>
        [XmlElement("tpRegPrev")]
        public TpRegPrev TpRegPrev { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtAdm { get; set; }
#else
        public DateTimeOffset DtAdm { get; set; }
#endif

        /// <summary>
        /// Preencher com a data de admissão do trabalhador
        /// </summary>
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
        /// Preencher com o código relativo ao tipo de contrato em tempo parcial.
        /// Informar este campo apenas no caso de empregado submetido a horário de trabalho(Capítulo II do Título II da CLT)
        /// </summary>
        [XmlElement("tmpParc")]
#if INTEROP
        public TmpParc TmpParc { get; set; } = (TmpParc)(-1);
#else
        public TmpParc? TmpParc { get; set; }
#endif

        /// <summary>
        /// Duração do contrato de trabalho
        /// </summary>
        [XmlElement("duracao")]
        public Duracao2500 Duracao { get; set; }

        /// <summary>
        /// Observações do contrato de trabalho
        /// </summary>
        [XmlElement("observacoes")]
        public List<Observacoes2500> Observacoes { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddObservacoes(Observacoes2500 item)
        {
            if (Observacoes == null)
            {
                Observacoes = new List<Observacoes2500>();
            }

            Observacoes.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Observacoes2500 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Observacoes</returns>
        public Observacoes2500 GetObservacoes(int index)
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
        /// Grupo de informações da sucessão de vínculo trabalhista/estatutário
        /// </summary>
        [XmlElement("sucessaoVinc")]
        public SucessaoVinc2500 SucessaoVinc { get; set; }

        /// <summary>
        /// Informações do desligamento
        /// </summary>
        [XmlElement("infoDeslig")]
        public InfoDeslig2500 InfoDeslig { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeTmpParc() => TmpParc != (TmpParc)(-1);
#else
        public bool ShouldSerializeTmpParc() => TmpParc != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Duração do contrato de trabalho
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Duracao2500")]
    [ComVisible(true)]
#endif
    public class Duracao2500
    {
        /// <summary>
        /// Tipo de contrato de trabalho
        /// </summary>
        [XmlElement("tpContr")]
        public TipoDeContratoDeTrabalho TipoDeContratoDeTrabalho { get; set; }

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

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Observações do contrato de trabalho
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Observacoes2500")]
    [ComVisible(true)]
#endif
    public class Observacoes2500 : Observacoes2306 { }

    /// <summary>
    /// Grupo de informações da sucessão de vínculo trabalhista/estatutário
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SucessaoVinc2500")]
    [ComVisible(true)]
#endif
    public class SucessaoVinc2500
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 
        /// Valores válidos:
        /// 1 - CNPJ
        /// 2 - CPF
        /// 5 - CGC
        /// 6 - CEI
        /// </summary>
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do empregador anterior, de acordo com o tipo de inscrição indicado no campo sucessaoVinc/tpInsc
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Matrícula do trabalhador no empregador 
        /// </summary>
        [XmlElement("matricAnt")]
        public string MatricAnt { get; set; }

        /// <summary>
        /// Preencher com a data da transferência do empregado para o empregador declarante
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

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações do desligamento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoDeslig2500")]
    [ComVisible(true)]
#endif
    public class InfoDeslig2500
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

        /// <summary>
        /// Código de motivo do desligamento
        /// </summary>
        [XmlElement("mtvDeslig")]
        public string MtvDeslig { get; set; }

        /// <summary>
        /// Data projetada para o término do aviso prévio indenizado
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtProjFimAPI { get; set; }
#else
        public DateTimeOffset DtProjFimAPI { get; set; }
#endif

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
        /// Indicativo de pensão alimentícia para fins de retenção de FGTS
        /// </summary>
        [XmlElement("pensAlim")]
#if INTEROP
        public PensAlim PensAlim { get; set; } = (PensAlim)(-1);
#else
        public PensAlim? PensAlim { get; set; }
#endif
        /// <summary>
        /// Percentual a ser destinado a pensão alimentícia.
        /// Validação: Deve ser maior que 0 (zero) e menor ou igual a 100 (cem).
        /// Informação obrigatória e exclusiva se pensAlim = [1, 3].
        /// </summary>
        [XmlIgnore]
        public double PercAliment { get; set; }

        [XmlElement("percAliment")]
        public string PercAlimentField
        {
            get => PercAliment.ToString("F2", CultureInfo.InvariantCulture);
            set => PercAliment = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da pensão alimentícia.
        /// Validação: Deve ser maior que 0 (zero).
        /// Informação obrigatória e exclusiva se pensAlim = [2, 3].
        /// </summary>
        [XmlIgnore]
        public double VrAlim { get; set; }

        [XmlElement("vrAlim")]
        public string VrAlimField
        {
            get => VrAlim.ToString("F2", CultureInfo.InvariantCulture);
            set => VrAlim = Converter.ToDouble(value);
        }

        #region ShouldSerialize
        public bool ShouldSerializeDtProjFimAPIField() => DtProjFimAPI > DateTime.MinValue;

#if INTEROP
        public bool ShouldSerializePensAlim() => PensAlim != (PensAlim)(-1);
#else
        public bool ShouldSerializePensAlim() => PensAlim != null;
#endif

        public bool ShouldSerializePercAlimentField() => PercAliment > 0;

        public bool ShouldSerializeVrAlimField() => VrAlim > 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações de término de TSVE
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTerm")]
    [ComVisible(true)]
#endif
    public class InfoTerm
    {
        /// <summary>
        /// Data do término
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
        /// Motivo do término do diretor não empregado, com FGTS
        /// </summary>
        [XmlElement("mtvDesligTSV")]
#if INTEROP
        public MtvDesligTSV MtvDesligTSV { get; set; } = (MtvDesligTSV)(-1);
#else
        public MtvDesligTSV? MtvDesligTSV { get; set; }
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeMtvDesligTSV() => MtvDesligTSV != (MtvDesligTSV)(-1);
#else
        public bool ShouldSerializeMtvDesligTSV() => MtvDesligTSV != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informação do novo código de categoria e/ou da nova natureza da atividade, no caso de reconhecimento judicial nesse sentido
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.MudCategAtiv")]
    [ComVisible(true)]
#endif
    public class MudCategAtiv
    {
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
        public NatAtividade? NatAtividade { get; set; }
#endif

        /// <summary>
        /// Data a partir da qual foi reconhecida a nova categoria e/ou a nova natureza da atividade
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtMudCategAtiv { get; set; }
#else
        public DateTimeOffset DtMudCategAtiv { get; set; }
#endif

        [XmlElement("dtMudCategAtiv")]
        public string DtMudCategAtivField
        {
            get => DtMudCategAtiv.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtMudCategAtiv = DateTime.Parse(value);
#else
            set => DtMudCategAtiv = DateTimeOffset.Parse(value);
#endif
        }

        #region ShouldSerialize


#if INTEROP
        public bool ShouldSerializeNatAtividade() => NatAtividade != (NatAtividade)(-1);
#else
        public bool ShouldSerializeNatAtividade() => NatAtividade != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações dos vínculos/contratos incorporados, no caso de reconhecimento de unicidade contratual
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.UnicContr")]
    [ComVisible(true)]
#endif
    public class UnicContr
    {
        /// <summary>
        /// Informar a matrícula incorporada (matrícula cujo vínculo/contrato passou a integrar período de unicidade contratual reconhecido judicialmente)
        /// </summary>
        [XmlElement("matUnic")]
        public string MatUnic { get; set; }

        /// <summary>
        /// Preencher com o código da categoria do trabalhador (código de categoria cujo contrato passou a integrar período de unicidade contratual reconhecido judicialmente)
        /// </summary>
        [XmlElement("codCateg")]
#if INTEROP
        public CodCateg CodCateg { get; set; } = (CodCateg)(-1);
#else
        public CodCateg? CodCateg { get; set; }
#endif

        /// <summary>
        /// Data de início de TSVE (data de início cujo contrato passou a integrar período de unicidade contratual reconhecido judicialmente)
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtInicio { get; set; }
#else
        public DateTimeOffset DtInicio { get; set; }
#endif

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

        #region ShouldSerialize

        public bool ShouldSerializeMatUnic() => !string.IsNullOrEmpty(MatUnic);

#if INTEROP
        public bool ShouldSerializeCodCateg() => CodCateg != (CodCateg)(-1);
#else
        public bool ShouldSerializeCodCateg() => CodCateg != null;
#endif

        public bool ShouldSerializeDtInicioField() => DtInicio > DateTime.MinValue;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Identificação do estabelecimento responsável pelo pagamento ao trabalhador dos valores informados neste evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstab2500")]
    [ComVisible(true)]
#endif
    public class IdeEstab2500
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição do estabelecimento, de acordo com as opções da Tabela 05
        /// </summary>
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do estabelecimento do contribuinte de acordo com o tipo de inscrição indicado no campo acima
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Informações dos períodos e valores decorrentes de processo trabalhista
        /// </summary>
        [XmlElement("infoVlr")]
        public InfoVlr InfoVlr { get; set; }
    }

    /// <summary>
    /// Informações dos períodos e valores decorrentes de processo trabalhista
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoVlr")]
    [ComVisible(true)]
#endif
    public class InfoVlr
    {
        /// <summary>
        /// Competência inicial a que se refere o processo ou conciliação, no formato AAAA-MM
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime CompIni { get; set; }
#else
        public DateTimeOffset CompIni { get; set; }
#endif

        [XmlElement("compIni")]
        public string CompIniField
        {
            get => CompIni.ToString("yyyy-MM");
#if INTEROP
            set => CompIni = DateTime.Parse(value);
#else
            set => CompIni = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Competência final a que se refere o processo ou conciliação, no formato AAAA-MM
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime CompFim { get; set; }
#else
        public DateTimeOffset CompFim { get; set; }
#endif

        [XmlElement("compFim")]
        public string CompFimField
        {
            get => CompFim.ToString("yyyy-MM");
#if INTEROP
            set => CompFim = DateTime.Parse(value);
#else
            set => CompFim = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Indicativo de repercussão do processo trabalhista ou de demanda submetida à CCP ou ao NINTER
        /// </summary>
        [XmlElement("indReperc")]
        public IndReperc IndReperc { get; set; }

        /// <summary>
        /// Houve decisão para pagamento da indenização substitutiva do seguro-desemprego?
        /// </summary>
        [XmlElement("indenSD")]
#if INTEROP
        public SimNaoLetra IndenSD { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndenSD { get; set; }
#endif

        /// <summary>
        /// Houve decisão para pagamento da indenização substitutiva de abono salarial?
        /// </summary>
        [XmlElement("indenAbono")]
#if INTEROP
        public SimNaoLetra IndenAbono { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndenAbono { get; set; }
#endif

        /// <summary>
        /// Identificação do(s) ano(s)-base em que houve indenização substitutiva de abono salarial
        /// </summary>
        [XmlElement("abono")]
        public List<Abono> Abono { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddAbono(Abono item)
        {
            if (Abono == null)
            {
                Abono = new List<Abono>();
            }

            Abono.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Abono (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Abono</returns>
        public Abono GetAbono(int index)
        {
            if ((Abono?.Count ?? 0) == 0)
            {
                return default;
            };

            return Abono[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Abono
        /// </summary>
        public int GetAbonoCount => (Abono != null ? Abono.Count : 0);
#endif

        /// <summary>
        /// Identificação do período ao qual se referem as bases de cálculo
        /// </summary>
        [XmlElement("idePeriodo")]
        public List<IdePeriodo2500> IdePeriodo { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdePeriodo(IdePeriodo2500 item)
        {
            if (IdePeriodo == null)
            {
                IdePeriodo = new List<IdePeriodo2500>();
            }

            IdePeriodo.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdePeriodo (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdePeriodo</returns>
        public IdePeriodo2500 GetIdePeriodo(int index)
        {
            if ((IdePeriodo?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdePeriodo[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdePeriodo
        /// </summary>
        public int GetIdePeriodoCount => (IdePeriodo != null ? IdePeriodo.Count : 0);
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndenSD() => IndenSD != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndenSD() => IndenSD != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndenAbono() => IndenAbono != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndenAbono() => IndenAbono != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Identificação do(s) ano(s)-base em que houve indenização substitutiva de abono salarial
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Abono")]
    [ComVisible(true)]
#endif
    public class Abono
    {
        /// <summary>
        /// Ano-base em que houve indenização substitutiva do abono salarial
        /// </summary>
        [XmlElement("anoBase")]
        public string AnoBase { get; set; }
    }

    /// <summary>
    /// Identificação do período ao qual se referem as bases de cálculo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdePeriodo2500")]
    [ComVisible(true)]
#endif
    public class IdePeriodo2500
    {
        /// <summary>
        /// Informar o mês/ano (formato AAAA-MM) de referência das informações
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime PerRef { get; set; }
#else
        public DateTimeOffset PerRef { get; set; }
#endif

        [XmlElement("perRef")]
        public string PerRefField
        {
            get => PerRef.ToString("yyyy-MM");
#if INTEROP
            set => PerRef = DateTime.Parse(value);
#else
            set => PerRef = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Bases de cálculo de contribuição previdenciária decorrentes de processo trabalhista e ainda não declaradas
        /// </summary>
        [XmlElement("baseCalculo")]
        public BaseCalculo BaseCalculo { get; set; }

        /// <summary>
        /// Informações referentes a bases de cálculo de FGTS (valores históricos, sem atualização), inclusive valores de 13º 
        /// salário, aviso prévio indenizado e seu reflexo sobre o 13º salário, para geração de guia no FGTS Digital
        /// </summary>
        [XmlElement("infoFGTS")]
        public InfoFGTS InfoFGTS { get; set; }

        /// <summary>
        /// Bases de cálculo de contribuição previdenciária já declaradas anteriormente em GFIP ou no evento S-1200 (exclusivamente 
        /// para remuneração de trabalhador sem cadastro no S-2300), no caso de reconhecimento de mudança de código de categoria
        /// </summary>
        [XmlElement("baseMudCateg")]
        public BaseMudCateg BaseMudCateg { get; set; }

        /// <summary>
        /// Informações relativas ao trabalho intermitente.
        /// </summary>
        [XmlElement("infoInterm")]
        public string InfoInterm { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeInfoInterm() => !string.IsNullOrWhiteSpace(InfoInterm);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações referentes a bases de cálculo de FGTS (valores históricos, sem atualização), inclusive valores de 13º salário, 
    /// aviso prévio indenizado e seu reflexo sobre o 13º salário, para geração de guia no FGTS Digital
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BaseCalculo")]
    [ComVisible(true)]
#endif
    public class BaseCalculo
    {
        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária
        /// sobre a remuneração mensal do trabalhador.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrBcCpMensal { get; set; }

        [XmlElement("vrBcCpMensal")]
        public string VrBcCpMensalField
        {
            get => VrBcCpMensal.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCpMensal = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária
        /// sobre a remuneração do trabalhador referente ao 13º salário.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrBcCp13 { get; set; }
        [XmlElement("vrBcCp13")]

        public string VrBcCp13Field
        {
            get => VrBcCp13.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp13 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo referente ao detalhamento do grau de exposição do trabalhador aos agentes nocivos que ensejam a 
        /// cobrança da contribuição adicional para financiamento dos benefícios de aposentadoria especial
        /// </summary>
        [XmlElement("infoAgNocivo")]
        public InfoAgNocivo2500 InfoAgNocivo { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVrBcCp13() => VrBcCp13 > 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Grupo referente ao detalhamento do grau de exposição do trabalhador aos agentes nocivos que ensejam a 
    /// cobrança da contribuição adicional para financiamento dos benefícios de aposentadoria especials
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAgNocivo2500")]
    [ComVisible(true)]
#endif
    public class InfoAgNocivo2500 : InfoAgNocivo1200 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoFGTS")]
    [ComVisible(true)]
#endif
    public class InfoFGTS
    {
        /// <summary>
        /// Valor da base de cálculo de FGTS ainda não declarada em SEFIP ou no
        /// eSocial, inclusive de verba reconhecida no processo trabalhista.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrBcFGTSProcTrab { get; set; }

        [XmlElement("vrBcFGTSProcTrab")]
        public string VrBcFGTSProcTrabField
        {
            get => VrBcFGTSProcTrab.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcFGTSProcTrab = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo de FGTS declarada apenas em SEFIP
        /// (não informada no eSocial) e ainda não recolhida.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrBcFGTSSefip { get; set; }

        [XmlElement("vrBcFGTSSefip")]
        public string VrBcFGTSSefipField
        {
            get => VrBcFGTSSefip.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcFGTSSefip = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo de FGTS declarada anteriormente no eSocial e ainda não recolhida.
        /// Validação: Somente pode ser informado se perRef for anterior ao início do FGTS Digital.
        /// Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrBcFGTSDecAnt { get; set; }

        [XmlElement("vrBcFGTSDecAnt")]
        public string VrBcFGTSDeAntField
        {
            get => VrBcFGTSDecAnt.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcFGTSDecAnt = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVrBcFGTSSefipField() => VrBcFGTSSefip > 0;

        public bool ShouldSerializeVrBcFGTSDecAntField() => VrBcFGTSDecAnt > 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Bases de cálculo de contribuição previdenciária já declaradas anteriormente em GFIP ou no evento S-1200 (exclusivamente para 
    /// remuneração de trabalhador sem cadastro no S-2300), no caso de reconhecimento de mudança de código de categoria.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BaseMudCateg")]
    [ComVisible(true)]
#endif
    public class BaseMudCateg
    {
        /// <summary>
        /// Preencher com o código da categoria do trabalhador declarado no período de referência
        /// </summary>
        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        /// <summary>
        /// Valor da remuneração do trabalhador a ser considerada para fins previdenciários
        /// declarada em GFIP ou em S-1200 de trabalhador sem cadastro no S-2300.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrBcCPrev { get; set; }

        [XmlElement("vrBcCPrev")]
        public string VrBcCPrevField
        {
            get => VrBcCPrev.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCPrev = Converter.ToDouble(value);
        }
    }

}
