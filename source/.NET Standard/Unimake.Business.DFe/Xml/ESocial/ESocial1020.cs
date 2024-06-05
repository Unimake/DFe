#pragma warning disable CS1591
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1020")]
    [ComVisible(true)]
#endif

    /// <summary>
    /// Evento Tabela de Lotações Tributárias
    /// </summary>
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTabLotacao/v_S_01_02_00", IsNullable = false)]
    public class ESocial1020 : XMLBase
    {
        /// <summary>
        /// Evento Tabela de Lotações Tributárias
        /// </summary>
        [XmlElement("evtTabLotacao")]
        public EvtTabLotacao evtTabLotacao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtTabLotacao")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Evento Tabela de Lotações Tributárias
    /// </summary>
    public class EvtTabLotacao
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
        public IdeEvento IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações da lotação 
        /// </summary>
        [XmlElement("infoLotacao")]
        public InfoLotacao InfoLotacao { get; set; }


    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoLotacao")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações da lotação 
    /// </summary>
    public class InfoLotacao
    {
        /// <summary>
        /// Inclusão de novas informações
        /// </summary>
        [XmlElement("inclusao")]
        public InclusaoE1020 Inclusao { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InclusaoE1020")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Inclusão de novas informações
    /// </summary>
    public class InclusaoE1020
    {
        /// <summary>
        /// Identificação da lotação e validade das informações
        /// </summary>
        [XmlElement("ideLotacao")]
        public IdeLotacao IdeLotacao { get; set; }

        /// <summary>
        /// Detalhamento das informações da lotação.
        /// </summary>
        [XmlElement("dadosLotacao")]
        public DadosLotacao DadosLotacao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeLotacao")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação da lotação e validade das informações
    /// </summary>
    public class IdeLotacao
    {
        /// <summary>
        /// Informar o código atribuído pelo empregador para a lotação tributária. Validação: O código atribuído não pode conter a expressão 'eSocial' nas 7 primeiras posições.
        /// </summary>
        [XmlElement("codLotacao")]
        public string CodLotacao { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime IniValid {get; set; }
#else
        /// <summary>
        /// Preencher com o mês e ano de início da validade das informações prestadas no evento, no formato AAAAMM
        /// </summary>
        public DateTimeOffset IniValid { get; set; }
#endif

        /// <summary>
        /// Preencher com o mês e ano de início da validade das informações prestadas no evento, no formato AAAAMM
        /// </summary>
        [XmlElement("iniValid")]
        public string IniValidField
        {
            get => IniValid.ToString("yyyy-MM");
#if INTEROP
            set => IniValid = DateTime.Parse(value);
#else
            set => IniValid = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime FimValid {get; set; }

#else
        /// <summary>
        /// Preencher com o mês e ano de término da validade das informações, se houver.
        /// </summary>
        public DateTimeOffset FimValid { get; set; }
#endif
        /// <summary>
        /// Preencher com o mês e ano de término da validade das informações, se houver.
        /// </summary>
        [XmlElement("fimValid")]
        public string FimValidField
        {
            get => FimValid.ToString("yyyy-MM");
#if INTEROP
            set => FimValid = DateTime.Parse(value);
#else
            set => FimValid = DateTimeOffset.Parse(value);
#endif
        }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosLotacao")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Detalhamento das informações da lotação.
    /// </summary>
    public class DadosLotacao
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de lotação.
        /// </summary>
        [XmlElement("tpLotacao")]
        public TpLotacao TpLotacao { get; set; }

        /// <summary>
        /// Preencher com o número de inscrição (CNPJ, CPF, CNO) ao qual pertence a lotação tributária.
        /// </summary>
        [XmlElement("tpInsc")]
        public TpInsc TpInsc { get; set; }

        /// <summary>
        /// Preencher com o número de inscrição (CNPJ, CPF, CNO) ao qual pertence a lotação tributária.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Informações de FPAS e Terceiros relativos à lotação tributária.
        /// </summary>
        [XmlElement("fpasLotacao")]
        public FpasLotacao FpasLotacao { get; set; }

        /// <summary>
        /// Informação complementar que apresenta identificação
        /// do contratante de obra de construção civil sob regime
        /// de empreitada parcial ou subempreitada.
        /// </summary>
        [XmlElement("infoEmprParcial")]
        public InfoEmprParcial InfoEmprParcial { get; set; }

        /// <summary>
        /// Informações do operador portuário.
        /// </summary>
        [XmlElement("dadosOpPort")]
        public DadosOpPort DadosOpPort { get; set; }
    }

    #region FpasLotacao

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.FpasLotacao")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de FPAS e Terceiros relativos à lotação tributária.
    /// </summary>
    public class FpasLotacao
    {
        /// <summary>
        /// Preencher com o código relativo ao FPAS. Validação: Deve ser um código FPAS válido, 
        /// conforme Tabela 04.
        /// </summary>
        [XmlElement("fpas")]
        public string Fpas { get; set; }

        /// <summary>
        /// Preencher com o código de Terceiros, já considerando a
        /// existência de eventuais convênios para recolhimento
        /// direto. Ex.: Se o contribuinte está enquadrado com FPAS
        /// [507], cujo código cheio de Terceiros é [0079], se possuir
        /// convênio com SENAI deve informar o código [0075].
        /// Validação: Se a classificação tributária em S-1000 for
        /// igual a[01, 02, 03, 04], informar[0000]. Nos demais
        /// casos, o código de Terceiros informado deve ser
        /// compatível com o código de FPAS informado, conforme
        /// Tabela 04.
        /// </summary>
        [XmlElement("codTercs")]
        public string CodTercs { get; set; }

        /// <summary>
        /// Informar o código combinado dos Terceiros para os
        /// quais o recolhimento está suspenso em virtude de
        /// processos judiciais.Ex.: Se o contribuinte possui
        /// decisões de processos para suspensão de recolhimentos
        /// ao SESI (0008) e ao SEBRAE(0064), deve informar o
        /// código combinado das duas entidades, ou seja, [0072].
        /// Validação: Deve ser um código consistente com a
        /// Tabela 04.
        /// Deve haver pelo menos um processo em
        /// procJudTerceiro para cada código de Terceiro cujo
        /// recolhimento esteja suspenso.
        ///  </summary>
        [XmlElement("codTercsSusp")]
        public string CodTercsSusp { get; set; }

        /// <summary>
        /// Informações sobre a existência de processos judiciais,
        /// com sentença/decisão favorável ao contribuinte,
        /// relativos às contribuições destinadas a Outras Entidades
        /// e Fundos.
        /// </summary>
        [XmlElement("infoProcJudTerceiros")]
        public InfoProcJudTerceiro InfoProcJudTerceiro { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcJudTerceiro")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações sobre a existência de processos judiciais,
    /// com sentença/decisão favorável ao contribuinte,
    /// relativos às contribuições destinadas a Outras Entidades
    /// e Fundos.
    /// </summary>
    public class InfoProcJudTerceiro
    {
        /// <summary>
        /// Identificação do processo judicial.
        /// </summary>
        [XmlElement("procJudTerceiro")]
        public ProcJudTerceiro ProcJudTerceiro { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ProcJudTerceiro")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação do processo judicial.
    /// </summary>
    public class ProcJudTerceiro
    {
        /// <summary>
        /// Informar o código de Terceiro.
        /// Validação: Deve ser um código de Terceiro válido e
        /// compatível com o FPAS/Terceiros informado no grupo
        /// superior, conforme Tabela 04.
        /// </summary>
        [XmlElement("codTerc")]
        public string CodTerc { get; set; }

        /// <summary>
        /// Informar um número de processo judicial cadastrado
        /// através do evento S-1070, cujo indMatProc seja igual a [1].
        /// Validação: Deve ser um número de processo judicial
        /// válido e existente na Tabela de Processos(S-1070), com
        /// indMatProc = [1].
        /// </summary>
        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }

        /// <summary>
        /// Código do indicativo da suspensão, atribuído pelo
        /// empregador em S-1070.
        /// Validação: A informação prestada deve estar de acordo
        /// com o que foi informado em S-1070.
        /// </summary>
        [XmlElement("codSusp")]
        public string CodSusp { get; set; }
    }

    #endregion FpasLotacao

    #region InfoEmprParcial

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEmprParcial")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informação complementar que apresenta identificação
    /// do contratante de obra de construção civil sob regime
    /// de empreitada parcial ou subempreitada.
    /// </summary>
    public class InfoEmprParcial
    {
        /// <summary>
        /// Tipo de inscrição do contratante.
        ///  Valores válidos:
        /// 1 - CNPJ
        /// 2 - CPF
        /// </summary>
        [XmlElement("tpInscContrat")]
        public TpInsc TpInscContrat { get; set; }

        /// <summary>
        /// Número de inscrição (CNPJ/CPF) do contratante.
        /// Validação: Deve ser um número de CNPJ ou CPF válido,
        /// conforme definido em tpInscContrat
        /// </summary>
        [XmlElement("nrInscContrat")]
        public string NrInscContrat { get; set; }

        /// <summary>
        /// Tipo de inscrição do proprietário do CNO.
        ///  Valores válidos:
        /// 1 - CNPJ
        /// 2 - CPF
        /// Validação: Preenchimento obrigatório e exclusivo
        /// quando o proprietário não for encontrado no CNO.
        /// </summary>
        [XmlElement("tpInscProp")]
        public TpInsc TpInscProp { get; set; }

        /// <summary>
        /// Preencher com o número de inscrição (CNPJ/CPF) do proprietário do CNO
        /// Validação: Preenchimento obrigatório e exclusivo se
        /// tpInscProp for informado.Deve ser um número de CNPJ
        /// ou CPF válido, conforme indicado em tpInscProp.
        /// </summary>
        [XmlElement("nrInscProp")]
        public string NrInscProp { get; set; }
    }

    #endregion InfoEmprParcial

    #region DadosOpPort

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosOpPort")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações do operador portuário.
    /// </summary>
    public class DadosOpPort
    {
        /// <summary>
        /// Preencher com a alíquota definida na legislação vigente
        /// para a atividade(CNAE) preponderante.
        /// Valores válidos: 1, 2, 3
        /// </summary>
        [XmlElement("aliqRat")]
        public string AliqRat { get; set; }

        /// <summary>
        /// Fator Acidentário de Prevenção - FAP.
        /// Validação: Deve ser um número maior ou igual a 0,5000
        /// e menor ou igual a 2,0000, de acordo com o
        /// estabelecido pelo órgão governamental competente.
        /// </summary>
        [XmlElement("fap")]
        public double Fap { get; set; }
    }
    #endregion DadosOpPort

}
