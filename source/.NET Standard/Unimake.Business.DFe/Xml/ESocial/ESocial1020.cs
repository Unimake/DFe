#pragma warning disable CS1591
using System;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// Evento Tabela de Lotações Tributárias
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1020")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTabLotacao/v_S_01_03_00", IsNullable = false)]
    public class ESocial1020 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Tabela de Lotações Tributárias
        /// </summary>
        [XmlElement("evtTabLotacao")]
        public EvtTabLotacao EvtTabLotacao { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Tabela de Lotações Tributárias
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtTabLotacao")]
    [ComVisible(true)]
#endif
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
        public IdeEvento1020 IdeEvento { get; set; }

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

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1020")]
    [ComVisible(true)]
#endif
    public class IdeEvento1020 : IdeEvento { }

    /// <summary>
    /// Informações da lotação 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoLotacao")]
    [ComVisible(true)]
#endif
    public class InfoLotacao
    {
        /// <summary>
        /// Inclusão de novas informações
        /// </summary>
        [XmlElement("inclusao")]
        public Inclusao1020 Inclusao { get; set; }

        /// <summary>
        /// Alteração das informações
        /// </summary>
        [XmlElement("alteracao")]
        public Alteracao1020 Alteracao { get; set; }

        /// <summary>
        /// Exclusão das informações
        /// </summary>
        [XmlElement("exclusao")]
        public Exclusao1020 Exclusao { get; set; }

    }

    /// <summary>
    /// Inclusão de novas informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Inclusao1020")]
    [ComVisible(true)]
#endif
    public class Inclusao1020
    {
        /// <summary>
        /// Identificação da lotação e validade das informações
        /// </summary>
        [XmlElement("ideLotacao")]
        public IdeLotacao IdeLotacao { get; set; }

        /// <summary>
        /// Detalhamento das informações da lotação
        /// </summary>
        [XmlElement("dadosLotacao")]
        public DadosLotacao DadosLotacao { get; set; }
    }

    /// <summary>
    /// Identificação da lotação e validade das informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeLotacao")]
    [ComVisible(true)]
#endif
    public class IdeLotacao
    {
        /// <summary>
        /// Informar o código atribuído pelo empregador para a lotação tributária. Validação: O código atribuído não pode conter a expressão 'eSocial' nas 7 primeiras posições.
        /// </summary>
        [XmlElement("codLotacao")]
        public string CodLotacao { get; set; }

        /// <summary>
        /// Preencher com o mês e ano de início da validade das informações prestadas no evento, no formato AAAAMM
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime IniValid {get; set; }
#else
        public DateTimeOffset IniValid { get; set; }
#endif

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

        /// <summary>
        /// Preencher com o mês e ano de término da validade das informações, se houver.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime FimValid {get; set; }
#else
        public DateTimeOffset FimValid { get; set; }
#endif

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

        #region ShouldSerialize
        public bool ShouldSerializeFimValidField() => FimValid > DateTime.MinValue;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Detalhamento das informações da lotação.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosLotacao")]
    [ComVisible(true)]
#endif
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
#if INTEROP
        public TpInsc TpInsc { get; set; } = (TpInsc)(-1);
#else
        public TpInsc? TpInsc { get; set; }
#endif

        /// <summary>
        /// Preencher com o número de inscrição (CNPJ, CPF, CNO) ao qual pertence a lotação tributária.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Informações de FPAS e Terceiros relativos à lotação tributária
        /// </summary>
        [XmlElement("fpasLotacao")]
        public FpasLotacao FpasLotacao { get; set; }

        /// <summary>
        /// Informação complementar de obra de construção civil
        /// </summary>
        [XmlElement("infoEmprParcial")]
        public InfoEmprParcial InfoEmprParcial { get; set; }

        /// <summary>
        /// Informações do operador portuário
        /// </summary>
        [XmlElement("dadosOpPort")]
        public DadosOpPort DadosOpPort { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeTpInsc() => TpInsc != (TpInsc)(-1);
#else
        public bool ShouldSerializeTpInsc() => TpInsc != null;
#endif

        public bool ShouldSerializeNrInsc() => !string.IsNullOrEmpty(NrInsc);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações de FPAS e Terceiros relativos à lotação tributária.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.FpasLotacao")]
    [ComVisible(true)]
#endif
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
        /// Informações de processos judiciais relativos às contribuições destinadas a Outras Entidades
        /// </summary>
        [XmlElement("infoProcJudTerceiros")]
        public InfoProcJudTerceiro InfoProcJudTerceiro { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeCodTercsSusp() => !string.IsNullOrEmpty(CodTercsSusp);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações sobre a existência de processos judiciais,
    /// com sentença/decisão favorável ao contribuinte,
    /// relativos às contribuições destinadas a Outras Entidades
    /// e Fundos.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcJudTerceiro")]
    [ComVisible(true)]
#endif
    public class InfoProcJudTerceiro
    {
        /// <summary>
        /// Identificação do processo judicial
        /// </summary>
        [XmlElement("procJudTerceiro")]
        public ProcJudTerceiro ProcJudTerceiro { get; set; }
    }

    /// <summary>
    /// Identificação do processo judicial.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ProcJudTerceiro")]
    [ComVisible(true)]
#endif
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

    /// <summary>
    /// Informação complementar que apresenta identificação
    /// do contratante de obra de construção civil sob regime
    /// de empreitada parcial ou subempreitada.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEmprParcial")]
    [ComVisible(true)]
#endif
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
#if INTEROP
        public TpInsc TpInscProp { get; set; } = (TpInsc)(-1);
#else
        public TpInsc? TpInscProp { get; set; }
#endif

        /// <summary>
        /// Preencher com o número de inscrição (CNPJ/CPF) do proprietário do CNO
        /// Validação: Preenchimento obrigatório e exclusivo se
        /// tpInscProp for informado.Deve ser um número de CNPJ
        /// ou CPF válido, conforme indicado em tpInscProp.
        /// </summary>
        [XmlElement("nrInscProp")]
        public string NrInscProp { get; set; }

        #region ShouldSerialize
#if INTEROP
        public bool ShouldSerializeTpInscProp() => TpInscProp != (TpInsc)(-1);
#else
        public bool ShouldSerializeTpInscProp() => TpInscProp != null;
#endif
        public bool ShouldSerializeNrInscProp() => !string.IsNullOrEmpty(NrInscProp);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações do operador portuário.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosOpPort")]
    [ComVisible(true)]
#endif
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
        /// Validação: Preenchimento obrigatório e exclusivo por
        /// Pessoa Jurídica e:
        /// a) ideEstab/tpInsc = [4] e o campo cnpjResp não estiver
        /// informado; ou
        /// b) ideEstab/tpInsc = [1, 4] e o fator informado for diferente
        /// do definido pelo órgão governamental competente para o
        /// estabelecimento ou para o CNPJ responsável pela inscrição
        /// no CNO(neste caso, deverá haver informações de processo
        /// em procAdmJudFap); ou c) ideEstab/tpInsc = [1, 4] e o estabelecimento ou o CNPJ
        /// responsável pela inscrição no CNO não for encontrado na tabela FAP.
        /// Se informado, deve ser um número maior ou igual a 0,5000
        /// e menor ou igual a 2,0000 e, no caso da alínea "b", deve ser
        /// diferente do valor definido pelo órgão governamental competente.
        /// </summary>
        [XmlIgnore]
        public double Fap { get; set; }

        /// <summary>
        /// Fator Acidentário de Prevenção - FAP.
        /// Validação: Preenchimento obrigatório e exclusivo por
        /// Pessoa Jurídica e:
        /// a) ideEstab/tpInsc = [4] e o campo cnpjResp não estiver
        /// informado; ou
        /// b) ideEstab/tpInsc = [1, 4] e o fator informado for diferente
        /// do definido pelo órgão governamental competente para o
        /// estabelecimento ou para o CNPJ responsável pela inscrição
        /// no CNO(neste caso, deverá haver informações de processo
        /// em procAdmJudFap); ou c) ideEstab/tpInsc = [1, 4] e o estabelecimento ou o CNPJ
        /// responsável pela inscrição no CNO não for encontrado na tabela FAP.
        /// Se informado, deve ser um número maior ou igual a 0,5000
        /// e menor ou igual a 2,0000 e, no caso da alínea "b", deve ser
        /// diferente do valor definido pelo órgão governamental competente.
        /// </summary>
        [XmlElement("fap")]
        public string FapField
        {
            get => Fap.ToString("F4", CultureInfo.InvariantCulture);
            set => Fap = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Alteração das informações.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Alteracao1020")]
    [ComVisible(true)]
#endif
    public class Alteracao1020
    {
        /// <summary>
        /// Identificação da lotação e validade das informações
        /// </summary>
        [XmlElement("ideLotacao")]
        public IdeLotacao IdeLotacao { get; set; }

        /// <summary>
        /// Detalhamento das informações da lotação
        /// </summary>
        [XmlElement("dadosLotacao")]
        public DadosLotacao DadosLotacao { get; set; }

        /// <summary>
        /// Novo período de validade das informações
        /// </summary>
        [XmlElement("novaValidade")]
        public NovaValidade1020 NovaValidade { get; set; }
    }

    /// <summary>
    /// Informação preenchida exclusivamente em caso de alteração do período de validade das informações, 
    /// apresentando o novo período de validade..
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.NovaValidade1020")]
    [ComVisible(true)]
#endif
    public class NovaValidade1020 : NovaValidade1010 { }

    /// <summary>
    /// Exclusão das informações.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Exclusao1020")]
    [ComVisible(true)]
#endif
    public class Exclusao1020
    {
        /// <summary>
        /// Identificação da lotação e validade das informações
        /// </summary>
        [XmlElement("ideLotacao")]
        public IdeLotacao IdeLotacao { get; set; }
    }
}
