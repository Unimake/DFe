#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Collections.Generic;
using System.Globalization;
using Unimake.Business.DFe.Utility;


namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-1005 - Tabela de Estabelecimentos, Obras ou Unidades de Órgãos Públicos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.ESocial1005")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTabEstab/v_S_01_03_00", IsNullable = false)]
    public class ESocial1005 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Tabela de Estabelecimentos
        /// </summary>
        [XmlElement("evtTabEstab")]
        public EvtTabEstab EvtTabEstab { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Tabela de Estabelecimentos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtTabEstab")]
    [ComVisible(true)]
#endif
    public class EvtTabEstab
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
        public IdeEvento1005 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações do estabelecimento
        /// </summary>
        [XmlElement("infoEstab")]
        public InfoEstab InfoEstab { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1005")]
    [ComVisible(true)]
#endif
    public class IdeEvento1005 : IdeEvento { }

    /// <summary>
    /// Informações do estabelecimento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEstab")]
    [ComVisible(true)]
#endif
    public class InfoEstab
    {
        /// <summary>
        /// Inclusão de novas informações
        /// </summary>
        [XmlElement("inclusao")]
        public Inclusao1005 Inclusao { get; set; }

        /// <summary>
        /// Alteração das informações
        /// </summary>
        [XmlElement("alteracao")]
        public Alteracao1005 Alteracao { get; set; }

        /// <summary>
        /// Exclusão das informações
        /// </summary>
        [XmlElement("exclusao")]
        public Exclusao1005 Exclusao { get; set; }
    }

    /// <summary>
    /// Inclusão de novas informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Inclusao1005")]
    [ComVisible(true)]
#endif
    public class Inclusao1005
    {
        /// <summary>
        /// Identificação do estabelecimento e validade das informações
        /// </summary>
        [XmlElement("ideEstab")]
        public IdeEstab IdeEstab { get; set; }

        /// <summary>
        /// Detalhamento das informações do estabelecimento
        /// </summary>
        [XmlElement("dadosEstab")]
        public DadosEstab DadosEstab { get; set; }
    }

    /// <summary>
    /// Identificação do estabelecimento e validade das informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstab")]
    [ComVisible(true)]
#endif
    public class IdeEstab
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 05
        /// </summary>
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do estabelecimento (inclusive Sociedade em Conta de Participação - SCP), 
        /// obra de construção civil ou órgão público de acordo com o tipo de inscrição indicado no campo ideEstab/tpInsc.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Preencher com o mês e ano de início da validade das informações prestadas no evento, no formato AAAA-MM
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime IniValid { get; set; }
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
        /// Preencher com o mês e ano de término da validade das informações, se houver
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime FimValid { get; set; }
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
    /// Detalhamento das informações do estabelecimento, obra de construção civil ou unidade de órgão público
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosEstab")]
    [ComVisible(true)]
#endif
    public class DadosEstab
    {
        /// <summary>
        /// Preencher com o código CNAE conforme legislação vigente, referente à atividade econômica preponderante do estabelecimento
        /// </summary>
        [XmlElement("cnaePrep")]
        public string CnaePrep { get; set; }

        /// <summary>
        /// Preencher com o CNPJ responsável pela inscrição no cadastro de obras da RFB
        /// </summary>
        [XmlElement("cnpjResp")]
        public string CnpjResp { get; set; }

        /// <summary>
        /// Informações de apuração da alíquota GILRAT do estabelecimento
        /// </summary>
        [XmlElement("aliqGilrat")]
        public AliqGilrat AliqGilrat { get; set; }

        /// <summary>
        /// Informações relativas ao CAEPF
        /// </summary>
        [XmlElement("infoCaepf")]
        public InfoCaepf InfoCaepf { get; set; }

        /// <summary>
        /// Indicativo de substituição da contribuição patronal - Obra de construção civil
        /// </summary>
        [XmlElement("infoObra")]
        public InfoObra InfoObra { get; set; }

        /// <summary>
        ///Informações trabalhistas
        /// </summary>
        [XmlElement("infoTrab")]
        public InfoTrab InfoTrab { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCnpjResp() => !string.IsNullOrEmpty(CnpjResp);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações de apuração da alíquota GILRAT do estabelecimento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.AliqGilrat")]
    [ComVisible(true)]
#endif
    public class AliqGilrat
    {
        /// <summary>
        /// Informar a alíquota RAT, quando divergente da legislação vigente para a atividade (CNAE) preponderante. 
        /// A divergência só é permitida se existir o grupo com informações sobre o processo administrativo/judicial que permite a aplicação de alíquota diferente.
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

        [XmlElement("fap")]
        public string FapField
        {
            get => Fap.ToString("F4", CultureInfo.InvariantCulture);
            set => Fap = Converter.ToDouble(value);
        }

        /// <summary>
        /// Processo administrativo/judicial relativo à alíquota RAT
        /// </summary>
        [XmlElement("procAdmJudRat")]
        public ProcAdmJudRat ProcAdmJudRat { get; set; }

        /// <summary>
        /// Processo administrativo/judicial relativo ao FAP
        /// </summary>
        [XmlElement("procAdmJudFap")]
        public ProcAdmJudFap ProcAdmJudFap { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeAliqRat() => !string.IsNullOrEmpty(AliqRat);

        public bool ShouldSerializeFapField() => !Fap.IsNullOrEmpty();

        #endregion
    }

    /// <summary>
    /// Grupo que identifica, em caso de existência, o processo administrativo ou judicial em que houve decisão/sentença favorável ao contribuinte modificando a alíquota RAT da empresa.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ProcAdmJudRat")]
    [ComVisible(true)]
#endif
    public class ProcAdmJudRat
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de processo
        /// </summary>
        [XmlElement("tpProc")]
        public TipoProcesso TpProc { get; set; }

        /// <summary>
        /// Informar um número de processo cadastrado através do evento S-1070, cujo indMatProc seja igual a [1]
        /// </summary>
        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        /// <summary>
        /// Código do indicativo da suspensão, atribuído pelo empregador em S-1070
        /// </summary>
        [XmlElement("codSusp")]
        public string CodSusp { get; set; }
    }

    /// <summary>
    /// Grupo que identifica, em caso de existência, o processo administrativo/judicial em que houve decisão ou sentença favorável ao contribuinte suspendendo ou alterando a alíquota FAP aplicável ao contribuinte.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ProcAdmJudFap")]
    [ComVisible(true)]
#endif
    public class ProcAdmJudFap
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de processo
        /// </summary>
        [XmlElement("tpProc")]
        public TipoProcessoESocial TpProc { get; set; }

        /// <summary>
        /// Informar um número de processo cadastrado através do evento S-1070, cujo indMatProc seja igual a [1]
        /// </summary>
        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        /// <summary>
        /// Código do indicativo da suspensão, atribuído pelo empregador em S-1070
        /// </summary>
        [XmlElement("codSusp")]
        public string CodSusp { get; set; }
    }

    /// <summary>
    /// Informações relativas ao Cadastro de Atividade Econômica da Pessoa Física - CAEPF
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCaepf")]
    [ComVisible(true)]
#endif
    public class InfoCaepf
    {
        /// <summary>
        /// Tipo de CAEPF
        /// </summary>
        [XmlElement("tpCaepf")]
        public TipoCaepf TpCaepf { get; set; }
    }

    /// <summary>
    /// Grupo preenchido obrigatória e exclusivamente por empresa construtora, 
    /// relacionando os estabelecimentos inscritos no Cadastro Nacional de Obras - CNO, 
    /// para indicar a substituição ou não da contribuição patronal incidente sobre a remuneração 
    /// dos trabalhadores de obra de construção civil
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoObra")]
    [ComVisible(true)]
#endif
    public class InfoObra
    {
        /// <summary>
        /// Indicativo de substituição da contribuição patronal de obra de construção civil
        /// </summary>
        [XmlElement("indSubstPatrObra")]
        public IndicativoSubstituicaoPatronal IndSubstPatrObra { get; set; }
    }

    /// <summary>
    /// Informações trabalhistas relativas ao estabelecimento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTrab")]
    [ComVisible(true)]
#endif
    public class InfoTrab
    {
        /// <summary>
        /// Informações relacionadas à contratação de aprendiz
        /// </summary>
        [XmlElement("infoApr")]
        public InfoApr InfoApr { get; set; }

        /// <summary>
        /// Informações sobre a contratação de PCD
        /// </summary>
        [XmlElement("infoPCD")]
        public InfoPCD InfoPCD { get; set; }
    }

    /// <summary>
    /// Informações relacionadas à contratação de aprendiz.
    /// Preenchimento obrigatório somente no caso de dispensa, ainda que parcial, de contratação de aprendiz em virtude de processo judicial ou quando houver contratação de aprendiz por meio de entidade educativa ou de prática desportiva.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoApr")]
    [ComVisible(true)]
#endif
    public class InfoApr
    {
        /// <summary>
        /// Preencher com o número do processo judicial
        /// </summary>
        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }

        /// <summary>
        /// Identificação da(s) entidade(s) educativa(s) ou de prática desportiva
        /// </summary>
        [XmlElement("infoEntEduc")]
        public List<InfoEntEduc> InfoEntEduc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoEntEduc(InfoEntEduc item)
        {
            if (InfoEntEduc == null)
            {
                InfoEntEduc = new List<InfoEntEduc>();
            }

            InfoEntEduc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoEntEduc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoEntEduc</returns>
        public InfoEntEduc GetInfoEntEduc(int index)
        {
            if ((InfoEntEduc?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoEntEduc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoEntEduc
        /// </summary>
        public int GetInfoEntEducCount => (InfoEntEduc != null ? InfoEntEduc.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeNrProcJud() => !string.IsNullOrEmpty(NrProcJud);

        #endregion
    }

    /// <summary>
    /// Identificação da(s) entidade(s) educativa(s) ou de prática desportiva
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEntEduc")]
    [ComVisible(true)]
#endif
    public class InfoEntEduc
    {
        /// <summary>
        /// Informar o número de inscrição da entidade educativa ou de prática desportiva
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

    /// <summary>
    /// Informações sobre a contratação de pessoa com deficiência (PCD)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPCD")]
    [ComVisible(true)]
#endif
    public class InfoPCD
    {
        /// <summary>
        /// Preencher com o número do processo 
        /// </summary>
        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }
    }

    /// <summary>
    /// Alteração das informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Alteracao1005")]
    [ComVisible(true)]
#endif
    public class Alteracao1005
    {
        /// <summary>
        /// Identificação do estabelecimento e validade das informações
        /// </summary>
        [XmlElement("ideEstab")]
        public IdeEstab IdeEstab { get; set; }

        /// <summary>
        /// Detalhamento das informações do estabelecimento
        /// </summary>
        [XmlElement("dadosEstab")]
        public DadosEstab DadosEstab { get; set; }

        /// <summary>
        /// Novo período de validade das informações
        /// </summary>
        [XmlElement("novaValidade")]
        public NovaValidade1005 NovaValidade { get; set; }
    }

    /// <summary>
    /// Informação preenchida exclusivamente em caso de alteração do período de validade das informações, apresentando o novo período de validade
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.NovaValidade1005")]
    [ComVisible(true)]
#endif
    public class NovaValidade1005
    {
        /// <summary>
        /// Preencher com o mês e ano de início da validade das informações prestadas no evento, no formato AAAA-MM
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime IniValid { get; set; }
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
        /// Preencher com o mês e ano de término da validade das informações, se houver
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime FimValid { get; set; }
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

        #endregion
    }

    /// <summary>
    /// Exclusão das informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Exclusao1005")]
    [ComVisible(true)]
#endif
    public class Exclusao1005
    {
        /// <summary>
        /// Identificação do estabelecimento e validade das informações
        /// </summary>
        [XmlElement("ideEstab")]
        public IdeEstab IdeEstab { get; set; }
    }
}
