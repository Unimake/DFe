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
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.ESocial1005")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTabEstab/v_S_01_02_00", IsNullable = false)]
    public class ESocial1005 : XMLBase
    {
        [XmlElement("evtTabEstab")]
        public EvtTabEstab EvtTabEstab { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtTabEstab")]
    [ComVisible(true)]
#endif
    public class EvtTabEstab
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEvento1005 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("infoEstab")]
        public InfoEstab InfoEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1005")]
    [ComVisible(true)]
#endif
    public class IdeEvento1005 : IdeEvento { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEstab")]
    [ComVisible(true)]
#endif
    public class InfoEstab
    {
        [XmlElement("inclusao")]
        public Inclusao1005 Inclusao { get; set; }

        [XmlElement("alteracao")]
        public Alteracao1005 Alteracao { get; set; }

        [XmlElement("exclusao")]
        public Exclusao1005 Exclusao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Inclusao1005")]
    [ComVisible(true)]
#endif
    public class Inclusao1005
    {
        [XmlElement("ideEstab")]
        public IdeEstab IdeEstab { get; set; }

        [XmlElement("dadosEstab")]
        public DadosEstab DadosEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstab")]
    [ComVisible(true)]
#endif
    public class IdeEstab
    {
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosEstab")]
    [ComVisible(true)]
#endif
    public class DadosEstab
    {
        [XmlElement("cnaePrep")]
        public string CnaePrep { get; set; }

        [XmlElement("cnpjResp")]
        public string CnpjResp { get; set; }

        /// <summary>
        /// Informações de apuração da alíquota GILRAT do estabelecimento
        /// </summary>
        [XmlElement("aliqGilrat")]
        public AliqGilrat AliqGilrat { get; set; }

        [XmlElement("infoCaepf")]
        public InfoCaepf InfoCaepf { get; set; }

        /// <summary>
        /// Grupo preenchido obrigatória e exclusivamente por empresa construtora, relacionando os estabelecimentos inscritos no Cadastro Nacional de Obras - CNO, para indicar a substituição ou não da contribuição patronal incidente sobre a remuneração dos trabalhadores de obra de construção civil.
        /// </summary>
        [XmlElement("infoObra")]
        public InfoObra InfoObra { get; set; }

        /// <summary>
        /// Informações trabalhistas relativas ao estabelecimento.
        /// </summary>
        [XmlElement("infoTrab")]
        public InfoTrab InfoTrab { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCnpjResp() => !string.IsNullOrEmpty(CnpjResp);

        #endregion ShouldSerialize
    }

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

        /// <summary>
        /// Grupo que identifica, em caso de existência, o processo administrativo ou judicial em que houve decisão/sentença favorável ao contribuinte modificando a alíquota RAT da empresa.
        /// </summary>
        [XmlElement("procAdmJudRat")]
        public ProcAdmJudRat ProcAdmJudRat { get; set; }

        /// <summary>
        /// Grupo que identifica, em caso de existência, o processo administrativo/judicial em que houve decisão ou sentença favorável ao contribuinte suspendendo ou alterando a alíquota FAP aplicável ao contribuinte.
        /// </summary>
        [XmlElement("procAdmJudFap")]
        public ProcAdmJudFap ProcAdmJudFap { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeAliqRat() => !string.IsNullOrEmpty(AliqRat);

        public bool ShouldSerializeFapField() => !Fap.IsNullOrEmpty();

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ProcAdmJudRat")]
    [ComVisible(true)]
#endif
    public class ProcAdmJudRat
    {
        [XmlElement("tpProc")]
        public TipoProcesso TpProc { get; set; }

        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        [XmlElement("codSusp")]
        public string CodSusp { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ProcAdmJudFap")]
    [ComVisible(true)]
#endif
    public class ProcAdmJudFap
    {
        [XmlElement("tpProc")]
        public TipoProcessoESocial TpProc { get; set; }

        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        [XmlElement("codSusp")]
        public string CodSusp { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCaepf")]
    [ComVisible(true)]
#endif
    public class InfoCaepf
    {
        [XmlElement("tpCaepf")]
        public TipoCaepf TpCaepf { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoObra")]
    [ComVisible(true)]
#endif
    public class InfoObra
    {
        [XmlElement("indSubstPatrObra")]
        public IndicativoSubstituicaoPatronal IndSubstPatrObra { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTrab")]
    [ComVisible(true)]
#endif
    public class InfoTrab
    {
        /// <summary>
        /// Informações relacionadas à contratação de aprendiz.
        /// Preenchimento obrigatório somente no caso de dispensa, ainda que parcial, de contratação de aprendiz em virtude de processo judicial ou quando houver contratação de aprendiz por meio de entidade educativa ou de prática desportiva.
        /// </summary>
        [XmlElement("infoApr")]
        public InfoApr InfoApr { get; set; }

        [XmlElement("infoPCD")]
        public InfoPCD InfoPCD { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoApr")]
    [ComVisible(true)]
#endif
    public class InfoApr
    {
        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }

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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEntEduc")]
    [ComVisible(true)]
#endif
    public class InfoEntEduc
    {
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPCD")]
    [ComVisible(true)]
#endif
    public class InfoPCD
    {
        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Alteracao1005")]
    [ComVisible(true)]
#endif
    public class Alteracao1005
    {
        [XmlElement("ideEstab")]
        public IdeEstab IdeEstab { get; set; }

        /// <summary>
        /// Detalhamento das informações do estabelecimento, obra de construção civil ou unidade de órgão público.
        /// </summary>
        [XmlElement("dadosEstab")]
        public DadosEstab DadosEstab { get; set; }

        [XmlElement("novaValidade")]
        public NovaValidade1005 NovaValidade { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.NovaValidade1005")]
    [ComVisible(true)]
#endif
    public class NovaValidade1005
    {
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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Exclusao1005")]
    [ComVisible(true)]
#endif
    public class Exclusao1005
    {
        [XmlElement("ideEstab")]
        public IdeEstab IdeEstab { get; set; }
    }
}
