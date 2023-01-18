#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.SNCM
{
    /// <summary>
    /// XML de retorno do serviço de vinculação, pelo detentor do registro, de metadados ao Número de Registro do Medicamento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.RetMdataMAH")]
    [ComVisible(true)]
#endif
    [XmlRoot("retMdataMAH", Namespace = "http://sncm.anvisa.gov.br/", IsNullable = false)]
    public class RetMdataMAH : XMLBase
    {
        /// <summary>
        /// Identificador de controle da comunicação do SNCM.
        /// </summary>
        [XmlElement("docId")]
        public string DocId { get; set; }

        /// <summary>
        /// Data e horário da recepção da mensagem de entrada
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime CrTime { get; set; }
#else
        public DateTimeOffset CrTime { get; set; }
#endif

        /// <summary>
        /// Auxiliar de serialização da tag "crTime" - Use a propriedade "CrTime" para atribuir o valor.
        /// </summary>
        [XmlElement("crTime")]
        public string CrTimeField
        {
            get => CrTime.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => CrTime = DateTime.Parse(value);
#else
            set => CrTime = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Versão do Leiaute
        /// </summary>
        [XmlElement("ver")]
        public string Ver { get; set; } = "0.01";

        /// <summary>
        /// Idioma desejado para as mensagens descritivas de retorno. Padrão: pt-BR.
        /// </summary>
        [XmlElement("lc")]
        public string Lc { get; set; } = "pt-BR";

        /// <summary>
        /// Identificação do Ambiente.
        /// </summary>
        [XmlElement("env")]
        public IdentificacaoAmbiente Env { get; set; } = IdentificacaoAmbiente.Teste;

        /// <summary>
        /// Código identificador da retaguarda que atendeu a solicitação
        /// </summary>
        [XmlElement("backOff")]
        public string BackOff { get; set; }

        /// <summary>
        /// Número de registro do medicamento processado na solicitação.
        /// </summary>
        [XmlElement("regAnvisa")]
        public string RegAnvisa { get; set; }

        /// <summary>
        /// Grupo com informações sobre a vinculação do número de registro do medicamento com os Metadados.Só informado quando o comando executado for Consultar Metadados.
        /// </summary>
        [XmlElement("mdata")]
        public MDataRet MData { get; set; }

        /// <summary>
        /// Grupo de mensagens de retorno do Web Service.
        /// </summary>
        [XmlElement("rets")]
        public Rets Rets { get; set; }

        /// <summary>
        /// Identificação do surgimento de anomalias pela comunicação do documento de entrada: false – Não surgiram anomalias / true – Surgiram anomalias.
        /// </summary>
        [XmlElement("anom")]
        public bool Anom { get; set; }

        /// <summary>
        /// Identificação de existência de notificações ao membro: false – Não existem notificações / true – Existem notificações
        /// </summary>
        [XmlElement("notif")]
        public bool Notif { get; set; }

        /// <summary>
        /// Identificação de existência de ações a serem desempenhadas pelo membro: false – Não existem ações / true – Existem ações do SNCM a serem desempenhados pelo membro.
        /// </summary>
        [XmlElement("action")]
        public bool Action { get; set; }

        /// <summary>
        /// Deserializar o XML retMdataMAH no objeto RetMdataMAH.
        /// </summary>
        /// <param name="filename">Localização do arquivo XML retMdataMAH</param>
        /// <returns>Objeto do RetMdataMAH</returns>
        public RetMdataMAH LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<RetMdataMAH>(doc);
        }

        /// <summary>
        /// Deserializar o XML retMdataMAH no objeto RetMdataMAH.
        /// </summary>
        /// <param name="xml">string do XML retMdataMAH</param>
        /// <returns>Objeto do RetMdataMAH</returns>
        public RetMdataMAH LoadFromXML(string xml) => XMLUtility.Deserializar<RetMdataMAH>(xml);
    }

    /// <summary>
    /// Grupo com informações sobre a vinculação do número de registro do medicamento com os Metadados.Só informado quando o comando executado for Consultar Metadados.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.RetMData")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class MDataRet : MDataBase
    {
        /// <summary>
        /// Data da interrupção da comercialização.
        /// </summary>
        [XmlIgnore]
        public DateTime BreakDate { get; set; }

        /// <summary>
        /// Auxiliar de serialização da tag "breakDate" - Use a propriedade "BreakDate" para atribuir o valor.
        /// </summary>
        [XmlElement("breakDate")]
        public string BreakDateField
        {
            get => BreakDate.ToString("yyyy-MM-dd");
            set => BreakDate = DateTime.Parse(value);
        }

        private string ReasonField { get; set; }

        /// <summary>
        /// Razão da interrupção da comercialização.
        /// </summary>
        [XmlElement("reason")]
        public string Reason
        {
            get => ReasonField;
            set => ReasonField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(140).Trim());
        }

        /// <summary>
        /// Só gera a tag "breakDate" se a propriedade "BreakDate" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "breakDate"</returns>
        public bool ShouldSerializeBreakDateField() => BreakDate > DateTime.MinValue;

        /// <summary>
        /// Só gera a tag "reason" se a propriedade "Reason" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "reason"</returns>
        public bool ShouldSerializeReason() => !string.IsNullOrWhiteSpace(Reason);
    }

    /// <summary>
    /// Grupo com informações sobre a vinculação do número de registro do medicamento com os Metadados.Só informado quando o comando executado for Consultar Metadados.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.MDataBase")]
    [ComVisible(true)]
#endif
    public abstract class MDataBase
    {
        /// <summary>
        /// Código de Classificação ATC, mantido pela Organização Mundial de Saúde(OMS).
        /// </summary>
        [XmlElement("atcWho")]
        public string AtcWho { get; set; }

        private string AtcDDDField { get; set; }

        /// <summary>
        /// A Dose Diária Definida DDD (Defined Daily Dose), combinada com o elemento AtcWho, forma o sistema ATC/DDD, mantido pela Organização Mundial de Saúde (OMS).
        /// </summary>
        [XmlElement("atcDDD")]
        public string AtcDDD
        {
            get => AtcDDDField;
            set => AtcDDDField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(140).Trim());
        }

        /// <summary>
        /// Código de Classificação ATC, mantido pela European Pharmaceutical Market Research Association(EphMRA).
        /// </summary>
        [XmlElement("atcEph")]
        public string AtcEph { get; set; }

        /// <summary>
        /// Classificação Regulatória do Medicamento, conforme Portaria SVS/MS nº 344/98 e RDC Anvisa nº 20/11: 1 – A1/A2/A3 Entorpecentes, Psicotrópicas / 2 – B1/B2 Psicotrópicas, Anorexígenas / 3 – C1 Outras substâncias / 4 – C2 Retinóides (sistêmico) / 5 – C3 Imunossupressor(Talidomida) / 6 – C4 Anti-retrovirais / 7 – C5 Anabolizantes / 8 – Antibióticos.
        /// </summary>
        [XmlElement("regClass")]
        public string RegClass { get; set; }

        /// <summary>
        /// Data inicial da comercialização do produto.
        /// </summary>
        [XmlIgnore]
        public DateTime InitDate { get; set; }

        /// <summary>
        /// Auxiliar de serialização da tag "initDate" - Use a propriedade "InitDate" para atribuir o valor.
        /// </summary>
        [XmlElement("initDate")]
        public string InitDateField
        {
            get => InitDate.ToString("yyyy-MM-dd");
            set => InitDate = DateTime.Parse(value);
        }

        /// <summary>
        /// Tipo da Notificação de Receita, conforme Portaria SVS/MS nº 344/98 e RDC Anvisa nº 20/11.
        /// </summary>
        [XmlElement("presType")]
        public string PresType { get; set; }

        /// <summary>
        /// Data final prevista da comercialização do produto.
        /// </summary>
        [XmlIgnore]
        public DateTime FinalDate { get; set; }

        /// <summary>
        /// Auxiliar de serialização da tag "finalDate" - Use a propriedade "FinalDate" para atribuir o valor.
        /// </summary>
        [XmlElement("finalDate")]
        public string FinalDateField
        {
            get => FinalDate.ToString("yyyy-MM-dd");
            set => FinalDate = DateTime.Parse(value);
        }

        /// <summary>
        /// GTIN da apresentação do medicamento. Mesmo código utilizado na serialização do medicamento.
        /// </summary>
        [XmlElement("gtin")]
        public List<string> Gtin { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddGtin(string item)
        {
            if (Gtin == null)
            {
                Gtin = new List<string>();
            }

            Gtin.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Gtin (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Gtin</returns>
        public string GetDui(int index)
        {
            if ((Gtin?.Count ?? 0) == 0)
            {
                return default;
            };

            return Gtin[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Gtin
        /// </summary>
        public int GetGtinCount => (Gtin != null ? Gtin.Count : 0);

#endif

        #region ShouldSerialize

        /// <summary>
        /// Só gera a tag "atcWho" se a propriedade "AtcWho" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "atcWho"</returns>
        public bool ShouldSerializeAtcWho() => !string.IsNullOrWhiteSpace(AtcWho);

        /// <summary>
        /// Só gera a tag "atcDDD" se a propriedade "AtcDDD" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "atcDDD"</returns>
        public bool ShouldSerializeAtcDDD() => !string.IsNullOrWhiteSpace(AtcDDD);

        /// <summary>
        /// Só gera a tag "atcEph" se a propriedade "AtcEph" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "atcEph"</returns>
        public bool ShouldSerializeAtcEph() => !string.IsNullOrWhiteSpace(AtcEph);

        /// <summary>
        /// Só gera a tag "regClass" se a propriedade "RegClass" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "regClass"</returns>
        public bool ShouldSerializeRegClass() => !string.IsNullOrWhiteSpace(RegClass);

        /// <summary>
        /// Só gera a tag "initDate" se a propriedade "InitDate" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "initDate"</returns>
        public bool ShouldSerializeInitDateField() => InitDate > DateTime.MinValue;

        /// <summary>
        /// Só gera a tag "presType" se a propriedade "PresType" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "presType"</returns>
        public bool ShouldSerializePresType() => !string.IsNullOrWhiteSpace(PresType);

        /// <summary>
        /// Só gera a tag "finalDate" se a propriedade "FinalDate" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "finalDate"</returns>
        public bool ShouldSerializeFinalDateField() => FinalDate > DateTime.MinValue;

        #endregion
    }
}