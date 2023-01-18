#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.SNCM
{
    /// <summary>
    /// XML para envio destinado à vinculação, pelo detentor do registro, de metadados ao Número de Registro do Medicamento. Por meio dele é possível alterar as informações do cadastro de uma apresentação de medicamento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.MsgMdataMAH")]
    [ComVisible(true)]
#endif
    [XmlRoot("msgMdataMAH", Namespace = "http://sncm.anvisa.gov.br/", IsNullable = false)]
    public class MsgMdataMAH : XMLBase
    {
        /// <summary>
        /// Identificador de controle da comunicação com o SNCM, gerado pelo Sistema Cliente.
        /// </summary>
        [XmlElement("docId")]
        public string DocId { get; set; }

        /// <summary>
        /// Carimbo de tempo realizado pelo Sistema Cliente no instante da comunicação com o SNCM.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime CcTime { get; set; }
#else
        public DateTimeOffset CcTime { get; set; }
#endif

        /// <summary>
        /// Auxiliar da propriedade "CcTime" - utilize a propriedade "CcTime" para atribuir valor.
        /// </summary>
        [XmlElement("ccTime")]
        public string CcTimeField
        {
            get => CcTime.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => CcTime = DateTime.Parse(value);
#else
            set => CcTime = DateTimeOffset.Parse(value);
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
        /// Identificador do membro da cadeia.
        /// </summary>
        [XmlElement("declarant")]
        public Declarant Declarant { get; set; }

        /// <summary>
        /// Identificação do procurador que assina a comunicação (CNPJ).
        /// </summary>
        [XmlElement("mbrAgt")]
        public string MbrAgt { get; set; }

        private string UsrAgtField { get; set; }

        /// <summary>
        /// Elemento livre para o desenvolvedor do Sistema Cliente indicar seu nome, a respectiva versão do software cliente, um telefone para contato com DDD e um email.
        /// </summary>
        [XmlElement("usrAgt")]
        public string UsrAgt
        {
            get => UsrAgtField;
            set => UsrAgtField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(140).Trim());
        }

        /// <summary>
        /// Comando a ser executado para inclusão, exclusão ou listar procuradores cadastrados.
        /// </summary>
        [XmlElement("comnd")]
        public string Comnd { get; set; }

        /// <summary>
        /// Número de registro do medicamento processado na solicitação.
        /// </summary>
        [XmlElement("regAnvisa")]
        public string RegAnvisa { get; set; }

        /// <summary>
        /// Grupo com informações sobre a vinculação do número de registro do medicamento com os Metadados.Só informado quando o comando executado for Consultar Metadados.
        /// </summary>
        [XmlElement("mdata")]
        public MData MData { get; set; }

        /// <summary>
        /// Deserializar o XML msgMdataMAH no objeto MsgMdataMAH.
        /// </summary>
        /// <param name="filename">Localização do arquivo XML msgMdataMAH</param>
        /// <returns>Objeto do MsgMdataMAH</returns>
        public MsgMdataMAH LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<MsgMdataMAH>(doc);
        }

        /// <summary>
        /// Deserializar o XML msgMdataMAH no objeto MsgMdataMAH.
        /// </summary>
        /// <param name="xml">string do XML MsgMdataMAH</param>
        /// <returns>Objeto do MsgMdataMAH</returns>
        public MsgMdataMAH LoadFromXML(string xml) => XMLUtility.Deserializar<MsgMdataMAH>(xml);
    }

    /// <summary>
    /// Grupo com informações sobre a vinculação do número de registro do medicamento com os Metadados.Só informado quando o comando executado for Consultar Metadados.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.MData")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class MData : MDataBase
    {
        /// <summary>
        /// Grupo com informações sobre a interrupção da comercialização do produto.Só informado quandocomnd igual a “3”.
        /// </summary>
        [XmlElement("interrupt")]
        public Interrupt Interrupt { get; set; }
    }

    /// <summary>
    /// Grupo com informações sobre a interrupção da comercialização do produto.Só informado quandocomnd igual a “3”.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Interrupt")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Interrupt
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
    }
}
