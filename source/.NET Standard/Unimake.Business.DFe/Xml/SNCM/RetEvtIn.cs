#pragma warning disable CS1591

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
    /// XML para o serviço para recepção e/ou atualização dos parâmetros do SNCM necessários para o Sistema Cliente funcionar
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.RetEvtIn")]
    [ComVisible(true)]
#endif
    [XmlRoot("retEvtIn", Namespace = "http://sncm.anvisa.gov.br/", IsNullable = false)]
    public class RetEvtIn : XMLBase
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
        /// Número do recibo de processamento do SNCM
        /// </summary>
        [XmlElement("receipt")]
        public string Receipt { get; set; }

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
        /// Deserializar o XML retEvtIn no objeto RetEvtIn.
        /// </summary>
        /// <param name="filename">Localização do arquivo XML retEvtIn</param>
        /// <returns>Objeto do RetEvtIn</returns>
        public RetEvtIn LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<RetEvtIn>(doc);
        }

        /// <summary>
        /// Deserializar o XML retEvtIn no objeto RetEvtIn.
        /// </summary>
        /// <param name="xml">string do XML retEvtIn</param>
        /// <returns>Objeto do RetEvtIn</returns>
        public RetEvtIn LoadFromXML(string xml) => XMLUtility.Deserializar<RetEvtIn>(xml);
    }
}