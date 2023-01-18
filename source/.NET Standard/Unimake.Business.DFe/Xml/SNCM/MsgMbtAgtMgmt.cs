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
    /// XML para envio afim de incluir, excluir ou editar informações de um procurador que será autorizado a se comunicar com o SNCM em nome do membro.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.MsgMbtAgtMgmt")]
    [ComVisible(true)]
#endif
    [XmlRoot("msgMbtAgtMgmt", Namespace = "http://sncm.anvisa.gov.br/", IsNullable = false)]
    public class MsgMbtAgtMgmt : XMLBase
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
        /// Grupo com informações sobre inclusão e exclusão de procuradores.
        /// </summary>
        [XmlElement("mbrAgts")]
        public MbrAgts MbrAgts { get; set; }

        /// <summary>
        /// Deserializar o XML msgMbtAgtMgmt no objeto MsgMbtAgtMgmt.
        /// </summary>
        /// <param name="filename">Localização do arquivo XML msgMbtAgtMgmt</param>
        /// <returns>Objeto do MsgMbtAgtMgmt</returns>
        public MsgMbtAgtMgmt LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<MsgMbtAgtMgmt>(doc);
        }

        /// <summary>
        /// Deserializar o XML msgMbtAgtMgmt no objeto MsgMbtAgtMgmt.
        /// </summary>
        /// <param name="xml">string do XML msgMbtAgtMgmt</param>
        /// <returns>Objeto do MsgMbtAgtMgmt</returns>
        public MsgMbtAgtMgmt LoadFromXML(string xml) => XMLUtility.Deserializar<MsgMbtAgtMgmt>(xml);

        #region ShouldSerialize

        /// <summary>
        /// Só gera a tag "mbrAgt" se a propriedade "MbrAgt" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "mbrAgt"</returns>
        public bool ShouldSerializeMbrAgt() => !string.IsNullOrWhiteSpace(MbrAgt);

        #endregion
    }

    /// <summary>
    /// Grupo com informações sobre inclusão e exclusão de procuradores.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.MbrAgts")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class MbrAgts
    {
        /// <summary>
        /// Comando a ser executado para inclusão, exclusão ou listar procuradores cadastrados.
        /// </summary>
        [XmlElement("comnd")]
        public string Comnd { get; set; }

        /// <summary>
        /// Número no Cadastro de Pessoas Físicas.
        /// </summary>
        [XmlElement("cpf")]
        public string CPF { get; set; }

        /// <summary>
        /// Número no Cadastro Nacional de Pessoas Jurídicas.
        /// </summary>
        [XmlElement("cnpj")]
        public string CNPJ { get; set; }
    }
}