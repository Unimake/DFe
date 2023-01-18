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
    /// XML para o serviço param recepção e/ou atualização dos parâmetros do SNCM necessários para o Sistema Cliente funcionar
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.MsgParam")]
    [ComVisible(true)]
#endif
    [XmlRoot("msgParam", Namespace = "http://sncm.anvisa.gov.br/", IsNullable = false)]
    public class MsgParam : XMLBase
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
        /// Deserializar o XML msgParam no objeto MsgParam.
        /// </summary>
        /// <param name="filename">Localização do arquivo XML msgParam</param>
        /// <returns>Objeto do MsgParam</returns>
        public MsgParam LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<MsgParam>(doc);
        }

        /// <summary>
        /// Deserializar o XML msgParam no objeto MsgParam.
        /// </summary>
        /// <param name="xml">string do XML msgParam</param>
        /// <returns>Objeto do MsgParam</returns>
        public MsgParam LoadFromXML(string xml) => XMLUtility.Deserializar<MsgParam>(xml);

        #region ShouldSerialize

        public bool ShouldSerializeMbrAgt() => !string.IsNullOrWhiteSpace(MbrAgt);

        #endregion
    }

    /// <summary>
    /// Identificação de atores do processo.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Declarant")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Declarant : StakeholderId
    {
    }

    public abstract class StakeholderId
    {
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

        /// <summary>
        /// Número do Cadastro Nacional de Estabelecimentos de Saúde.
        /// </summary>
        [XmlElement("cnes")]
        public string CNES { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCNES() => !string.IsNullOrWhiteSpace(CNES);

        #endregion
    }
}