#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Classe de informações resumidas de um evento da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ResEvento")]
    [ComVisible(true)]
#endif
    [XmlRoot("resEvento", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class ResEvento : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de resumo do evento da NFe/NFCe
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Código do órgão de recepção do Evento. Utilizar a Tabela do IBGE extendida, utilizar 91 para identificar o Ambiente Nacional
        /// </summary>
        [XmlIgnore]
        public UFBrasil COrgao { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade COrgao para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cOrgao")]
        public int COrgaoField
        {
            get => (int)COrgao;
            set => COrgao = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// CNPJ do emitente
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do emitente
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Chave de acesso da NFe/NFCe
        /// </summary>
        [XmlElement("chNFe")]
        public string ChNFe { get; set; }

        /// <summary>
        /// Data e hora do evento no formato AAAA-MM-DDThh:mm:ssTZD
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEvento { get; set; }
#else
        public DateTimeOffset DhEvento { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DhEvento para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhEvento")]
        public string DhEventoField
        {
            get => DhEvento.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhEvento = DateTime.Parse(value);
#else
            set => DhEvento = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Tipo do evento da NFe/NFCe
        /// </summary>
        [XmlElement("tpEvento")]
        public TipoEventoNFe TpEvento { get; set; }

        /// <summary>
        /// Sequencial do evento para o mesmo tipo de evento.
        /// Para maioria dos eventos será 1, nos casos em que possa existir mais de um evento, 
        /// como é o caso da carta de correção, o autor do evento deve numerar de forma sequencial
        /// </summary>
        [XmlElement("nSeqEvento")]
        public int NSeqEvento { get; set; }

        /// <summary>
        /// Descrição do evento
        /// </summary>
        [XmlElement("xEvento")]
        public string XEvento { get; set; }

        /// <summary>
        /// Data e hora de autorização do evento no formato AAAA-MM-DDThh:mm:ssTZD
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DhRecbto para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRecbto = DateTime.Parse(value);
#else
            set => DhRecbto = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Número do protocolo do evento
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion ShouldSerialize
    }
}