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
    /// Classe do protocolo de status resultado do processamento da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ProtNFe")]
    [ComVisible(true)]
#endif
    public class ProtNFe
    {
        /// <summary>
        /// Versão do schema do XML do protocolo da NFe/NFCe
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Dados do protocolo de status
        /// </summary>
        [XmlElement("infProt")]
        public InfProt InfProt { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Classe dos dados do protocolo de status
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfProt")]
    [ComVisible(true)]
#endif
    public class InfProt
    {
        /// <summary>
        /// ID.
        /// Composição: ID + nProt
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "ID")]
        public string Id { get; set; }

        /// <summary>
        /// Tipo de ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versão do Aplicativo que processou a NFe/NFCe
        /// </summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        /// <summary>
        /// Chave de acesso da NFe/NFCe
        /// </summary>
        [XmlElement("chNFe")]
        public string ChNFe { get; set; }

        /// <summary>
        /// Data e hora de processamento, no formato AAAA-MM-DDTHH:MM:SS (ou AAAA-MM-DDTHH:MM:SSTZD, de acordo com versão).
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
        /// Número do Protocolo de Status da NFe/NFCe
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Digest Value da NFe/NFCe processada. Utilizado para conferir a integridade da NFe/NFCe original
        /// </summary>
        [XmlElement("digVal")]
        public string DigVal { get; set; }

        /// <summary>
        /// Código do status da mensagem enviada
        /// </summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }

        /// <summary>
        /// Descrição literal do status do serviço solicitado
        /// </summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        /// <summary>
        /// Código da Mensagem
        /// </summary>
        [XmlElement("cMsg")]
        public string CMsg { get; set; }

        /// <summary>
        /// Mensagem da SEFAZ para o emissor
        /// </summary>
        [XmlElement("xMsg")]
        public string XMsg { get; set; }
    }
}
