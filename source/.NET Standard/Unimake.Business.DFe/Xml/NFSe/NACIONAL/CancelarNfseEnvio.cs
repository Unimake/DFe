#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Xml.Schema;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{

    internal static class NfseNs
    {
        public const string Ns = "http://www.sped.fazenda.gov.br/nfse";
    }

    ///<summary>
    ///Pedido de Cancelamento de NFS-e padrão NACIONAL
    ///</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.CancelarNfse")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("pedRegEvento", Namespace = NfseNs.Ns, IsNullable = false)]
    public class CancelarNfseEnvio : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de Cancelamento da NFS-e
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Informações do Pedido de Cancelamento da NFS-e
        /// </summary>
        [XmlElement("infPedReg", Namespace = NfseNs.Ns)]
        public InfPedRegCancelamento InfPedReg { get; set; } = new InfPedRegCancelamento();

        /// <summary>
        /// Assinatura do XML
        /// </summary>
        [XmlElement("Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Bloco das informações do Pedido de Cancelamento da NFS-e
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.InfPedRegCancelamento")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("infPedReg", Namespace = NfseNs.Ns)]
    public class InfPedRegCancelamento
    {
        /// <summary>
        /// Identificador do Pedido de Cancelamento da NFS-e
        /// </summary>
        [XmlAttribute(AttributeName = "Id")]
        public string Id { get; set; }

        /// <summary>
        /// Tipo de Ambiente - Produção ou Homologação
        /// </summary>
        [XmlElement("tpAmb", Namespace = NfseNs.Ns)]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versão da Aplicação
        /// </summary>
        [XmlElement("verAplic", Namespace = NfseNs.Ns)]
        public string VerAplic { get; set; }

        /// <summary>
        /// Data e Hora do Evento, formato UTC (AAAA-MM-DDThh:mm:ssTZD, onde TZD = +hh:mm ou -hh:mm)
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
        [XmlElement("dhEvento", Namespace = NfseNs.Ns)]
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
        /// CNPJ do Autor do Evento
        /// </summary>
        [XmlElement("CNPJAutor", Namespace = NfseNs.Ns)]
        public string CNPJAutor { get; set; }

        /// <summary>
        /// CPF do Autor do Evento
        /// </summary>
        [XmlElement("CPFAutor", Namespace = NfseNs.Ns)]
        public string CPFAutor { get; set; }

        /// <summary>
        /// Chave da NFS-e a ser cancelada
        /// </summary>
        [XmlElement("chNFSe", Namespace = NfseNs.Ns)]
        public string ChNFSe { get; set; }

        /// <summary>
        /// Código do Cancelamento
        /// </summary>
        [XmlElement("nPedRegEvento", Namespace = NfseNs.Ns)]
        public string NPedRegEvento { get; set; }

        /// <summary>
        /// Evento de Cancelamento da NFS-e
        /// </summary>
        [XmlElement("e101101", Namespace = NfseNs.Ns)]
        public EvCanc101101 E101101 { get; set; } = new EvCanc101101();

        #region ShouldSerialize
        /// <summary>
        /// Método que indica se o campo CNPJAutor deve ser serializado
        /// </summary>
        /// <returns></returns>
        public bool ShouldSerializeCNPJAutor() => !string.IsNullOrWhiteSpace(CNPJAutor);

        /// <summary>
        /// Método que indica se o campo CPFAutor deve ser serializado
        /// </summary>
        /// <returns></returns>
        public bool ShouldSerializeCPFAutor() => !string.IsNullOrWhiteSpace(CPFAutor);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Bloco do evento de Cancelamento da NFS-e
    /// </summary>
    [Serializable]
    [XmlType("e101101", Namespace = NfseNs.Ns)]
    public class EvCanc101101
    {
        /// <summary>
        /// Descrição do evento.
        /// </summary>
        [XmlElement("xDesc", Namespace = NfseNs.Ns)]
        public string XDesc { get; set; }

        /// <summary>
        /// Código do motivo do cancelamento.
        /// </summary>
        [XmlElement("cMotivo", Namespace = NfseNs.Ns)]
        public string CMotivo { get; set; }

        /// <summary>
        /// Descrição do motivo do cancelamento.
        /// </summary>
        [XmlElement("xMotivo", Namespace = NfseNs.Ns)]
        public string XMotivo { get; set; }
    }
}