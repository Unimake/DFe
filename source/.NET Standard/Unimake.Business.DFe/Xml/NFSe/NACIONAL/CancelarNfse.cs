#pragma warning disable CS1591
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{
    internal static class NfseNs
    {
        public const string Ns = "http://www.sped.fazenda.gov.br/nfse";
    }

    /// <summary>
    /// Pedido de Cancelamento de NFS-e (Padrão Nacional).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.CancelarNfse")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = NfseNs.Ns)]
    [XmlRoot("pedRegEvento", Namespace = NfseNs.Ns, IsNullable = false)]
    public class CancelarNfse : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de Cancelamento da NFS-e.
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Informações do Pedido de Cancelamento da NFS-e.
        /// </summary>
        [XmlElement("infPedReg", Namespace = NfseNs.Ns)]
        public InfPedRegCancelamento InfPedReg { get; set; } = new InfPedRegCancelamento();

        /// <summary>
        /// Assinatura XML-DSig (se exigida pelo provedor).
        /// </summary>
        [XmlElement("Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

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
        /// Identificador do Pedido de Cancelamento da NFS-e.
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id { get; set; }

        /// <summary>
        /// Tipo de Ambiente - Produção ou Homologação.
        /// </summary>
        [XmlElement("tpAmb", Namespace = NfseNs.Ns)]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versão da Aplicação.
        /// </summary>
        [XmlElement("verAplic", Namespace = NfseNs.Ns)]
        public string VerAplic { get; set; }

        /// <summary>
        /// Data e hora do evento.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEvento { get; set; }
#else
        public DateTimeOffset DhEvento { get; set; }
#endif

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
        /// CNPJ do autor do evento (opcional se houver CPF).
        /// </summary>
        [XmlElement("CNPJAutor", Namespace = NfseNs.Ns)]
        public string CNPJAutor { get; set; }

        /// <summary>
        /// CPF do autor do evento (opcional se houver CNPJ).
        /// </summary>
        [XmlElement("CPFAutor", Namespace = NfseNs.Ns)]
        public string CPFAutor { get; set; }

        /// <summary>
        /// Chave da NFS-e a ser cancelada.
        /// </summary>
        [XmlElement("chNFSe", Namespace = NfseNs.Ns)]
        public string ChNFSe { get; set; }

        /// <summary>
        /// Número do pedido/registro do evento.
        /// </summary>
        [XmlElement("nPedRegEvento", Namespace = NfseNs.Ns)]
        public string NPedRegEvento { get; set; }

        /// <summary>
        /// Evento de Cancelamento (código 101101).
        /// </summary>
        [XmlElement("e101101", Namespace = NfseNs.Ns)]
        public E101101 E101101 { get; set; } = new E101101();

        #region ShouldSerialize
        public bool ShouldSerializeCNPJAutor() => !string.IsNullOrWhiteSpace(CNPJAutor);
        public bool ShouldSerializeCPFAutor() => !string.IsNullOrWhiteSpace(CPFAutor);
        #endregion

    }

    /// <summary>
    /// Bloco do evento de Cancelamento da NFS-e (e101101).</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.E101101")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("e101101", Namespace = NfseNs.Ns)]
    public class E101101
    {
        /// <summary>Descrição do evento.</summary>
        [XmlElement("xDesc", Namespace = NfseNs.Ns)]
        public string XDesc { get; set; }

        /// <summary>Código do motivo do cancelamento.</summary>
        [XmlElement("cMotivo", Namespace = NfseNs.Ns)]
        public string CMotivo { get; set; }

        /// <summary>Descrição do motivo do cancelamento.</summary>
        [XmlElement("xMotivo", Namespace = NfseNs.Ns)]
        public string XMotivo { get; set; }
    }
}
