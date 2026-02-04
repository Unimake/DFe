#pragma warning disable CS1591
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{
    /// <summary>
    /// Namespace da NFSe Ambiente Nacional
    /// </summary>
    public static class NfseNs
    {
        public const string Ns = "http://www.sped.fazenda.gov.br/nfse";
    }

    /// <summary>
    /// Pedido de Cancelamento de NFS-e (Padrão Nacional).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.PedRegEvento")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = NfseNs.Ns)]
    [XmlRoot("pedRegEvento", Namespace = NfseNs.Ns, IsNullable = false)]
    public class PedRegEvento : XMLBase
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
        public InfPedReg InfPedReg { get; set; } = new InfPedReg();

        /// <summary>
        /// Assinatura XML-DSig (se exigida pelo provedor).
        /// </summary>
        [XmlElement("Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

        public override System.Xml.XmlDocument GerarXML()
        {
            InfPedReg?.ValidarRegrasAutor();
            return base.GerarXML();
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.InfPedReg")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("infPedReg", Namespace = NfseNs.Ns)]
    public class InfPedReg
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
        public E101101 E101101 { get; set; }

        /// <summary>
        /// Evento de Cancelamento por Substituição (código 105102).
        /// </summary>
        [XmlElement("e105102", Namespace = NfseNs.Ns)]
        public E105102 E105102 { get; set; }

        /// <summary>
        /// Evento de Confirmação do Prestador (código 202201).
        /// </summary>
        [XmlElement("e202201", Namespace = NfseNs.Ns)]
        public E202201 E202201 { get; set; }

        /// <summary>
        /// Evento de Rejeição do Prestador (código 202205).
        /// </summary>
        [XmlElement("e202205", Namespace = NfseNs.Ns)]
        public E202205 E202205 { get; set; }

        /// <summary>
        /// Evento de Confirmação do Tomador (código 203202).
        /// </summary>
        [XmlElement("e203202", Namespace = NfseNs.Ns)]
        public E203202 E203202 { get; set; }

        ///<summary>
        ///Evento de Confirmação do Intermediário (código 204203). 
        /// </summary>
        [XmlElement("e204203", Namespace = NfseNs.Ns)]
        public E204203 E204203 { get; set; }

        /// <summary>
        /// Evento de Confirmação Tácita (código 205204).
        /// </summary>
        [XmlElement("e205204", Namespace = NfseNs.Ns)]
        public E205204 E205204 { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeCNPJAutor() => !string.IsNullOrWhiteSpace(CNPJAutor);
        public bool ShouldSerializeCPFAutor() => !string.IsNullOrWhiteSpace(CPFAutor);
        public bool ShouldSerializeNPedRegEvento() => !string.IsNullOrWhiteSpace(NPedRegEvento);
        #endregion

        public void ValidarRegrasAutor()
        {
            var temCnpj = !string.IsNullOrWhiteSpace(CNPJAutor);
            var temCpf = !string.IsNullOrWhiteSpace(CPFAutor);
            if (temCnpj == temCpf)
            {
                throw new Exception("Informe exatamente um identificador do autor: CNPJAutor OU CPFAutor.");
            }
        }
    }
}