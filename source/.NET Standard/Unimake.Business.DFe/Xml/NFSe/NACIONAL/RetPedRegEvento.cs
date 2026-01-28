#pragma warning disable CS1591
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{
   
    #region Retorno de Sucesso (estrutura <evento>)

    /// <summary>
    /// Retorno de sucesso do evento (envelope que contém o pedRegEvento original).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Evento")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("evento", Namespace = NfseNs.Ns)]
    public class Evento : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de retorno.
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Informações do evento (dados adicionais do retorno).
        /// </summary>
        [XmlElement("infEvento", Namespace = NfseNs.Ns)]
        public InfEvento InfEvento { get; set; }

        /// <summary>
        /// Assinatura XML-DSig do retorno.
        /// </summary>
        [XmlElement("Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Informações do evento (metadados do processamento).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.InfEvento")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("infEvento", Namespace = NfseNs.Ns)]
    public class InfEvento
    {
        /// <summary>
        /// Identificador do evento.
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id { get; set; }

        /// <summary>
        /// Versão da aplicação que processou.
        /// </summary>
        [XmlElement("verAplic", Namespace = NfseNs.Ns)]
        public string VerAplic { get; set; }

        /// <summary>
        /// Ambiente de geração do evento.
        /// </summary>
        [XmlElement("ambGer", Namespace = NfseNs.Ns)]
        public TipoAmbiente AmbGer { get; set; }

        /// <summary>
        /// Número sequencial do evento.
        /// </summary>
        [XmlElement("nSeqEvento", Namespace = NfseNs.Ns)]
        public int NSeqEvento { get; set; }

        /// <summary>
        /// Data e hora do processamento.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhProc { get; set; }
#else
        public DateTimeOffset DhProc { get; set; }
#endif

        [XmlElement("dhProc", Namespace = NfseNs.Ns)]
        public string DhProcField
        {
            get => DhProc.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhProc = DateTime.Parse(value);
#else
            set => DhProc = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Número do documento fiscal eletrônico.
        /// </summary>
        [XmlElement("nDFe", Namespace = NfseNs.Ns)]
        public int NDFe { get; set; }

        /// <summary>
        /// Pedido de registro de evento original (retornado pelo servidor).
        /// </summary>
        [XmlElement("pedRegEvento", Namespace = NfseNs.Ns)]
        public PedRegEvento PedRegEvento { get; set; }
    }

    #endregion
}