#pragma warning disable CS1591
using Org.BouncyCastle.Asn1.Cms;
using System;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Xml.ESocial.Retorno;
using Unimake.Business.DFe.Xml.NFe;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// Downloads de eventos por ID
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DownloadEventosPorID")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/download/solicitacao/id/v1_0_0", IsNullable = false)]
    public class DownloadEventosPorID : XMLBase
    {
        /// <summary>
        /// Versão do evento
        /// </summary>
        [XmlIgnore]
        public string Versao { get; set; } = "1.1.0";

        /// <summary>
        /// Contém as informações relativas ao download.
        /// </summary>
        [XmlElement("download")]
        public Download Download {  get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Downloads de eventos por número do recibo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DownloadEventosPorNrRec")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/download/solicitacao/nrRecibo/v1_0_0", IsNullable = false)]
    public class DownloadEventosPorNrRec : XMLBase
    {
        /// <summary>
        /// Contém as informações relativas ao download.
        /// </summary>
        [XmlElement("download")]
        public Download Download { get; set; }

        /// <summary>
        /// Versão do evento
        /// </summary>
        [XmlIgnore]
        public string Versao { get; set; } = "1.1.0";

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Contém as informações relativas ao download
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Download")]
    [ComVisible(true)]
#endif
    public class Download
    {
        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Contém os identificadores dos eventos que serão solicitados.
        /// </summary>
        [XmlElement("solicDownloadEvtsPorId")]
        public SolicitacaoDownloadPorId SolicitacaoDownloadPorId {  get; set; }

        /// <summary>
        /// Contém os números de recibo dos eventos que serão solicitados.
        /// </summary>
        [XmlElement("solicDownloadEventosPorNrRecibo")]
        public SolicitacaoDownloadPorNrRec SolicitacaoDownloadPorNrRec { get; set; }
    }

    /// <summary>
    /// Contém os identificadores dos eventos que serão solicitados.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SolicitacaoDownloadPorId")]
    [ComVisible(true)]
#endif
    public class SolicitacaoDownloadPorId
    {
        /// <summary>
        /// Contém o identificador do evento, atributo Id que na tag evtXXXX de cada evento
        /// </summary>
        [XmlElement("id")]
        public string Id {  get; set; }
    }

    /// <summary>
    /// Contém os números de recibo dos eventos que serão solicitados.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SolicitacaoDownloadPorNrRec")]
    [ComVisible(true)]
#endif
    public class SolicitacaoDownloadPorNrRec
    {
        /// <summary>
        /// Contém o número de recibo do evento que será solicitado
        /// </summary>
        [XmlElement("nrRec")]
        public string NrRec { get; set; }
    }
}
