#pragma warning disable CS1591
using System;
using System.Xml;
using System.Xml.Serialization;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DownloadEventosPorID")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/download/solicitacao/id/v1_0_0", IsNullable = false)]
    public class DownloadEventosPorID : XMLBase
    {
        [XmlElement("download")]
        public Download Download {  get; set; }

        [XmlIgnore]
        public string Versao { get; set; } = "1.1.0";
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DownloadEventosPorNrRec")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/download/solicitacao/nrRecibo/v1_0_0", IsNullable = false)]
    public class DownloadEventosPorNrRec : XMLBase
    {
        [XmlElement("download")]
        public Download Download { get; set; }

        [XmlIgnore]
        public string Versao { get; set; } = "1.1.0";
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Download")]
    [ComVisible(true)]
#endif
    public class Download
    {
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("solicDownloadEvtsPorId")]
        public SolicitacaoDownloadPorId SolicitacaoDownloadPorId {  get; set; }

        [XmlElement("solicDownloadEventosPorNrRecibo")]
        public SolicitacaoDownloadPorNrRec SolicitacaoDownloadPorNrRec { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SolicitacaoDownloadPorId")]
    [ComVisible(true)]
#endif
    public class SolicitacaoDownloadPorId
    {
        [XmlElement("id")]
        public string Id {  get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SolicitacaoDownloadPorNrRec")]
    [ComVisible(true)]
#endif
    public class SolicitacaoDownloadPorNrRec
    {
        [XmlElement("nrRec")]
        public string NrRec { get; set; }
    }
}
