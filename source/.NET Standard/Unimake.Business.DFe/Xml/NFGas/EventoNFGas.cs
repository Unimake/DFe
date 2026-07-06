#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFGas
{
    /// <summary>
    /// Evento da NFGas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.EventoNFGas")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfgas")]
    [XmlRoot("eventoNFGas", Namespace = "http://www.portalfiscal.inf.br/nfgas", IsNullable = false)]
    public class EventoNFGas : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("infEvento")]
        public InfEventoNFGas InfEvento { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Detalhamento do evento de vinculação do pagamento da NFGas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.EvVincPgto")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfgas")]
    [XmlRoot("evVincPgto", Namespace = "http://www.portalfiscal.inf.br/nfgas", IsNullable = false)]
    public class EvVincPgto : XMLBase
    {
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Vinculacao do Pagamento";

        [XmlElement("nProt")]
        public string NProt { get; set; }

        [XmlElement("pgto")]
        public Pgto Pgto { get; set; }
    }

    /// <summary>
    /// Detalhamento do evento de cancelamento da vinculação do pagamento da NFGas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.EvCancVincPgto")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfgas")]
    [XmlRoot("evCancVincPgto", Namespace = "http://www.portalfiscal.inf.br/nfgas", IsNullable = false)]
    public class EvCancVincPgto : XMLBase
    {
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Cancelamento da vinculacao do pagamento";

        [XmlElement("nProt")]
        public string NProt { get; set; }

        [XmlElement("nProtVincPgto")]
        public string NProtVincPgto { get; set; }
    }

    /// <summary>
    /// Dados do pagamento vinculados ao evento da NFGas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.Pgto")]
    [ComVisible(true)]
#endif
    public class Pgto
    {
        [XmlAttribute(AttributeName = "nPag", DataType = "token")]
        public string NPag { get; set; }

        [XmlAttribute(AttributeName = "idTransacao", DataType = "token")]
        public string IdTransacao { get; set; }

        [XmlElement("tpMeioPgto")]
        public string TpMeioPgto { get; set; }

        [XmlElement("CNPJReceb")]
        public string CNPJReceb { get; set; }

        [XmlElement("CNPJBasePSP")]
        public string CNPJBasePSP { get; set; }
    }
}
