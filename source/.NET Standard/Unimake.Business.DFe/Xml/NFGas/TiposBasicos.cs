#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFGas
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.ProtNFGas")]
    [ComVisible(true)]
#endif
    public class ProtNFGas
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("infProt")]
        public InfProtNFGas InfProt { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.InfProtNFGas")]
    [ComVisible(true)]
#endif
    public class InfProtNFGas
    {
        [XmlElement("tpAmb")]
        public int TpAmb { get; set; }

        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        [XmlElement("chNFGas")]
        public string ChNFGas { get; set; }

        [XmlElement("dhRecbto")]
        public string DhRecbto { get; set; }

        [XmlElement("nProt")]
        public string NProt { get; set; }

        [XmlElement("digVal")]
        public string DigVal { get; set; }

        [XmlElement("cStat")]
        public int CStat { get; set; }

        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.InfEventoNFGas")]
    [ComVisible(true)]
#endif
    public class InfEventoNFGas
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id { get; set; }

        [XmlElement("cOrgao")]
        public int COrgao { get; set; }

        [XmlElement("tpAmb")]
        public int TpAmb { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("chNFGas")]
        public string ChNFGas { get; set; }

        [XmlElement("dhEvento")]
        public string DhEvento { get; set; }

        [XmlElement("tpEvento")]
        public string TpEvento { get; set; }

        [XmlElement("nSeqEvento")]
        public int NSeqEvento { get; set; }

        [XmlElement("detEvento")]
        public DetEventoNFGas DetEvento { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.DetEventoNFGas")]
    [ComVisible(true)]
#endif
    public class DetEventoNFGas
    {
        [XmlAttribute(AttributeName = "versaoEvento", DataType = "token")]
        public string VersaoEvento { get; set; }

        [XmlElement("evCancNFGas")]
        public EvCancNFGas EvCancNFGas { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.InfRetEventoNFGas")]
    [ComVisible(true)]
#endif
    public class InfRetEventoNFGas
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id { get; set; }

        [XmlElement("tpAmb")]
        public int TpAmb { get; set; }

        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        [XmlElement("cOrgao")]
        public int COrgao { get; set; }

        [XmlElement("cStat")]
        public int CStat { get; set; }

        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        [XmlElement("chNFGas")]
        public string ChNFGas { get; set; }

        [XmlElement("tpEvento")]
        public string TpEvento { get; set; }

        [XmlElement("xEvento")]
        public string XEvento { get; set; }

        [XmlElement("nSeqEvento")]
        public int NSeqEvento { get; set; }

        [XmlElement("dhRegEvento")]
        public string DhRegEvento { get; set; }

        [XmlElement("nProt")]
        public string NProt { get; set; }
    }
}
