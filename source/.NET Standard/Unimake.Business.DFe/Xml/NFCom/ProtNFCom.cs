﻿#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFCom
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.ProtNFCom")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("protNFCom", Namespace = "http://www.portalfiscal.inf.br/nfcom", IsNullable = false)]
    public class ProtNFCom : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("infProt")]
        public InfProt InfProt { get; set; }

        [XmlElement("infFisco")]
        public InfFisco InfFisco { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.InfProt")]
    [ComVisible(true)]
#endif
    public class InfProt
    {
        [XmlAttribute(AttributeName = "Id", DataType = "ID")]
        public string Id { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        [XmlElement("chNFCom")]
        public string ChNFCom { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif

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
    [ProgId("Unimake.Business.DFe.Xml.NFCom.InfFisco")]
    [ComVisible(true)]
#endif
    public class InfFisco
    {
        [XmlElement("cMsg")]
        public string CMsg { get; set; }

        [XmlElement("xMsg")]
        public string XMsg { get; set; }
    }
}