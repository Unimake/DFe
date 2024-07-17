#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1270")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtContratAvNP/v_S_01_02_00", IsNullable = false)]
    public class ESocial1270 : XMLBase
    {
        [XmlElement("evtContratAvNP")]
        public EvtContratAvNP EvtContratAvNP { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtContratAvNP")]
    [ComVisible(true)]
#endif
    public class EvtContratAvNP
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEventoESocial1210 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("remunAvNP")]
        public RemunAvNP RemunAvNP { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunAvNP")]
    [ComVisible(true)]
#endif
    public class RemunAvNP
    {
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NnrInsc { get; set; }

        [XmlElement("codLotacao")]
        public string CodLotacao { get; set; }

        [XmlElement("vrBcCp00")]
        public double VrBcCp00 { get; set; }

        [XmlElement("vrBcCp15")]
        public double VrBcCp15 { get; set; }

        [XmlElement("vrBcCp20")]
        public double VrBcCp20 { get; set; }

        [XmlElement("vrBcCp25")]
        public double VrBcCp25 { get; set; }

        [XmlElement("vrBcCp13")]
        public double VrBcCp13 { get; set; }

        [XmlElement("vrBcFgts")]
        public double VrBcFgts { get; set; }

        [XmlElement("vrDescCP")]
        public double VrDescCP { get; set; }
    }
}
