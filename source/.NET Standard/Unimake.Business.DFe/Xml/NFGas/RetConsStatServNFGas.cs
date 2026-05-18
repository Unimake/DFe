#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFGas
{
    /// <summary>
    /// Retorno da consulta status do serviço da NFGas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.RetConsStatServNFGas")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfgas")]
    [XmlRoot("retConsStatServNFGas", Namespace = "http://www.portalfiscal.inf.br/nfgas", IsNullable = false)]
    public class RetConsStatServNFGas : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("tpAmb")]
        public int TpAmb { get; set; }

        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        [XmlElement("cStat")]
        public int CStat { get; set; }

        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        [XmlElement("cUF")]
        public int CUF { get; set; }

        [XmlElement("dhRecbto")]
        public string DhRecbto { get; set; }

        [XmlElement("tMed")]
        public int TMed { get; set; }

        public bool ShouldSerializeTMed() => TMed > 0;
    }
}
