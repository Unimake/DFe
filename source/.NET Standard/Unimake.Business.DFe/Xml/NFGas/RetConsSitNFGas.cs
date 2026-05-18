#pragma warning disable CS1591

#if INTEROP
using System.Collections.Generic;
using System.Runtime.InteropServices;
#else
using System.Collections.Generic;
#endif

using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFGas
{
    /// <summary>
    /// Retorno da consulta situação da NFGas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.RetConsSitNFGas")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfgas")]
    [XmlRoot("retConsSitNFGas", Namespace = "http://www.portalfiscal.inf.br/nfgas", IsNullable = false)]
    public class RetConsSitNFGas : XMLBase
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

        [XmlElement("protNFGas")]
        public ProtNFGas ProtNFGas { get; set; }

        [XmlElement("procEventoNFGas")]
        public List<ProcEventoNFGas> ProcEventoNFGas { get; set; }

        public bool ShouldSerializeProcEventoNFGas() => ProcEventoNFGas?.Count > 0;
    }
}
