#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Text;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Classe da consulta do recido do lote de NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ConsReciNFe")]
    [ComVisible(true)]
#endif
    [XmlRoot("consReciNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class ConsReciNFe: XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de consulta recibo da NFe/NFCe
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }
        
        /// <summary>
        /// Tipo do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Número do recibo
        /// </summary>
        [XmlElement("nRec")]
        public string NRec { get; set; }
    }
}
