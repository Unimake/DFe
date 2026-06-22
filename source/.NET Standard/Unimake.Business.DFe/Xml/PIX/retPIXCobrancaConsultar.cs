#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.PIX
{
    /// <summary>
    /// Retorno da consulta de cobrança PIX
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.PIX.retPIXCobrancaConsultar")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("PIXConsultaResponse", IsNullable = false)]
    public class retPIXCobrancaConsultar : XMLBase
    {
        /// <summary>
        /// Status do retorno
        /// </summary>
        [XmlElement(Order = 0)]
        public int Status { get; set; }

        /// <summary>
        /// Motivo do retorno
        /// </summary>
        [XmlElement(Order = 1)]
        public string Motivo { get; set; }

        /// <summary>
        /// Lista de itens encontrados
        /// </summary>
        [XmlArray("Items", Order = 2)]
        [XmlArrayItem("Item")]
        public List<retPIXItem> Items { get; set; } = new List<retPIXItem>();

        /// <summary>
        /// Versão da DLL
        /// </summary>
        [XmlElement(Order = 3)]
        public string DLLVersao { get; set; }
    }
}
