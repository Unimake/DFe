#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.PIX
{
    /// <summary>
    /// Retorno da consulta de PIX
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.PIX.retPIXConsultar")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("PIXGetResponse", IsNullable = false)]
    public class retPIXConsultar : XMLBase
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
        /// Identificador da transação PIX
        /// </summary>
        [XmlElement(Order = 2)]
        public string TxId { get; set; }

        /// <summary>
        /// Valor do PIX
        /// </summary>
        [XmlElement(Order = 3)]
        public string Valor { get; set; }

        /// <summary>
        /// Data e hora do PIX
        /// </summary>
        [XmlElement(Order = 4)]
        public string Horario { get; set; }

        /// <summary>
        /// Dados do pagador
        /// </summary>
        [XmlElement(Order = 5)]
        public retPIXPagador Pagador { get; set; }

        /// <summary>
        /// Versão da DLL
        /// </summary>
        [XmlElement(Order = 6)]
        public string DLLVersao { get; set; }
    }
}
