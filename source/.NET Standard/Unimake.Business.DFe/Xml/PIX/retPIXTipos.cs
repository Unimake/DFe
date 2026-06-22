#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.PIX
{
    /// <summary>
    /// Item da consulta de cobrança PIX
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.PIX.retPIXItem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class retPIXItem
    {
        /// <summary>
        /// Identificador do item
        /// </summary>
        [XmlAttribute]
        public string Id { get; set; }

        /// <summary>
        /// Identificador da transação PIX
        /// </summary>
        [XmlElement(Order = 0)]
        public string TxId { get; set; }

        /// <summary>
        /// Valor do PIX
        /// </summary>
        [XmlElement(Order = 1)]
        public string Valor { get; set; }

        /// <summary>
        /// Data e hora do PIX
        /// </summary>
        [XmlElement(Order = 2)]
        public string Horario { get; set; }

        /// <summary>
        /// Dados do pagador
        /// </summary>
        [XmlElement(Order = 3)]
        public retPIXPagador Pagador { get; set; }
    }

    /// <summary>
    /// Dados do pagador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.PIX.retPIXPagador")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class retPIXPagador
    {
        /// <summary>
        /// Nome do pagador
        /// </summary>
        [XmlElement(Order = 0)]
        public string Nome { get; set; }

        /// <summary>
        /// Inscrição do pagador
        /// </summary>
        [XmlElement(Order = 1)]
        public string Inscricao { get; set; }
    }
}
