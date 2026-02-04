#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos
{
    /// <summary>
    /// Bloco do evento de Rejeição do Pestador (e202205).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos.E2202205")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("e202205", Namespace = NfseNs.Ns)]
    public class E202205
    {
        /// <summary>
        /// Descrição do evento.
        /// </summary>
        [XmlElement("xDesc", Namespace = NfseNs.Ns)]
        public string XDesc { get; set; }

        /// <summary>
        /// Código do Motivo da Rejeição
        /// </summary>
        [XmlElement("cMotivo", Namespace = NfseNs.Ns)]
        public CodigoMotivoRejeicao CMotivo { get; set; }

        /// <summary>
        /// Descrição para explicitar o motivo indicado neste evento
        /// </summary>
        [XmlElement("xMotivo", Namespace = NfseNs.Ns)]
        public string XMotivo { get; set; }
    }
}