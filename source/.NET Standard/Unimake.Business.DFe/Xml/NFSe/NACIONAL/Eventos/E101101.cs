#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos
{
    /// <summary>
    /// Bloco do evento de Cancelamento da NFS-e (e101101).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos.E101101")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("e101101", Namespace = NfseNs.Ns)]
    public class E101101
    {
        /// <summary>
        /// Descrição do evento.
        /// </summary>
        [XmlElement("xDesc", Namespace = NfseNs.Ns)]
        public string XDesc { get; set; }

        /// <summary>
        /// Código do motivo do cancelamento.
        /// </summary>
        [XmlElement("cMotivo", Namespace = NfseNs.Ns)]
        public CodigoJustificativaCancelamento CMotivo { get; set; }

        /// <summary>
        /// Descrição do motivo do cancelamento.
        /// </summary>
        [XmlElement("xMotivo", Namespace = NfseNs.Ns)]
        public string XMotivo { get; set; }
    }
}