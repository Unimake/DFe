#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos
{
    /// <summary>
    /// Bloco do evento de Rejeição do Intermediário (e204207).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos.E204207")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("e204207", Namespace = NfseNs.Ns)]
    public class E204207
    {
        /// <summary>
        /// Descrição do evento.
        /// </summary>
        [XmlElement("xDesc", Namespace = NfseNs.Ns)]
        public string XDesc { get; set; }

        /// <summary>
        /// Códogo do motivo de rejeição da NFS-e pelo Intermediário.
        /// </summary>
        [XmlElement("cMotivo", Namespace = NfseNs.Ns)]
        public CodigoMotivoRejeicao CMotivo { get; set; }

        /// <summary>
        /// Descrição para explicar o motivo neste evento
        /// </summary>
        [XmlElement("xMotivo", Namespace = NfseNs.Ns)]
        public string XMotivo { get; set; }
    }
}