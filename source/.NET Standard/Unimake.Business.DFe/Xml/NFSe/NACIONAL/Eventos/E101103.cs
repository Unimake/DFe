#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos
{
    /// <summary>
    /// Bloco do evento de Solicitação de Análise Fiscal para Cancelamento (e101103).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos.E101103")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("e101103", Namespace = NfseNs.Ns)]
    public class E101103
    {
        /// <summary>
        /// Solicitação de Análise Fiscal para Cancelamento de NFS-e.
        /// </summary>
        [XmlElement("xDesc", Namespace = NfseNs.Ns)]
        public string XDesc { get; set; }

        /// <summary>
        /// Código do motivo da solicitação de análise fiscal para cancelamento de NFS-e.
        /// </summary>
        [XmlElement("cMotivo", Namespace = NfseNs.Ns)]
        public CodigoMotivoSolicitacaoAnaliseFiscalCancelamento CMotivo { get; set; }

        /// <summary>
        /// Descrição para explicitar o motivo indicado neste evento.
        /// </summary>
        [XmlElement("xMotivo", Namespace = NfseNs.Ns)]
        public string XMotivo { get; set; }
    }
}