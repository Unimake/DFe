#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos
{
    /// <summary>
    /// Bloco do evento de Solicitação de Análise Fiscal para Cancelamento (e105104).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos.E105104")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("e105104", Namespace = NfseNs.Ns)]
    public class E105104
    {
        /// <summary>
        /// Cancelamento de NFS-e Deferido por Análise Fiscal.
        /// </summary>
        [XmlElement("xDesc", Namespace = NfseNs.Ns)]
        public string XDesc { get; set; }

        /// <summary>
        /// CPF do agente da administração tributária municipal que efetuou o deferimento da solicitação de análise fiscal para cancelamento de NFS-e.
        /// </summary>
        [XmlElement("CPFAgTrib", Namespace = NfseNs.Ns)]
        public string CPFAgTrib { get; set; }

        /// <summary>
        /// Número do processo administrativo municipal vinculado à solicitação de análise fiscal para cancelamento de NFS-e.
        /// </summary>
        [XmlElement("nProcAdm", Namespace = NfseNs.Ns)]
        public string nProcAdm { get; set; }

        /// <summary>
        /// Resposta da solicitação de análise fiscal para cancelamento de NFS-e.
        /// </summary>
        [XmlElement("cMotivo", Namespace = NfseNs.Ns)]
        public CodJustAnaliseFiscalCancDef CMotivo { get; set; }

        /// <summary>
        /// Descrição para explicitar o motivo indicado neste evento.
        /// </summary>
        [XmlElement("xMotivo", Namespace = NfseNs.Ns)]
        public string XMotivo { get; set; }
    }
}