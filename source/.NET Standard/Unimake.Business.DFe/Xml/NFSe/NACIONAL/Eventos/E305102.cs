#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos
{
    /// <summary>
    /// Bloco do evento de Bloqueio por Ofício (e305102).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos.E305102")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("e305102", Namespace = NfseNs.Ns)]
    public class E305102
    {
        /// <summary>
        /// Descrição do evento.
        /// </summary>
        [XmlElement("xDesc", Namespace = NfseNs.Ns)]
        public string XDesc { get; set; }

        ///<summary>
        /// CPF do agente da administração tributária municipal que efetuou o anulação da manifestação de rejeição da NFS-e
        /// </summary>
        [XmlElement("CPFAgTrib", Namespace = NfseNs.Ns)]
        public string CPFAgTrib { get; set; }

        /// <summary>
        /// Referência ao "id" do Evento de Manifestação de NFS-e - Rejeição, que originou o presente evento de anulação
        /// </summary>
        [XmlElement("codEvento", Namespace = NfseNs.Ns)]
        public CodigoEventoNFSe CodEvento { get; set; }

        /// <summary>
        /// Descrição para explicitar o motivo indicado neste evento
        /// </summary>
        [XmlElement("xMotivo", Namespace = NfseNs.Ns)]
        public string XMotivo { get; set; }
    }
}