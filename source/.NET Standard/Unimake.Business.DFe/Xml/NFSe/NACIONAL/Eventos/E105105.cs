#pragma warning disable CS1591
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos
{
    /// <summary>
    /// Bloco do Evento de Cancelamento por Análise Fiscal (e105105).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos.E105105")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("e105105", Namespace = NfseNs.Ns)]
    public class E105105
    {
        /// <summary>
        /// Descrição do evento.
        /// </summary>
        [XmlElement("xDesc", Namespace = NfseNs.Ns)]
        public string XDesc { get; set; }

        /// <summary>
        /// CPF do Agente de Tributos que efeitou a análise fiscal.
        /// </summary>
        [XmlElement("CPFAgTrib", Namespace = NfseNs.Ns)]
        public string CPFAgTrib { get; set; }

        /// <summary>
        /// Número do processo administrativo municipal vinculado à solicitação de cancelamento extemporâneo de NFS-e.
        /// </summary>
        [XmlElement("nProcAdm", Namespace = NfseNs.Ns)]
        public string NProcAdm { get; set; }

        /// <summary>
        /// Código do motivo da análise fiscal que resultou na solicitação de cancelamento da NFS-e.
        /// </summary>
        [XmlElement("cMotivo", Namespace = NfseNs.Ns)]
        public CodJustAnaliseFiscalCancIndef CMotivo { get; set; }

        /// <summary>
        /// Descrição do motivo da análise fiscal que resultou na solicitação de cancelamento da NFS-e.
        /// </summary>
        [XmlElement("xMotivo", Namespace = NfseNs.Ns)]
        public string XMotivo { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeNProcAdm() => !string.IsNullOrEmpty(NProcAdm);
        #endregion ShouldSerialize
    }
}