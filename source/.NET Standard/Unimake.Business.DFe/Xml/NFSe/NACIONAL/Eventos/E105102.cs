#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos
{
    /// <summary>
    /// Bloco do evento de Cancelamento por Substiuição de NFSe (e105102).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos.E105102")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("e105102", Namespace = NfseNs.Ns)]
    public class E105102
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
        public CodigoJustificativaSubstituicaoSubstituicao CMotivo { get; set; }

        /// <summary>
        /// Descrição do motivo do cancelamento.
        /// </summary>
        [XmlElement("xMotivo", Namespace = NfseNs.Ns)]
        public string XMotivo { get; set; }

        /// <summary>
        /// Chave de Acesso da NFS-e substituta.
        /// </summary>
        [XmlElement("chSubstituta", Namespace = NfseNs.Ns)]
        public string ChSubstituta { get; set; }
    }
}