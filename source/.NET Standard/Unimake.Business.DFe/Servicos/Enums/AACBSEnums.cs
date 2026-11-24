using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;
using Unimake.Business.DFe.Xml.AACBS;

namespace Unimake.Business.DFe.Servicos
{
    /// <summary>
    /// Situações publicadas para uma solicitação assíncrona da AACBS.
    /// </summary>
#if INTEROP
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = AACBSFormat.Namespace)]
    public enum SituacaoSolicitacaoAACBS
    {
        /// <summary>
        /// Solicitação pendente.
        /// </summary>
        [XmlEnum("PENDENTE")]
        Pendente = 1,

        /// <summary>
        /// Solicitação em processamento.
        /// </summary>
        [XmlEnum("EM_PROCESSAMENTO")]
        EmProcessamento = 2,

        /// <summary>
        /// Solicitação concluída.
        /// </summary>
        [XmlEnum("CONCLUIDA")]
        Concluida = 3,

        /// <summary>
        /// Solicitação encerrada com erro.
        /// </summary>
        [XmlEnum("ERRO")]
        Erro = 4,

        /// <summary>
        /// Solicitação expirada.
        /// </summary>
        [XmlEnum("EXPIRADA")]
        Expirada = 5
    }

    /// <summary>
    /// Famílias de arquivo de uma consulta da AACBS.
    /// </summary>
#if INTEROP
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = AACBSFormat.Namespace)]
    public enum TipoConsultaAACBS
    {
        /// <summary>
        /// Consulta de débitos.
        /// </summary>
        [XmlEnum("DEBITOS")]
        Debitos = 1,

        /// <summary>
        /// Consulta de créditos.
        /// </summary>
        [XmlEnum("CREDITOS")]
        Creditos = 2,

        /// <summary>
        /// Consulta de pagamentos.
        /// </summary>
        [XmlEnum("PAGAMENTOS")]
        Pagamentos = 3,

        /// <summary>
        /// Consulta de recolhimentos.
        /// </summary>
        [XmlEnum("RECOLHIMENTOS")]
        Recolhimentos = 4
    }
}
