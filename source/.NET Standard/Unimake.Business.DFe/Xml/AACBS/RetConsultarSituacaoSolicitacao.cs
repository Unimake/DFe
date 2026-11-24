using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Resultado da consulta da situação de uma solicitação da AACBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.RetConsultarSituacaoSolicitacao")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "RetConsultarSituacaoSolicitacaoTipo", Namespace = AACBSFormat.Namespace)]
    [XmlRoot("retConsultarSituacaoSolicitacao", Namespace = AACBSFormat.Namespace, IsNullable = false)]
    public class RetConsultarSituacaoSolicitacao : RetornoAACBSBase
    {
        /// <summary>
        /// Situação da solicitação.
        /// </summary>
        [XmlElement("situacao")]
        public SituacaoSolicitacaoAACBS Situacao { get; set; }

        /// <summary>
        /// Indica se a propriedade Situacao foi informada.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public bool ShouldSerializeSituacao() => Situacao != 0;

        /// <summary>
        /// Código de erro informado pela Receita Federal.
        /// </summary>
        [XmlElement("codigoErro")]
        public string CodigoErro { get; set; }

        /// <summary>
        /// Mensagem de erro informada pela Receita Federal.
        /// </summary>
        [XmlElement("mensagemErro")]
        public string MensagemErro { get; set; }
    }
}
