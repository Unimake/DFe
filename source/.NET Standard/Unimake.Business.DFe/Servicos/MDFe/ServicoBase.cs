#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.MDFe
{
    /// <summary>
    /// Classe base para os servidos de MDFe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.MDFe.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase : NFe.ServicoBase
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public ServicoBase() : base() { }

        #endregion Public Constructors

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            XmlValidarConteudo();

            var resultadoValidacao = ValidarXMLCentralizado();

            if (!resultadoValidacao.Validado)
            {
                throw new ValidarXMLException(resultadoValidacao.MensagemRetorno);
            }
        }
    }
}
