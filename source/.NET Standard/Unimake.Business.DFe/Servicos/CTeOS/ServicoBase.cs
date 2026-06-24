#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CTeOS
{
    /// <summary>
    /// Classe base para consumo dos webservices do CTeOS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CTeOS.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase : NFe.ServicoBase
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public ServicoBase() : base() { }

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
