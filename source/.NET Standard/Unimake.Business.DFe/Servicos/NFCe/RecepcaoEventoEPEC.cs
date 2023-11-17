#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Servicos.NFCe
{
    /// <summary>
    /// Enviar o XML de evento de EPEC da NFCe para o web-service
    /// *** EXCLUSIVO PARA O ESTADO DE SÃO PAULO, somente eles tem EPEC para NFCe ***
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFCe.RecepcaoEventoEPEC")]
    [ComVisible(true)]
#endif
    public class RecepcaoEventoEPEC : NFe.RecepcaoEvento
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="envEvento">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public RecepcaoEventoEPEC(EnvEvento envEvento, Configuracao configuracao) : base(envEvento, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public RecepcaoEventoEPEC() : base() { }

        #endregion Public Constructors
    }
}