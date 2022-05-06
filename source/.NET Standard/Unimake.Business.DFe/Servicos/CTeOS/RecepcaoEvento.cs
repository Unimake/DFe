#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CTeOS
{
    /// <summary>
    /// Envio do XML de eventos do CTeOS para o WebService
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CTeOS.RecepcaoEvento")]
    [ComVisible(true)]
#endif
    public class RecepcaoEvento : CTe.RecepcaoEvento
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="envEvento">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public RecepcaoEvento(EventoCTe envEvento, Configuracao configuracao) : base(envEvento, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public RecepcaoEvento() : base() { }

        #endregion

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            var validar = new ValidarSchema();
            validar.Validar(ConteudoXML, TipoDFe.CTe.ToString() + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

            if (!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }
    }
}