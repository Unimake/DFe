#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFCe
{
    /// <summary>
    /// Enviar o XML de inutilização de NFCe para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFCe.Inutilizacao")]
    [ComVisible(true)]
#endif
    public class Inutilizacao : NFe.Inutilizacao
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="inutNFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public Inutilizacao(InutNFe inutNFe, Configuracao configuracao) : base(inutNFe, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public Inutilizacao() : base() { }

        #endregion Public Constructors

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            var validar = new ValidarSchema();
            validar.Validar(ConteudoXML, TipoDFe.NFe.ToString() + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

            if (!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }
    }
}