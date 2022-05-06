#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CTeOS
{
    /// <summary>
    /// Envio do XML de Inutilização do CTeOS para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CTeOS.Inutilizacao")]
    [ComVisible(true)]
#endif
    public class Inutilizacao : CTe.Inutilizacao
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="inutCTe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public Inutilizacao(InutCTe inutCTe, Configuracao configuracao) : base(inutCTe, configuracao) { }

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
            validar.Validar(ConteudoXML, TipoDFe.CTe.ToString() + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

            if (!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }
    }
}