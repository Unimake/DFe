#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFCe
{
    /// <summary>
    /// Enviar o XML de consulta protocolo da NFCe para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFCe.ConsultaProtocolo")]
    [ComVisible(true)]
#endif
    public class ConsultaProtocolo : NFe.ConsultaProtocolo
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="consSitNFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaProtocolo(ConsSitNFe consSitNFe, Configuracao configuracao) : base(consSitNFe, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaProtocolo(string conteudoXML, Configuracao configuracao) : base(conteudoXML, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultaProtocolo() : base() { }

        ///<summary>
        ///Construtor simplificado para uso de API
        /// </summary>
        /// <param name="chaveNFe">Chave de acesso NFe</param>
        /// <param name="tipoAmbiente">Ambiente de Produção ou Homologação</param>
        /// <param name="configuracao">Config para conexão e envio de XML</param>
        public ConsultaProtocolo(string chaveNFe, TipoAmbiente tipoAmbiente, Configuracao configuracao) : base(chaveNFe, tipoAmbiente, configuracao) { }

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