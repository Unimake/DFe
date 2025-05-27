#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFCe
{
    /// <summary>
    /// Enviar o XML de consulta cadastro do contribuinte para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFCe.ConsultaCadastro")]
    [ComVisible(true)]
#endif
    public class ConsultaCadastro: NFe.ConsultaCadastro
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="consCad">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaCadastro(ConsCad consCad, Configuracao configuracao) : base(consCad, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaCadastro(string conteudoXML, Configuracao configuracao) : base(conteudoXML, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultaCadastro() : base() { }

        ///<summary>
        ///Constutor simplificado para API
        /// </summary>
        /// <param name="ufBrasil">UF do contribuinte</param>
        /// <param name="cnpj">CNPJ do contribuinte</param>
        /// <param name="configuracao">Configuração para conexão e envio do XML</param>
        public ConsultaCadastro(UFBrasil ufBrasil, string cnpj, Configuracao configuracao) : base(ufBrasil, cnpj, configuracao) { }

        #endregion Public Constructors

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            var validar = new ValidarSchema();
            validar.Validar(ConteudoXML, TipoDFe.NFe.ToString() + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

            if(!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }
    }
}