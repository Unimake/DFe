#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CTeOS
{
    /// <summary>
    /// Envio do XML de Consulta Cadastro do Contribuinte para o Webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CTeOS.ConsultaCadastro")]
    [ComVisible(true)]
#endif
    public class ConsultaCadastro : NFe.ConsultaCadastro
    {
        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="consCad">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaCadastro(ConsCadBase consCad, Configuracao configuracao) : base(consCad, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultaCadastro() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaCadastro(string conteudoXML, Configuracao configuracao) : base(conteudoXML, configuracao) { }

        ///<summary>
        ///Construtor simplificado para uso de API
        /// </summary>
        /// <param name="ufBrasil">UF do contribuinte</param>
        /// <param name="cnpj">CNPJ do contribuinte</param>
        /// <param name="configuracao">Config para conexão e envio do XML</param>
        public ConsultaCadastro(UFBrasil ufBrasil, string cnpj, Configuracao configuracao) : base(ufBrasil, cnpj, configuracao) { }

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