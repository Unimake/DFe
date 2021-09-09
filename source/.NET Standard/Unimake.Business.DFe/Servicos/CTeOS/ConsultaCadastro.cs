using System;
using Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Servicos.CTeOS
{
    /// <summary>
    /// Envio do XML de Consulta Cadastro do Contribuinte para o Webservice
    /// </summary>
    public class ConsultaCadastro: NFe.ConsultaCadastro
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="consCad">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public ConsultaCadastro(ConsCadBase consCad, Configuracao configuracao)
            : base(consCad, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultaCadastro()
        {
        }

        #endregion Public Constructors

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            var validar = new ValidarSchema();
            validar.Validar(ConteudoXML, TipoDFe.CTe.ToString() + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

            if(!validar.Success)
            {
                throw new Exception(validar.ErrorMessage);
            }
        }
    }
}