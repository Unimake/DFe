using Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Servicos.CTe
{
    /// <summary>
    /// Envio do XML de consulta cadastro do contribuinte para o WebService
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
    }
}