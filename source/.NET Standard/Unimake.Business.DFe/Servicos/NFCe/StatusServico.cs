using System;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Security.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFCe
{
    /// <summary>
    /// Enviar o XML de consulta status do serviço da NFCe para o webservice
    /// </summary>
    public class StatusServico: NFe.StatusServico
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="consStatServ">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public StatusServico(ConsStatServ consStatServ, Configuracao configuracao)
            : base(consStatServ, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public StatusServico()
        {
        }

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