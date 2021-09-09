using System;
using System.Runtime.InteropServices;
using Unimake.Business.DFe.Xml.CTe;

namespace Unimake.Business.DFe.Servicos.CTeOS
{
    /// <summary>
    /// Envio do XML de consulta status do serviço do CTeOS para o WebService
    /// </summary>
    [ComVisible(true)]
    public class StatusServico: CTe.StatusServico
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="consStatServ">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public StatusServico(ConsStatServCte consStatServ, Configuracao configuracao)
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
            validar.Validar(ConteudoXML, TipoDFe.CTe.ToString() + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

            if(!validar.Success)
            {
                throw new Exception(validar.ErrorMessage);
            }
        }
    }
}