using System;
using Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Servicos.NFCe
{
    /// <summary>
    /// Enviar o XML de inutilização de NFCe para o webservice
    /// </summary>
    public class Inutilizacao: NFe.Inutilizacao
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="inutNFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public Inutilizacao(InutNFe inutNFe, Configuracao configuracao)
            : base(inutNFe, configuracao)
        {
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public Inutilizacao()
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
                throw new Exception(validar.ErrorMessage);
            }
        }
    }
}