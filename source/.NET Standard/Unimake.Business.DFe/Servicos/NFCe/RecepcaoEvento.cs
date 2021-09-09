using System;
using Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Servicos.NFCe
{
    /// <summary>
    /// Enviar o XML de eventos da NFCe para o webservice
    /// </summary>
    public class RecepcaoEvento: NFe.RecepcaoEvento
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="envEvento">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public RecepcaoEvento(EnvEvento envEvento, Configuracao configuracao)
                    : base(envEvento, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public RecepcaoEvento()
        {
        }

        #endregion Public Constructors
    }
}