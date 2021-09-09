using System;
using Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Servicos.NFCe
{
    /// <summary>
    /// Enviar o XML da consulta recibo do lote da NFCe para o webservice
    /// </summary>
    public class RetAutorizacao: NFe.RetAutorizacao
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="consReciNFe">Objeto do XML da consulta recibo da NFCe</param>
        /// <param name="configuracao">Configurações a serem utilizadas para conexão e envio do XML para o webservice</param>
        public RetAutorizacao(ConsReciNFe consReciNFe, Configuracao configuracao)
            : base(consReciNFe, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public RetAutorizacao()
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