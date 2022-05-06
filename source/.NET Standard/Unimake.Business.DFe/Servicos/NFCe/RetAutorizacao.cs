#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFCe
{
    /// <summary>
    /// Enviar o XML da consulta recibo do lote da NFCe para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFCe.RetAutorizacao")]
    [ComVisible(true)]
#endif
    public class RetAutorizacao: NFe.RetAutorizacao
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="consReciNFe">Objeto do XML da consulta recibo da NFCe</param>
        /// <param name="configuracao">Configurações a serem utilizadas para conexão e envio do XML para o web-service</param>
        public RetAutorizacao(ConsReciNFe consReciNFe, Configuracao configuracao) : base(consReciNFe, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public RetAutorizacao() : base() { }

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