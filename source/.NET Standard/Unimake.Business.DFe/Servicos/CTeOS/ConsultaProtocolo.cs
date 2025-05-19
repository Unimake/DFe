#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;
using System;
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CTeOS
{
    /// <summary>
    /// Envio do XML de Consulta Protocolo do CTeOS para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CTeOS.ConsultaProtocolo")]
    [ComVisible(true)]
#endif
    public class ConsultaProtocolo : CTe.ConsultaProtocolo
    {
        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="consSitNFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaProtocolo(ConsSitCTe consSitNFe, Configuracao configuracao) : base(consSitNFe, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultaProtocolo() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaProtocolo(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            Inicializar(doc, configuracao);
        }

        ///<summary>
        ///Constutor simplificado para API
        /// </summary>
        /// <param name="chaveCTe">Chave de Acesso do CTe</param>
        /// <param name="tipoAmbiente">Ambiente de Produção ou Homologação</param>
        /// <param name="configuracao">Configuração para conexão e envio de XML</param>
        public ConsultaProtocolo(string chaveCTe, TipoAmbiente tipoAmbiente, Configuracao configuracao) : base(chaveCTe, tipoAmbiente, configuracao) { }

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