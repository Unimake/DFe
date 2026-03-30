using System;
using System.IO;
using System.Net;
using System.Net.Security;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.ConsumirServico.Builders;
using Unimake.Business.DFe.ConsumirServico.Compatibility;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Parsers;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.Exceptions;

namespace Unimake.Business.DFe
{
    /// <summary>
    /// Classe para consumir webservices e API´s
    /// </summary>
    public abstract class ConsumirBase : IDisposable
    {
        private bool _disposed = false;
        #region Private Fields

        /// <summary>
        /// Objeto para leitura de cookies
        /// </summary>
        private readonly CookieContainer cookies = new CookieContainer();

        private bool tratarScapeEnvio;
        private bool tratarScapeRetorno;

        #endregion Private Fields

        #region Private Methods

        #endregion Private Methods

        #region Public Properties

        /// <summary>
        /// Conteudo retornado pelo WebService consumido (formato string)
        /// </summary>
        public string RetornoServicoString { get; protected set; }

        /// <summary>
        /// Conteudo retornado pelo WebService consumido (formato XmlDocument)
        /// </summary>
        public XmlDocument RetornoServicoXML { get; protected set; }

        /// <summary>
        /// Stream retornada pelo Webservice. Para consumo de serviços que retornam .pdf
        /// </summary>
        public Stream RetornoServicoStream { get; protected set; }

        /// <summary>
        /// Propriedade para uso interno nos testes unitários. 
        /// </summary>
        public HttpStatusCode HttpStatusCode { get; protected set; }

        #endregion Public Properties

        #region Public Methods

        /// <summary>
        /// Estabelece conexão com o Webservice e faz o envio do XML e recupera o retorno. Conteúdo retornado pelo webservice pode ser recuperado através das propriedades RetornoServicoXML ou RetornoServicoString.
        /// </summary>
        /// <param name="xml">XML a ser enviado para o webservice</param>
        /// <param name="servico">Parâmetros para execução do serviço (parâmetros do soap)</param>
        /// <param name="certificado">Certificado digital a ser utilizado na conexão com os serviços</param>
        public void ExecutarServico(XmlDocument xml, object servico, X509Certificate2 certificado)
        {
            var soap = (WSSoap)servico;

            if (certificado == null && soap.UsaCertificadoDigital)
            {
                throw new CertificadoDigitalException();
            }

            tratarScapeEnvio = false;
            tratarScapeRetorno = false;

            if (soap.SoapString.IndexOf("{xmlBodyScape}") > 0)
            {
                tratarScapeEnvio = true;
                tratarScapeRetorno = true;
            }
            else if (soap.SoapString.IndexOf("{xmlBodyScapeEnvio}") > 0)
            {
                tratarScapeEnvio = true;
                tratarScapeRetorno = false;
            }
            else if (soap.SoapString.IndexOf("{xmlBodyScapeRetorno}") > 0)
            {
                tratarScapeEnvio = false;
                tratarScapeRetorno = true;
            }

            var soapXML = new SoapEnvelopeBuilder().Build(soap, xml.OuterXml, certificado, new SoapEnvelopeContext
            {
                TratarScapeEnvio = tratarScapeEnvio,
                TratarScapeRetorno = tratarScapeRetorno
            });
            var request = new WSSoapTransportRequestMapper().Map(soap, certificado, soapXML, cookies);

            try
            {
                using (var transportResponse = new SoapTransportExecutor().Execute(request))
                {
                    HttpStatusCode = transportResponse.StatusCode;
                    var resultado = new SoapResponseParser().Parse(new SoapResponseContext
                    {
                        Soap = soap,
                        TransportResponse = transportResponse,
                        TratarScapeRetorno = tratarScapeRetorno,
                        ConteudoRetorno = transportResponse.Content
                    });

                    RetornoServicoString = resultado.RetornoServicoString;
                    RetornoServicoXML = resultado.RetornoServicoXml;
                }
            }
            finally
            {
                // Nada a liberar aqui, pois todos os recursos estão em using
            }
        }

        #endregion Public Methods

        /// <summary>
        /// Implementação do padrão Dispose para liberar recursos.
        /// </summary>
        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        /// <summary>
        /// Dispose protegido para sobrescrita em classes derivadas.
        /// </summary>
        /// <param name="disposing">Indica se está liberando recursos gerenciados.</param>
        protected virtual void Dispose(bool disposing)
        {
            if (_disposed)
                return;

            if (disposing)
            {
                if (RetornoServicoStream != null)
                {
                    RetornoServicoStream.Dispose();
                    RetornoServicoStream = null;
                }
            }

            _disposed = true;
        }

        /// <summary>
        /// Finalizador
        /// </summary>
        ~ConsumirBase()
        {
            Dispose(false);
        }

        /// <summary>
        /// Efetua validações do certificado - Por hora retorna sempre true, ou seja, não estamos validando nada.
        /// </summary>
        /// <param name="sender">Sender</param>
        /// <param name="certificate">Certificado digital</param>
        /// <param name="chain">X509Chain</param>
        /// <param name="sslPolicyErros">Políticas de Erros SSL</param>
        /// <returns>True = Tudo ok na validação - False = Problemas na validação</returns>
        private bool RetornoValidacao(object sender,
           X509Certificate certificate,
           X509Chain chain,
           SslPolicyErrors sslPolicyErros) => true;

        /// <summary>
        /// Obtém o encoding de forma segura, retornando UTF-8 se inválido.
        /// </summary>
        /// <param name="encodingName">Nome do encoding</param>
        private Encoding GetEncodingSafe(string encodingName)
        {
            try
            {
                return Encoding.GetEncoding(encodingName);
            }
            catch
            {
                return Encoding.UTF8;
            }
        }
    }
}
