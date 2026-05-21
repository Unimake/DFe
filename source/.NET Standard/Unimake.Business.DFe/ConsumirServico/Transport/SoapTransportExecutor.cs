using System;
using System.IO;
using System.Net;
using System.Net.Security;
using System.Net.Sockets;
using System.Security.Authentication;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using Unimake.Business.DFe.ConsumirServico.Contracts;

namespace Unimake.Business.DFe.ConsumirServico.Transport
{
    internal sealed class SoapTransportExecutor
    {
        /// <summary>
        /// Executa uma requisição SOAP com suporte a uma nova tentativa automática em falhas transitórias de handshake SSL/TLS.
        /// </summary>
        /// <param name="request">Configuração completa da requisição de transporte SOAP.</param>
        /// <returns>Retorno do transporte com status HTTP, conteúdo de resposta e exceção de rede quando aplicável.</returns>
        public TransportResponse Execute(TransportRequest request)
        {
            ServicePointManager.Expect100Continue = request.Expect100Continue;
            ServicePointManager.ServerCertificateValidationCallback = new RemoteCertificateValidationCallback(RetornoValidacao);
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12 | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls;

            return ExecuteInternal(request, allowRetryOnSslHandshakeFailure: true);
        }

        /// <summary>
        /// Executa internamente a chamada SOAP e realiza uma única nova tentativa quando a primeira falha por handshake SSL/TLS transitório.
        /// </summary>
        /// <param name="request">Configuração completa da requisição de transporte SOAP.</param>
        /// <param name="allowRetryOnSslHandshakeFailure">Indica se uma nova tentativa pode ser realizada ao detectar falha de handshake SSL/TLS.</param>
        /// <returns>Retorno do transporte com status HTTP, conteúdo de resposta e exceção de rede quando aplicável.</returns>
        private TransportResponse ExecuteInternal(TransportRequest request, bool allowRetryOnSslHandshakeFailure)
        {
            var httpWebRequest = CreateHttpWebRequest(request);

            var payloadBytes = Encoding.UTF8.GetBytes(request.Body);
            httpWebRequest.ContentLength = payloadBytes.Length;

            using (var postData = httpWebRequest.GetRequestStream())
            {
                postData.Write(payloadBytes, 0, payloadBytes.Length);
            }

            WebException webException = null;
            HttpWebResponse responsePost = null;

            try
            {
                responsePost = (HttpWebResponse)httpWebRequest.GetResponse();
            }
            catch (WebException ex) when (allowRetryOnSslHandshakeFailure && IsSslHandshakeFailure(ex))
            {
                return ExecuteInternal(request, allowRetryOnSslHandshakeFailure: false);
            }
            catch (WebException ex)
            {
                webException = ex;
                responsePost = ex.Response as HttpWebResponse;

                if (ex.Response == null)
                {
                    throw;
                }
            }

            using (responsePost)
            using (var streamPost = responsePost.GetResponseStream())
            using (var streamReaderResponse = new StreamReader(streamPost, GetEncodingSafe(request.ResponseEncoding)))
            {
                return new TransportResponse
                {
                    StatusCode = responsePost?.StatusCode ?? (HttpStatusCode)webException.Status,
                    Content = streamReaderResponse.ReadToEnd(),
                    WebException = webException
                };
            }
        }

        /// <summary>
        /// Cria e configura a instância de <see cref="HttpWebRequest"/> com os mesmos parâmetros utilizados no envio SOAP.
        /// </summary>
        /// <param name="request">Configuração completa da requisição de transporte SOAP.</param>
        /// <returns>Instância configurada de <see cref="HttpWebRequest"/> pronta para envio.</returns>
        private HttpWebRequest CreateHttpWebRequest(TransportRequest request)
        {
            var httpWebRequest = (HttpWebRequest)HttpWebRequest.Create(request.RequestUri);

            foreach (var header in request.Headers)
            {
                httpWebRequest.Headers.Add(header.Key, header.Value);
            }

            httpWebRequest.CookieContainer = request.Cookies;
            httpWebRequest.Timeout = request.Timeout;
            httpWebRequest.ContentType = request.ContentType;
            httpWebRequest.Method = request.Method;

            if (request.UseCertificate)
            {
                httpWebRequest.ClientCertificates.Add(request.Certificate);
            }

            if (request.Proxy != null)
            {
                httpWebRequest.Proxy = request.Proxy;
            }

            return httpWebRequest;
        }

        /// <summary>
        /// Verifica se a exceção de rede corresponde a uma falha de handshake SSL/TLS potencialmente transitória.
        /// </summary>
        /// <param name="webException">Exceção de rede retornada na tentativa de comunicação.</param>
        /// <returns>Retorna <c>true</c> quando o erro indica handshake SSL/TLS; caso contrário, <c>false</c>.</returns>
        private bool IsSslHandshakeFailure(WebException webException)
        {
            if (webException == null)
            {
                return false;
            }

            if (webException.Status == WebExceptionStatus.SecureChannelFailure)
            {
                return true;
            }

            if (webException.Status == WebExceptionStatus.TrustFailure)
            {
                return true;
            }

            if (HasExceptionType<AuthenticationException>(webException))
            {
                return true;
            }

            if (HasSslHandshakeMessage(webException))
            {
                return true;
            }

            if (webException.Response == null &&
                (webException.Status == WebExceptionStatus.ReceiveFailure ||
                 webException.Status == WebExceptionStatus.SendFailure ||
                 webException.Status == WebExceptionStatus.ConnectFailure) &&
                HasExceptionType<IOException>(webException) &&
                HasExceptionType<SocketException>(webException))
            {
                return true;
            }

            return false;
        }

        /// <summary>
        /// Verifica se uma exceção ou qualquer exceção interna é de um tipo específico.
        /// </summary>
        /// <typeparam name="TException">Tipo de exceção que será pesquisado na cadeia de inner exceptions.</typeparam>
        /// <param name="exception">Exceção raiz para análise.</param>
        /// <returns>Retorna <c>true</c> quando encontrar o tipo informado na cadeia de exceções; caso contrário, <c>false</c>.</returns>
        private bool HasExceptionType<TException>(Exception exception) where TException : Exception
        {
            while (exception != null)
            {
                if (exception is TException)
                {
                    return true;
                }

                exception = exception.InnerException;
            }

            return false;
        }

        /// <summary>
        /// Verifica se a mensagem da exceção ou de qualquer exceção interna indica falha de handshake SSL/TLS.
        /// </summary>
        /// <param name="exception">Exceção raiz para análise textual da cadeia de mensagens.</param>
        /// <returns>Retorna <c>true</c> quando encontrar mensagens típicas de falha SSL/TLS; caso contrário, <c>false</c>.</returns>
        private bool HasSslHandshakeMessage(Exception exception)
        {
            while (exception != null)
            {
                if (!string.IsNullOrWhiteSpace(exception.Message) &&
                    (exception.Message.IndexOf("SSL connection could not be established", StringComparison.OrdinalIgnoreCase) >= 0 ||
                     exception.Message.IndexOf("SSL", StringComparison.OrdinalIgnoreCase) >= 0 ||
                     exception.Message.IndexOf("TLS", StringComparison.OrdinalIgnoreCase) >= 0))
                {
                    return true;
                }

                exception = exception.InnerException;
            }

            return false;
        }

        private bool RetornoValidacao(object sender, X509Certificate certificate, X509Chain chain, SslPolicyErrors sslPolicyErros) => true;

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
