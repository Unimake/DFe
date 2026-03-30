using System.IO;
using System.Net;
using System.Net.Security;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using Unimake.Business.DFe.ConsumirServico.Contracts;

namespace Unimake.Business.DFe.ConsumirServico.Transport
{
    internal sealed class SoapTransportExecutor
    {
        public TransportResponse Execute(TransportRequest request)
        {
            ServicePointManager.Expect100Continue = request.Expect100Continue;
            ServicePointManager.ServerCertificateValidationCallback = new RemoteCertificateValidationCallback(RetornoValidacao);
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12 | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls;

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
