using System;
using System.Net;
using System.Net.Http;
using Unimake.Business.DFe.ConsumirServico.Contracts;

namespace Unimake.Business.DFe.ConsumirServico.Transport
{
    internal sealed class ApiTransportExecutor
    {
        public TransportResponse Execute(TransportRequest request)
        {
            using (var httpClient = CreateClient(request))
            {
                var httpResponse = string.Equals(request.Method, "get", StringComparison.CurrentCultureIgnoreCase)
                    ? httpClient.GetAsync("").GetAwaiter().GetResult()
                    : httpClient.PostAsync(request.RequestUri, request.HttpContent).GetAwaiter().GetResult();

                return new TransportResponse
                {
                    StatusCode = httpResponse.StatusCode,
                    HttpResponseMessage = httpResponse
                };
            }
        }

        private HttpClient CreateClient(TransportRequest request)
        {
            var httpClientHandler = new HttpClientHandler();

            if (!request.UseCertificate)
            {
                httpClientHandler.ClientCertificateOptions = ClientCertificateOption.Automatic;
                if (request.UseDefaultCredentials)
                {
                    httpClientHandler.Credentials = CredentialCache.DefaultCredentials;
                }
            }
            else
            {
                httpClientHandler.ClientCertificateOptions = ClientCertificateOption.Manual;
                httpClientHandler.ClientCertificates.Add(request.Certificate);
            }

            var client = new HttpClient(httpClientHandler)
            {
                BaseAddress = new Uri(request.RequestUri)
            };

            foreach (var header in request.Headers)
            {
                client.DefaultRequestHeaders.Add(header.Key, header.Value);
            }

            ServicePointManager.Expect100Continue = request.Expect100Continue;
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12 | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls;

            return client;
        }
    }
}
