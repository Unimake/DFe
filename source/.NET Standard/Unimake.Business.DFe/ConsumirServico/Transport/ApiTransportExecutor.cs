using System;
using System.Net;
using System.Net.Http;
using System.Runtime.InteropServices;
using Unimake.Business.DFe.ConsumirServico.Contracts;

namespace Unimake.Business.DFe.ConsumirServico.Transport
{
    internal sealed class ApiTransportExecutor : IApiTransportExecutor
    {
        public TransportResponse Execute(TransportRequest request)
        {
            using (var httpClient = CreateClient(request))
            {
                HttpResponseMessage httpResponse;
                if (string.Equals(request.Method, "get", StringComparison.OrdinalIgnoreCase))
                {
                    using (var requestMessage = new HttpRequestMessage(HttpMethod.Get, "") { Content = request.HttpContent })
                    {
                        httpResponse = httpClient.SendAsync(requestMessage).GetAwaiter().GetResult();
                    }
                }
                else if (string.Equals(request.Method, "delete", StringComparison.OrdinalIgnoreCase))
                    httpResponse = httpClient.DeleteAsync("").GetAwaiter().GetResult();
                else
                    httpResponse = httpClient.PostAsync(request.RequestUri, request.HttpContent).GetAwaiter().GetResult();

                return new TransportResponse
                {
                    StatusCode = httpResponse.StatusCode,
                    HttpResponseMessage = httpResponse
                };
            }
        }

        private HttpClient CreateClient(TransportRequest request)
        {
            var handler = CriarHandler(request);
            var client = new HttpClient(handler)
            {
                BaseAddress = new Uri(request.RequestUri)
            };

            if (request.Timeout > 0)
            {
                client.Timeout = TimeSpan.FromMilliseconds(request.Timeout);
            }

            foreach (var header in request.Headers)
            {
                client.DefaultRequestHeaders.Add(header.Key, header.Value);
            }

            ServicePointManager.Expect100Continue = request.Expect100Continue;
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12 | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls;

            return client;
        }

        private HttpMessageHandler CriarHandler(TransportRequest request)
        {
            if (request.UseWinHttpHandler &&
                RuntimeInformation.IsOSPlatform(OSPlatform.Windows) &&
                string.Equals(request.Method, "get", StringComparison.OrdinalIgnoreCase) &&
                request.HttpContent != null)
            {
                return CriarWinHttpHandler(request);
            }

            return CriarHttpClientHandler(request);
        }

        private HttpClientHandler CriarHttpClientHandler(TransportRequest request)
        {
            var handler = new HttpClientHandler();

            if (!request.UseCertificate)
            {
                handler.ClientCertificateOptions = request.DisableAutomaticClientCertificateSelection ?
                    ClientCertificateOption.Manual :
                    ClientCertificateOption.Automatic;
                if (request.UseDefaultCredentials)
                {
                    handler.Credentials = CredentialCache.DefaultCredentials;
                }
            }
            else
            {
                handler.ClientCertificateOptions = ClientCertificateOption.Manual;
                handler.ClientCertificates.Add(request.Certificate);
            }

            if (request.Proxy != null)
            {
                handler.Proxy = request.Proxy;
            }

            return handler;
        }

        private WinHttpHandler CriarWinHttpHandler(TransportRequest request)
        {
            var handler = new WinHttpHandler
            {
                WindowsProxyUsePolicy = request.Proxy == null ? WindowsProxyUsePolicy.UseWinInetProxy : WindowsProxyUsePolicy.UseCustomProxy,
                Proxy = request.Proxy
            };

            if (!request.UseCertificate)
            {
                // No WinHTTP, Automatic pode selecionar um certificado do repositório do
                // Windows mesmo quando a autenticação da API é feita por token/credenciais.
                // Manual sem certificados garante que o handshake não tente usar certificado.
                handler.ClientCertificateOption = ClientCertificateOption.Manual;
                if (request.UseDefaultCredentials)
                {
                    handler.ServerCredentials = CredentialCache.DefaultCredentials;
                }
            }
            else
            {
                handler.ClientCertificateOption = ClientCertificateOption.Manual;
                handler.ClientCertificates.Add(request.Certificate);
            }

            return handler;
        }
    }
}
