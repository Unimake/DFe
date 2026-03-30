using System.Security.Cryptography.X509Certificates;
using Unimake.Business.DFe.ConsumirServico.Contracts;

namespace Unimake.Business.DFe.ConsumirServico.Compatibility
{
    internal sealed class ApiConfigTransportRequestMapper
    {
        public TransportRequest Map(APIConfig configuracoes, X509Certificate2 certificado)
        {
            var request = new TransportRequest
            {
                Method = configuracoes.MetodoAPI,
                RequestUri = configuracoes.RequestURI,
                HttpContent = configuracoes.HttpContent,
                Certificate = certificado,
                UseCertificate = configuracoes.UsaCertificadoDigital,
                UseDefaultCredentials = !configuracoes.UsaCertificadoDigital,
                Expect100Continue = false
            };

            if (!string.IsNullOrEmpty(configuracoes.Token))
            {
                request.Headers.Add("Authorization", configuracoes.Token);
            }

            if (!string.IsNullOrEmpty(configuracoes.Host))
            {
                request.Headers.Add("Host", configuracoes.Host);
            }

            if (!string.IsNullOrEmpty(configuracoes.ApiKey))
            {
                request.Headers.Add("api-key", configuracoes.ApiKey);
            }

            if (!string.IsNullOrEmpty(configuracoes.Cookie))
            {
                request.Headers.Add("Cookie", configuracoes.Cookie);
            }

            return request;
        }
    }
}
