using System.Net;
using System.Security.Cryptography.X509Certificates;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.ConsumirServico.Compatibility
{
    internal sealed class WSSoapTransportRequestMapper
    {
        public TransportRequest Map(WSSoap soap, X509Certificate2 certificado, string soapXml, CookieContainer cookies)
        {
            var request = new TransportRequest
            {
                Method = "POST",
                RequestUri = soap.EnderecoWeb,
                Body = soapXml,
                Cookies = cookies,
                Timeout = soap.TimeOutWebServiceConnect,
                ContentType = string.IsNullOrEmpty(soap.ContentType) ? "application/soap+xml; charset=utf-8;" : soap.ContentType,
                Proxy = soap.Proxy,
                Certificate = certificado,
                UseCertificate = soap.UsaCertificadoDigital,
                ResponseEncoding = soap.EncodingRetorno,
                Expect100Continue = false
            };

            request.Headers.Add("SOAPAction", soap.ActionWeb);

            if (soap.PadraoNFSe == PadraoNFSe.FUTURIZE)
            {
                request.Headers.Add("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36");
            }

            return request;
        }
    }
}
