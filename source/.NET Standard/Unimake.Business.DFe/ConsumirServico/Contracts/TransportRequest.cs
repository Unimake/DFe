using System.Collections.Generic;
using System.Net;
using System.Net.Http;
using System.Security.Cryptography.X509Certificates;

namespace Unimake.Business.DFe.ConsumirServico.Contracts
{
    internal sealed class TransportRequest
    {
        public string Method { get; set; }

        public string RequestUri { get; set; }

        public string ContentType { get; set; }

        public string Body { get; set; }

        public string ResponseEncoding { get; set; }

        public int Timeout { get; set; }

        public bool UseCertificate { get; set; }

        public bool UseDefaultCredentials { get; set; }

        public bool Expect100Continue { get; set; }

        public X509Certificate2 Certificate { get; set; }

        public IWebProxy Proxy { get; set; }

        public CookieContainer Cookies { get; set; }

        public HttpContent HttpContent { get; set; }

        public IDictionary<string, string> Headers { get; } = new Dictionary<string, string>();
    }
}
