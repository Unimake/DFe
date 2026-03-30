using System;
using System.IO;
using System.Net;
using System.Net.Http;

namespace Unimake.Business.DFe.ConsumirServico.Contracts
{
    internal sealed class TransportResponse : IDisposable
    {
        public HttpStatusCode StatusCode { get; set; }

        public string Content { get; set; }

        public Stream ContentStream { get; set; }

        public WebException WebException { get; set; }

        public HttpResponseMessage HttpResponseMessage { get; set; }

        public void Dispose()
        {
            HttpResponseMessage?.Dispose();
            ContentStream?.Dispose();
        }
    }
}
