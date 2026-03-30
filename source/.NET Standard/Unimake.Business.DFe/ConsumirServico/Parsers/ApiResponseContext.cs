using System.IO;
using System.Net.Http;

namespace Unimake.Business.DFe.ConsumirServico.Parsers
{
    internal sealed class ApiResponseContext
    {
        public APIConfig Config { get; set; }

        public HttpResponseMessage Response { get; set; }

        public string ResponseContent { get; set; }

        public Stream Stream { get; set; }
    }
}
