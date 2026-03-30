using System.Xml;
using Unimake.Business.DFe.ConsumirServico.Contracts;

namespace Unimake.Business.DFe.ConsumirServico.Parsers
{
    internal sealed class SoapResponseContext
    {
        public WSSoap Soap { get; set; }

        public TransportResponse TransportResponse { get; set; }

        public bool TratarScapeRetorno { get; set; }

        public string ConteudoRetorno { get; set; }

        public XmlDocument RetornoXmlBruto { get; set; }
    }
}
