using System.Xml;

namespace Unimake.Business.DFe.ConsumirServico.Parsers
{
    internal sealed class SoapResponseResult
    {
        public string RetornoServicoString { get; set; }

        public XmlDocument RetornoServicoXml { get; set; }
    }
}
