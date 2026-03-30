using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.ConsumirServico.Parsers
{
    internal sealed class SoapResponseNormalizer
    {
        public string Normalize(WSSoap soap, string retornoServicoString)
        {
            if (soap.PadraoNFSe == PadraoNFSe.FIORILLI || soap.PadraoNFSe == PadraoNFSe.SONNER || soap.PadraoNFSe == PadraoNFSe.SMARAPD || soap.PadraoNFSe == PadraoNFSe.DSF)
            {
                retornoServicoString = retornoServicoString.Replace("ns1:", string.Empty)
                                                           .Replace("ns2:", string.Empty)
                                                           .Replace("ns3:", string.Empty)
                                                           .Replace("ns4:", string.Empty)
                                                           .Replace("ns0:", string.Empty);
            }
            else if (soap.PadraoNFSe == PadraoNFSe.COPLAN)
            {
                retornoServicoString = retornoServicoString.Replace("<CompNfse  ><?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<CompNfse>");
                retornoServicoString = retornoServicoString.Replace("<CompNfse><?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<CompNfse>");
            }
            else if (soap.PadraoNFSe == PadraoNFSe.GINFES && retornoServicoString.IndexOf("<p1:MensagemRetorno>") > 1)
            {
                retornoServicoString = retornoServicoString.Replace("</MensagemRetorno>", "</p1:MensagemRetorno>");
            }
            else if (soap.PadraoNFSe == PadraoNFSe.QUASAR && !retornoServicoString.TrimStart().StartsWith("<"))
            {
                retornoServicoString = $"<?xml version=\"1.0\" encoding=\"utf-8\"?><Retorno><Mensagem>{retornoServicoString}</Mensagem></Retorno>";
            }

            return retornoServicoString;
        }
    }
}
