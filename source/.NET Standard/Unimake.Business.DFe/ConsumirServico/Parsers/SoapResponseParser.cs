using System;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.ConsumirServico.Parsers
{
    internal sealed class SoapResponseParser
    {
        public SoapResponseResult Parse(SoapResponseContext context)
        {
            var retornoXml = new XmlDocument();
            try
            {
                if (string.IsNullOrEmpty(context.ConteudoRetorno))
                {
                    throw new ValidarXMLRetornoException($"O XML retornado pelo WebService está vazio. Conteúdo XML: {context.ConteudoRetorno}");
                }

                if (!context.ConteudoRetorno.TrimStart().StartsWith("<"))
                {
                    throw new ValidarXMLRetornoException($"O conteúdo retornado pelo WebService não é um XML válido. Conteúdo XML: {context.ConteudoRetorno}");
                }

                retornoXml.LoadXml(context.ConteudoRetorno);
            }
            catch (XmlException)
            {
                if (context.TransportResponse.WebException != null)
                {
                    throw context.TransportResponse.WebException;
                }

                throw;
            }

            context.RetornoXmlBruto = retornoXml;

            var retornoServicoString = context.Soap.TagRetorno.ToLower() == "prop:innertext"
                ? ParseInnerTextResponse(retornoXml)
                : ParseTaggedResponse(context, retornoXml);

            retornoServicoString = new SoapResponseNormalizer().Normalize(context.Soap, retornoServicoString);

            var retornoServicoXml = new XmlDocument
            {
                PreserveWhitespace = false
            };
            retornoServicoXml.LoadXml(retornoServicoString);

            return new SoapResponseResult
            {
                RetornoServicoString = retornoServicoString,
                RetornoServicoXml = retornoServicoXml
            };
        }

        private string ParseTaggedResponse(SoapResponseContext context, XmlDocument retornoXml)
        {
            var tagRetorno = ResolveTagRetorno(context.Soap, retornoXml);

            if (context.TratarScapeRetorno)
            {
                if ((context.Soap.PadraoNFSe == PadraoNFSe.GIF && retornoXml.GetElementsByTagName(tagRetorno)[0].ChildNodes[0].OuterXml.Contains("SOAP-ENV:Fault")) ||
                    (context.Soap.PadraoNFSe == PadraoNFSe.DBSELLER && context.Soap.TagRetorno == "SOAP-ENV:Body") ||
                    (context.Soap.PadraoNFSe == PadraoNFSe.FINTEL && context.Soap.TagRetorno == "soap:Body"))
                {
                    return retornoXml.GetElementsByTagName(tagRetorno)[0].ChildNodes[0].OuterXml;
                }

                if (context.Soap.PadraoNFSe == PadraoNFSe.GISSONLINE && retornoXml.GetElementsByTagName(tagRetorno)[0].ChildNodes[0] == null)
                {
                    return retornoXml.ChildNodes[0].OuterXml;
                }

                if (context.Soap.PadraoNFSe == PadraoNFSe.TIPLAN && retornoXml.GetElementsByTagName(tagRetorno)[0].ChildNodes[0].OuterXml.Contains("faultcode"))
                {
                    return retornoXml.OuterXml;
                }

                if (tagRetorno == "soap:Fault" || tagRetorno.Contains("faultcode") || (context.Soap.PadraoNFSe == PadraoNFSe.ADM_SISTEMAS && retornoXml.OuterXml.Contains("s:Fault")))
                {
                    return retornoXml.OuterXml;
                }

                if (context.Soap.PadraoNFSe == PadraoNFSe.TECNOSISTEMAS)
                {
                    return retornoXml.GetElementsByTagName(tagRetorno)[0].ChildNodes[0].OuterXml.Replace("&lt;", "<").Replace("&gt;", ">");
                }

                if ((context.Soap.PadraoNFSe == PadraoNFSe.MODERNIZACAO_PUBLICA || context.Soap.PadraoNFSe == PadraoNFSe.METROPOLIS) &&
                    context.Soap.Servico != Servico.NFSeConsultarNfseFaixa &&
                    context.Soap.Servico != Servico.NFSeConsultarNfsePorRps &&
                    context.Soap.Servico != Servico.NFSeConsultarNfseServicoPrestado &&
                    context.Soap.Servico != Servico.NFSeConsultarNfseServicoTomado &&
                    !retornoXml.GetElementsByTagName(tagRetorno)[0].OuterXml.Contains("Resposta"))
                {
                    return retornoXml.OuterXml;
                }

                return retornoXml.GetElementsByTagName(tagRetorno)[0].ChildNodes[0].InnerText;
            }

            var retornoServicoString = retornoXml.GetElementsByTagName(tagRetorno)[0].ChildNodes[0].OuterXml;
            if (context.Soap.PadraoNFSe == PadraoNFSe.DSF && tagRetorno == "soap:Fault")
            {
                retornoServicoString = retornoXml.OuterXml;
            }

            return retornoServicoString;
        }

        private string ParseInnerTextResponse(XmlDocument retornoXml)
        {
            if (string.IsNullOrWhiteSpace(retornoXml.InnerText))
            {
                throw new Exception("A propriedade InnerText do XML retornado pelo web-service está vazia.");
            }

            var retornoServicoString = retornoXml.InnerText;

            if (retornoServicoString.ToLower().IndexOf("<?xml") <= 1 && retornoServicoString.IndexOf("?>") >= 0)
            {
                retornoServicoString = retornoServicoString.Substring(retornoServicoString.IndexOf("?>") + 2);
            }

            return retornoServicoString.Replace("\r\n", "");
        }

        private string ResolveTagRetorno(WSSoap soap, XmlDocument retornoXml)
        {
            var tagRetorno = soap.TagRetorno;

            if (tagRetorno.Split('|').Length > 1)
            {
                for (int i = 0; i < tagRetorno.Split('|').Length; i++)
                {
                    var nomeTag = tagRetorno.Split('|')[i];
                    if (retornoXml.GetElementsByTagName(nomeTag)[0] != null)
                    {
                        tagRetorno = nomeTag;
                        break;
                    }
                }
            }

            if (retornoXml.GetElementsByTagName(tagRetorno)[0] != null)
            {
                return tagRetorno;
            }

            if (retornoXml.GetElementsByTagName("soap:Body").Count >= 1 && retornoXml.GetElementsByTagName("soap:Body")[0].ChildNodes.Count >= 1)
            {
                tagRetorno = retornoXml.GetElementsByTagName("soap:Body")[0].ChildNodes[0].Name;
            }

            if (retornoXml.GetElementsByTagName("env:Body").Count >= 1 && retornoXml.GetElementsByTagName("env:Body")[0].ChildNodes.Count >= 1)
            {
                tagRetorno = retornoXml.GetElementsByTagName("env:Body")[0].ChildNodes[0].Name;
            }

            if (retornoXml.GetElementsByTagName(tagRetorno)[0] == null)
            {
                throw new Exception("Não foi possível localizar a tag <" + tagRetorno + "> no XML retornado pelo web-service.\r\n\r\n" +
                    "Conteúdo retornado pelo servidor:\r\n\r\n" + retornoXml.InnerXml);
            }

            return tagRetorno;
        }
    }
}
