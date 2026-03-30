using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Xml;
using System.Xml.Linq;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.ConsumirServico.Builders
{
    internal sealed class NfsePayloadBuilder
    {
        public HttpContent BuildHm2SolucoesContent(XmlDocument conteudoXml, XmlDocument conteudoXmlAssinado)
        {
            if (!conteudoXml.GetElementsByTagName("EnviarLoteRpsEnvio").IsNullOrEmpty())
            {
                return new FormUrlEncodedContent(new Dictionary<string, string>
                {
                    { "xml", conteudoXmlAssinado.OuterXml }
                });
            }

            var document = XDocument.Parse(conteudoXml.OuterXml);
            var dictionary = new Dictionary<string, string>();

            foreach (var parameters in document.Descendants())
            {
                dictionary.Add(parameters.Name.ToString(), parameters.Value);
            }

            return new FormUrlEncodedContent(dictionary);
        }

        public HttpContent BuildIpm280Content(Configuracao configuracoes, XmlDocument conteudoXml)
        {
            var path = string.IsNullOrWhiteSpace(conteudoXml.BaseURI)
                ? "arquivo.xml"
                : conteudoXml.BaseURI.Substring(8, conteudoXml.BaseURI.Length - 8);

            var xml = conteudoXml.OuterXml.Replace("<", "&lt;")
                                          .Replace(">", "&gt;")
                                          .Replace("/", "")
                                          .Replace("&", "&amp;")
                                          .Replace("'", "&apos;")
                                          .Replace("\"", "&quot;");

            var boundary = "----------------------------" + DateTime.Now.Ticks.ToString("x");
            var xmlBytes = Encoding.UTF8.GetBytes(xml);
            var xmlContent = new ByteArrayContent(xmlBytes);

            xmlContent.Headers.ContentType = MediaTypeHeaderValue.Parse("text/xml");
            xmlContent.Headers.ContentEncoding.Add("ISO-8859-1");
            xmlContent.Headers.ContentDisposition = new ContentDispositionHeaderValue("form-data")
            {
                Name = "f1",
                FileName = path,
            };

            if (!string.IsNullOrWhiteSpace(configuracoes.CodigoTom))
            {
                return new MultipartContent("form-data", boundary)
                {
                    CreateMultipartStringContent(configuracoes.MunicipioUsuario, "login", "UTF-8"),
                    CreateMultipartStringContent(configuracoes.MunicipioSenha, "senha", "UTF-8"),
                    CreateMultipartStringContent(configuracoes.CodigoTom, "cidade", "UTF-8"),
                    CreateMultipartStringContent(path, "f1", "ISO-8859-1"),
                    xmlContent
                };
            }

            return new MultipartContent("form-data", boundary)
            {
                xmlContent,
            };
        }

        private StringContent CreateMultipartStringContent(string value, string name, string encoding)
        {
            var content = new StringContent(value ?? string.Empty);
            content.Headers.ContentType = MediaTypeHeaderValue.Parse("text/xml");
            content.Headers.ContentEncoding.Add(encoding);
            content.Headers.ContentDisposition = new ContentDispositionHeaderValue("form-data")
            {
                Name = name,
            };

            return content;
        }
    }
}
