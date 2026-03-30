using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.ConsumirServico.Builders
{
    internal sealed class CommonApiPayloadBuilder
    {
        public HttpContent Build(Configuracao configuracoes, XmlDocument conteudoXmlAssinado, XmlDocument conteudoXmlOriginal)
        {
            var xmlBody = conteudoXmlAssinado.OuterXml;

            if (configuracoes.GZIPCompress)
            {
                xmlBody = Convert.ToBase64String(Encoding.UTF8.GetBytes(xmlBody));
                xmlBody = Compress.GZIPCompress(conteudoXmlAssinado);
            }

            if (!string.IsNullOrWhiteSpace(configuracoes.WebSoapString))
            {
                configuracoes.WebSoapString = configuracoes.WebSoapString.Replace("{xml}", xmlBody);
                return new StringContent(configuracoes.WebSoapString, Encoding.UTF8, configuracoes.WebContentType);
            }

            if (configuracoes.WebContentType == "application/json")
            {
                return BuildJsonContent(configuracoes, xmlBody);
            }

            if (configuracoes.WebContentType == "multipart/form-data")
            {
                return BuildMultipartContent(configuracoes, conteudoXmlOriginal, xmlBody);
            }

            return new StringContent(xmlBody, Encoding.UTF8, configuracoes.WebContentType);
        }

        private HttpContent BuildJsonContent(Configuracao configuracoes, string xmlBody)
        {
            var dicionario = new Dictionary<string, object>();

            if (configuracoes.LoginConexao)
            {
                dicionario.Add("usuario", configuracoes.MunicipioUsuario);
                dicionario.Add("senha", configuracoes.MunicipioSenha);
            }

            var action = "xml";
            if (!string.IsNullOrWhiteSpace(configuracoes.WebActionProducao))
            {
                if (configuracoes.WebActionProducao.IndexOf(":[]") > 0)
                {
                    action = configuracoes.WebActionProducao.Replace(":[]", "");
                    dicionario.Add(action, new List<string> { xmlBody });
                }
                else
                {
                    action = configuracoes.WebActionProducao;
                    dicionario.Add(action, xmlBody);
                }
            }
            else
            {
                dicionario.Add(action, xmlBody);
            }

            var json = JsonConvert.SerializeObject(dicionario);
            return new StringContent(json, Encoding.UTF8, configuracoes.WebContentType);
        }

        private HttpContent BuildMultipartContent(Configuracao configuracoes, XmlDocument conteudoXmlOriginal, string xmlBody)
        {
            var path = string.IsNullOrWhiteSpace(conteudoXmlOriginal.BaseURI)
                ? "arquivo.xml"
                : conteudoXmlOriginal.BaseURI.Substring(8, conteudoXmlOriginal.BaseURI.Length - 8);

            var boundary = "----------------------------" + DateTime.Now.Ticks.ToString("x");
            var xmlBytes = Encoding.UTF8.GetBytes(xmlBody);
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
                return BuildMultipartContentWithCodigoTom(configuracoes, boundary, path, xmlContent);
            }

            return new MultipartContent("form-data", boundary)
            {
                xmlContent,
            };
        }

        private HttpContent BuildMultipartContentWithCodigoTom(Configuracao configuracoes, string boundary, string path, ByteArrayContent xmlContent)
        {
            var usuario = CreateMultipartStringContent(configuracoes.MunicipioUsuario, "login", "UTF-8");
            var senha = CreateMultipartStringContent(configuracoes.MunicipioSenha, "senha", "UTF-8");
            var codigoTom = CreateMultipartStringContent(configuracoes.CodigoTom, "cidade", "UTF-8");
            var f1 = CreateMultipartStringContent(path, "f1", "ISO-8859-1");

            return new MultipartContent("form-data", boundary)
            {
                usuario,
                senha,
                codigoTom,
                f1,
                xmlContent
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
