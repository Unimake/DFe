using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Net.Http;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.DARE;

namespace Unimake.Business.DFe.ConsumirServico.Parsers
{
    internal sealed class ApiResponseContentParser
    {
        private readonly ApiJsonXmlExtractor _jsonXmlExtractor = new ApiJsonXmlExtractor();
        private readonly ApiResponseXmlSupport _xmlSupport = new ApiResponseXmlSupport();

        public XmlDocument Parse(ref ApiResponseContext context)
        {
            var tipoRetorno = string.IsNullOrWhiteSpace(context.Config.ResponseMediaType)
                ? context.Response.Content.Headers.ContentType.MediaType
                : context.Config.ResponseMediaType;

            if (!context.ResponseContent.StartsWith("<") && context.Response.IsSuccessStatusCode && context.ResponseContent.StartsWith(" "))
            {
                context.ResponseContent = context.ResponseContent.Substring(1);
            }

            switch (tipoRetorno)
            {
                case "text/plain":
                case "application/xml":
                case "text/xml":
                    return ParseTextOrXml(context.ResponseContent);

                case "application/json":
                case "application/problem+json":
                    return ParseJson(ref context);

                case "text/html":
                    return ParseHtml(ref context);

                case "application/pdf":
                    return ParsePdf(ref context);

                default:
                    return _xmlSupport.CreatePdfXmlDocument(context.Response.Content.Headers.ToString());
            }
        }

        private XmlDocument ParseTextOrXml(string responseContent)
        {
            var resultadoRetorno = new XmlDocument();
            try
            {
                resultadoRetorno.LoadXml(responseContent);
            }
            catch
            {
                resultadoRetorno = _xmlSupport.StringToSerializedXml(responseContent);
            }

            return resultadoRetorno;
        }

        private XmlDocument ParseJson(ref ApiResponseContext context)
        {
            try
            {
                if (context.ResponseContent.TrimStart().StartsWith("<"))
                {
                    try
                    {
                        return ProcessXmlWithSafeEncoding(context.Response);
                    }
                    catch
                    {
                        var xml = new XmlDocument();
                        xml.LoadXml(context.ResponseContent);
                        return xml;
                    }
                }

                var config = context.Config;
                var resultadoRetorno = new XmlDocument();
                resultadoRetorno.LoadXml(_jsonXmlExtractor.ExtractXml(ref config, context.ResponseContent));
                context.Config = config;

                if (context.Config.Servico == Servico.DAREEnvio)
                {
                    if (context.ResponseContent.Contains("itensParaGeracao"))
                    {
                        var dareLote = JsonConvert.DeserializeObject<DARELoteRetorno>(context.ResponseContent);
                        return dareLote.GerarXML();
                    }

                    if (context.ResponseContent.Contains("documentoImpressao"))
                    {
                        var dareUnico = JsonConvert.DeserializeObject<DAREUnicoRetorno>(context.ResponseContent);
                        return CreateXmlDocumentDareRetorno(dareUnico);
                    }
                }

                return resultadoRetorno;
            }
            catch
            {
                if (context.Config.Servico == Servico.DAREReceita)
                {
                    return ParseDareReceitas(context.ResponseContent);
                }

                var xml = new XmlDocument();
                xml.LoadXml(context.ResponseContent);
                return xml;
            }
        }

        private XmlDocument ParseHtml(ref ApiResponseContext context)
        {
            try
            {
                var xml = new XmlDocument();
                var config = context.Config;
                xml.LoadXml(_jsonXmlExtractor.ExtractXml(ref config, context.ResponseContent));
                context.Config = config;
                return xml;
            }
            catch
            {
                return _xmlSupport.StringToSerializedXml(HtmlToPlainText(context.ResponseContent));
            }
        }

        private XmlDocument ParsePdf(ref ApiResponseContext context)
        {
            var responseString = context.ResponseContent.Replace("&lt;", "<").Replace("&gt;", ">").Replace("&amp;", "&");
            responseString = Convert.ToBase64String(Encoding.UTF8.GetBytes(responseString));

            if (context.Response.IsSuccessStatusCode)
            {
                using (var originalStream = context.Response.Content.ReadAsStreamAsync().Result)
                {
                    var memoryStream = new MemoryStream();
                    originalStream.CopyTo(memoryStream);
                    memoryStream.Position = 0;
                    context.Stream = memoryStream;
                }
            }
            else
            {
                context.Stream = null;
            }

            return _xmlSupport.CreatePdfXmlDocument(responseString);
        }

        private XmlDocument ParseDareReceitas(string responseContent)
        {
            List<ReceitaDARE> dare = null;
            try
            {
                dare = JsonConvert.DeserializeObject<List<ReceitaDARE>>(responseContent);
            }
            catch (Exception ex)
            {
                throw new InvalidOperationException("Não foi possível desserializar a lista de receitas.", ex);
            }

            if (dare == null)
            {
                throw new InvalidOperationException("Não foi possível desserializar a lista de receitas.");
            }

            var receitas = new Xml.DARE.Receitas
            {
                Receita = dare
            };

            return XMLUtility.Serializar<Xml.DARE.Receitas>(receitas);
        }

        private XmlDocument CreateXmlDocumentDareRetorno(DAREUnicoRetorno dareUnico)
        {
            var dareRetorno = new DARERetorno
            {
                DARE = dareUnico
            };

            return XMLUtility.Serializar<DARERetorno>(dareRetorno);
        }

        private XmlDocument ProcessXmlWithSafeEncoding(HttpResponseMessage response)
        {
            using (var stream = response.Content.ReadAsStreamAsync().Result)
            {
                var settings = new XmlReaderSettings
                {
                    DtdProcessing = DtdProcessing.Prohibit,
                    CloseInput = true
                };

                using (var reader = XmlReader.Create(stream, settings))
                {
                    var xmlDoc = new XmlDocument();
                    xmlDoc.Load(reader);
                    return xmlDoc;
                }
            }
        }

        private string HtmlToPlainText(string html)
        {
            const string tagWhiteSpace = @"(>|$)(\W|\n|\r)+<";
            const string stripFormatting = @"<[^>]*(>|$)";
            const string lineBreak = @"<(br|BR)\s{0,1}\/{0,1}>";
            var lineBreakRegex = new Regex(lineBreak, RegexOptions.Multiline);
            var stripFormattingRegex = new Regex(stripFormatting, RegexOptions.Multiline);
            var tagWhiteSpaceRegex = new Regex(tagWhiteSpace, RegexOptions.Multiline);

            var text = html;
            text = System.Net.WebUtility.HtmlDecode(text);
            text = tagWhiteSpaceRegex.Replace(text, "><");
            text = lineBreakRegex.Replace(text, Environment.NewLine);
            text = stripFormattingRegex.Replace(text, string.Empty);

            return text;
        }
    }
}
