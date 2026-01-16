using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Net.Http;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.DARE;

namespace Unimake.Business.DFe
{
    /// <summary>
    /// Classe para tratar o retornos das comunicações por API
    /// </summary>
    public static class TratarRetornoAPI
    {
        /// <summary>
        /// Classifica, faz o tratamento e retorna um XML (caso a comunicação tenha retorno)
        /// </summary>
        /// <returns></returns>
        public static XmlDocument ReceberRetorno(ref APIConfig Config, HttpResponseMessage Response, ref Stream stream)
        {
            var responseString = Response.Content.ReadAsStringAsync().Result;

            if (Response.StatusCode == System.Net.HttpStatusCode.InternalServerError)
            {
                var InternalServerError = new XmlDocument();
                InternalServerError.LoadXml(StringToXml("O servidor retornou um erro (500) || Mensagem retornada:  " + responseString));
                return InternalServerError;
            }
            else if (Response.StatusCode == System.Net.HttpStatusCode.NotFound && Config.PadraoNFSe == PadraoNFSe.NACIONAL)
            {
                var naoEncontradoErro = new XmlDocument();
                naoEncontradoErro.LoadXml(StringToXml("O servidor da Receita Federal retornou um erro (404) pois não encontrou a determinada nota no ambiente. " + responseString));
                return naoEncontradoErro;
            }
            else if (Response.StatusCode == System.Net.HttpStatusCode.Unauthorized && Config.PadraoNFSe == PadraoNFSe.GIAP)
            {
                var naoAutorizadoErro = new XmlDocument();
                naoAutorizadoErro.LoadXml(StringToXml($"Prefeitura retornou que o usuário {Config.MunicipioUsuario} não possui autorização para emissão de NFSe. Verifique as credenciais informadas: " +
                    $"usuário, senha e/ou token"));
                return naoAutorizadoErro;
            }

            var resultadoRetorno = new XmlDocument();
            var tipoRetorno = (string.IsNullOrWhiteSpace(Config.ResponseMediaType) ? Response.Content.Headers.ContentType.MediaType : Config.ResponseMediaType);

            if (!responseString.StartsWith("<") && Response.IsSuccessStatusCode)
            {
                if (responseString.StartsWith(" "))
                {
                    responseString = responseString.Substring(1);
                }
            }

            switch (tipoRetorno)
            {
                case "text/plain":
                case "application/xml":
                case "text/xml":
                    try
                    {
                        resultadoRetorno.LoadXml(responseString);
                    }
                    catch
                    {
                        resultadoRetorno.LoadXml(StringToXml(responseString));
                    }
                    break;

                case "application/json": //Retorno JSON -> Vamos ter que converter para XML
                case "application/problem+json": //DARE SP retorna isso quando o JSON de envio tem problemas nas tags
                    try
                    {
                        // Verificar se o conteúdo é realmente JSON ou se é XML com Content-Type incorreto
                        if (responseString.TrimStart().StartsWith("<"))
                        {
                            // É XML, não JSON. Tratar como XML diretamente
                            try
                            {
                                resultadoRetorno = ProcessarXmlComEncodingSeguro(Response);
                            }
                            catch
                            {
                                resultadoRetorno.LoadXml(responseString);
                            }
                        }
                        else
                        {
                            // É JSON verdadeiro, processar normalmente
                            resultadoRetorno.LoadXml(BuscarXML(ref Config, responseString));

                            if (Config.Servico == Servico.DAREEnvio)
                            {
                                if (responseString.Contains("itensParaGeracao"))
                                {
                                    DARELoteRetorno dareLote = JsonConvert.DeserializeObject<DARELoteRetorno>(responseString);
                                    resultadoRetorno = dareLote.GerarXML();
                                }
                                else if (responseString.Contains("documentoImpressao"))
                                {
                                    DAREUnicoRetorno dareUnico = JsonConvert.DeserializeObject<DAREUnicoRetorno>(responseString);
                                    resultadoRetorno = CreateXmlDocumentDARERetorno(dareUnico);
                                }
                            }
                        }
                    }
                    catch
                    {
                        if (Config.PadraoNFSe == PadraoNFSe.BAUHAUS)
                        {
                            resultadoRetorno.LoadXml(StringToXml(responseString));
                        }
                        else if (Config.Servico == Servico.DAREReceita)
                        {
                            List<ReceitaDARE> dare = null;
                            try
                            {
                                dare = JsonConvert.DeserializeObject<List<ReceitaDARE>>(responseString);
                            }
                            catch (Exception ex)
                            {
                                throw new InvalidOperationException("Não foi possível desserializar a lista de receitas.", ex);
                            }
                            if (dare == null)
                            {
                                throw new InvalidOperationException("Não foi possível desserializar a lista de receitas.");
                            }
                            resultadoRetorno = CreateXmlDocumentReceitas(dare);
                        }
                        else
                        {
                            resultadoRetorno.LoadXml(responseString);
                        }
                    }
                    break;

                case "text/html": //Retorno HTML -> Entendemos que sempre será erro
                    try
                    {
                        resultadoRetorno.LoadXml(BuscarXML(ref Config, responseString));
                    }
                    catch
                    {
                        resultadoRetorno.LoadXml(HtmlToPlainText(responseString));
                    }
                    break;

                case "application/pdf":
                    responseString = responseString.Replace("&lt;", "<").Replace("&gt;", ">").Replace("&amp;", "&");
                    responseString = Convert.ToBase64String(Encoding.UTF8.GetBytes(responseString));
                    if (Response.IsSuccessStatusCode)
                    {
                        using (var originalStream = Response.Content.ReadAsStreamAsync().Result)
                        {
                            var memoryStream = new MemoryStream();
                            originalStream.CopyTo(memoryStream);
                            memoryStream.Position = 0;
                            stream = memoryStream;
                        }
                    }
                    else
                    {
                        stream = null;
                    }
                    resultadoRetorno = CreateXmlDocument(responseString);
                    break;

                default:
                    return CreateXmlDocument(Response.Content.Headers.ToString());
            }

            if (Config.PadraoNFSe == PadraoNFSe.IPM)
            {
                var htmlNodes = resultadoRetorno.GetElementsByTagName("codigo_html");
                if (htmlNodes.Count >= 1 && resultadoRetorno.DocumentElement != null)
                {
                    resultadoRetorno.DocumentElement.RemoveChild(htmlNodes[0]);
                }
            }

            return resultadoRetorno;
        }

        #region Private Methods

        /// <summary>
        /// Método para desserializar e buscar o XML dentro de um JSON
        /// </summary>
        /// <param name="config">Objeto com as configurações para consumir a API</param>
        /// <param name="content">Conteúdo</param>
        /// <returns></returns>
        private static string BuscarXML(ref APIConfig config, string content)
        {
            string result;
            var xml = JsonConvert.DeserializeXmlNode(content, "temp");
            XmlNode node = null;

            try
            {
                var nodes = xml.GetElementsByTagName(config.TagRetorno);
                if (nodes.Count > 0)
                {
                    node = nodes[0];
                    if (node != null && config.TagRetorno != "chaveAcesso")
                    {
                        if (!string.IsNullOrWhiteSpace(node.InnerXml))
                        {
                            var temp = Compress.GZIPDecompress(node.InnerText);
                            config.TagRetorno = "prop:innertext";
                            return temp;
                        }
                    }
                }
            }
            finally
            {
                result = xml.OuterXml;
            }

            return result;
        }

        /// <summary>
        /// Método para processar XML com tratamento seguro de encoding
        /// </summary>
        /// <param name="response">Resposta HTTP da API</param>
        /// <returns>XmlDocument processado com encoding correto</returns>
        private static XmlDocument ProcessarXmlComEncodingSeguro(HttpResponseMessage response)
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

        /// <summary>
        /// Método para "limpar" o HTML, deixando apenas a string com conteúdo
        /// </summary>
        /// <param name="html"></param>
        /// <returns></returns>
        private static string HtmlToPlainText(string html)
        {
            const string tagWhiteSpace = @"(>|$)(\W|\n|\r)+<";//matches one or more (white space or line breaks) between '>' and '<'
            const string stripFormatting = @"<[^>]*(>|$)";//match any character between '<' and '>', even when end tag is missing
            const string lineBreak = @"<(br|BR)\s{0,1}\/{0,1}>";//matches: <br>,<br/>,<br />,<BR>,<BR/>,<BR />
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

        /// <summary>
        /// Método para serializar a string, para que seja realizado a leitura em XML
        /// </summary>
        /// <param name="str"></param>
        /// <returns></returns>
        private static string StringToXml(string str)
        {
            using (var retorno = new StringWriter())
            {
                var xml = new XmlSerializer(str.GetType());
                xml.Serialize(retorno, str);
                return retorno.ToString();
            }
        }

        static XmlDocument CreateXmlDocument(string text)
        {
            XmlDocument xmlDoc = new XmlDocument();
            XmlNode root = xmlDoc.CreateElement("root");
            xmlDoc.AppendChild(root);

            XmlNode textoElement = xmlDoc.CreateElement("Base64Pdf");
            textoElement.InnerText = text;
            root.AppendChild(textoElement);

            return xmlDoc;
        }

        static XmlDocument CreateXmlDocumentReceitas(List<ReceitaDARE> listaReceitas)
        {
            var receitas = new Xml.DARE.Receitas();
            receitas.Receita = listaReceitas;

            var xmlDoc = XMLUtility.Serializar<Xml.DARE.Receitas>(receitas);

            return xmlDoc;
        }

        static XmlDocument CreateXmlDocumentDARERetorno(DAREUnicoRetorno dareUnico)
        {
            var dareRetorno = new DARERetorno();
            dareRetorno.DARE = dareUnico;

            var xmlDoc = XMLUtility.Serializar<DARERetorno>(dareRetorno);

            return xmlDoc;
        }

        #endregion Private Methods
    }
}