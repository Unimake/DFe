using Newtonsoft.Json;
using System;
using System.IO;
using System.Net.Http;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

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
        public static XmlDocument ReceberRetorno(ref APIConfig Config, HttpResponseMessage Response)
        {
            var ResponseString = Response.Content.ReadAsStringAsync().Result;
            var resultadoRetorno = new XmlDocument();
            Response.Content.Headers.ContentType.MediaType = (string.IsNullOrWhiteSpace(Config.ResponseMediaType) ? Response.Content.Headers.ContentType.MediaType : Config.ResponseMediaType);

            //Response.Content.Headers.ContentType.MediaType -> ContentType retornado na comunicação || (Config.ContentType)
            switch (Response.Content.Headers.ContentType.MediaType)             //(Config.ContentType)
            {
                case "text/plain": //Retorno XML -> Não temos que fazer nada, já retornou no formato mais comum
                case "application/xml": //Retorno XML -> Não temos que fazer nada, já retornou no formato mais comum
                case "text/xml": //Retorno XML -> Não temos que fazer nada, já retornou no formato mais comum
                    try
                    {
                        resultadoRetorno.LoadXml(ResponseString);
                    }
                    catch
                    {
                        resultadoRetorno.LoadXml(StringToXml(ResponseString));
                    }
                    break;

                case "application/json": //Retorno JSON -> Vamos ter que converter para XML
                    try
                    {
                        resultadoRetorno.LoadXml(BuscarXML(ref Config, ResponseString));
                    }
                    catch
                    {
                        resultadoRetorno.LoadXml(ResponseString);
                    }

                    break;

                case "text/html": //Retorno HTML -> Entendemos que sempre será erro
                    try
                    {
                        resultadoRetorno.LoadXml(BuscarXML(ref Config, ResponseString));
                    }
                    catch
                    {
                        resultadoRetorno.LoadXml(HtmlToPlainText(ResponseString));
                    }
                    break;
            }
            
            if (Config.PadraoNFSe == PadraoNFSe.IPM)
            {
                if (resultadoRetorno.GetElementsByTagName("codigo_html").Count >= 1)
                {
                    resultadoRetorno.DocumentElement.RemoveChild(resultadoRetorno.GetElementsByTagName("codigo_html")[0]);
                }
            }
           
            return resultadoRetorno;
        }


        #region Private Methods

        /// <summary>
        /// Método para desserealizar e buscar o XML dentro de um JSON
        /// </summary>
        /// <param name="content"></param>
        /// <returns></returns>
        private static string BuscarXML(ref APIConfig Config, string content)
        {
            string result = "";
            string temp = "";
            XmlDocument xml = JsonConvert.DeserializeXmlNode(content, "temp");
            XmlNode node;


            try
            {
                node = xml.GetElementsByTagName(Config.TagRetorno)[0];         //tag retorno
                if (node != null)
                {
                    temp = Compress.GZIPDecompress(node.InnerText);
                    Config.TagRetorno = "prop:innertext";
                    return temp;
                }
            }
            finally
            {
                result = xml.OuterXml;
                //throw new Exception("Problema ao buscar o XML dentro do JSON :" + ex.Message);
            }

            return result;
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
            //Decode html specific characters
            text = System.Net.WebUtility.HtmlDecode(text);
            //Remove tag whitespace/line breaks
            text = tagWhiteSpaceRegex.Replace(text, "><");
            //Replace <br /> with line breaks
            text = lineBreakRegex.Replace(text, Environment.NewLine);
            //Strip formatting
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
            XmlSerializer xml = new XmlSerializer(str.GetType());
            StringWriter retorno = new StringWriter();
            xml.Serialize(retorno, str);

            return retorno.ToString();
        }

        #endregion Private Methods
    }
}
