using System.IO;
using System.Xml;
using Unimake.Business.DFe.ConsumirServico.Parsers;

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
        public static XmlDocument ReceberRetorno(ref APIConfig Config, System.Net.Http.HttpResponseMessage Response, ref Stream stream)
        {
            var context = new ApiResponseContext
            {
                Config = Config,
                Response = Response,
                ResponseContent = Response.Content.ReadAsStringAsync().Result,
                Stream = stream
            };

            var resultadoRetorno = new ApiResponseErrorParser().TryParse(ref context)
                                  ?? new ApiResponseContentParser().Parse(ref context);

            if (Config.PadraoNFSe == Servicos.PadraoNFSe.IPM)
            {
                var htmlNodes = resultadoRetorno.GetElementsByTagName("codigo_html");
                if (htmlNodes.Count >= 1 && resultadoRetorno.DocumentElement != null)
                {
                    resultadoRetorno.DocumentElement.RemoveChild(htmlNodes[0]);
                }
            }

            Config = context.Config;
            stream = context.Stream;

            return resultadoRetorno;
        }
    }
}
