using System.Xml;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.ConsumirServico.Parsers
{
    internal sealed class ApiResponseErrorParser
    {
        private readonly ApiResponseXmlSupport _xmlSupport = new ApiResponseXmlSupport();

        public XmlDocument TryParse(ref ApiResponseContext context)
        {
            if (context.Response.StatusCode == System.Net.HttpStatusCode.InternalServerError && context.Config.PadraoNFSe != PadraoNFSe.NACIONAL)
            {
                return _xmlSupport.StringToSerializedXml("O servidor retornou um erro (500) || Mensagem retornada:  " + context.ResponseContent);
            }

            if (context.Response.StatusCode == System.Net.HttpStatusCode.NotFound && context.Config.PadraoNFSe == PadraoNFSe.NACIONAL)
            {
                return ParseNotFound(ref context);
            }

            if (context.Response.StatusCode == System.Net.HttpStatusCode.Unauthorized && context.Config.PadraoNFSe == PadraoNFSe.GIAP)
            {
                return _xmlSupport.StringToSerializedXml(
                    $"Prefeitura retornou que o usuário {context.Config.MunicipioUsuario} não possui autorização para emissão de NFSe. Verifique as credenciais informadas: usuário, senha e/ou token");
            }

            return null;
        }

        private XmlDocument ParseNotFound(ref ApiResponseContext context)
        {
            if (context.Config.Servico == Servico.NFSeConsultarNfsePorRps || context.Config.Servico == Servico.NFSeConsultarNfse || 
                context.Config.Servico == Servico.NFSeConsultarDistribuicaoNFSeNSU)
            {
                try
                {
                    var config = context.Config;
                    var erroXml = new ApiJsonXmlExtractor().ExtractXml(ref config, context.ResponseContent);
                    context.Config = config;
                    var document = new XmlDocument();
                    document.LoadXml(erroXml);
                    return document;
                }
                catch
                {
                }
            }

            return _xmlSupport.StringToSerializedXml("O servidor da Receita Federal retornou um erro (404) pois não encontrou a determinada nota no ambiente. " + context.ResponseContent);
        }
    }
}
