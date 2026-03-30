using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.ConsumirServico.Compatibility
{
    internal sealed class ConfiguracaoApiConfigMapper
    {
        public APIConfig Map(Configuracao configuracoes) =>
            new APIConfig
            {
                ContentType = configuracoes.WebContentType,
                RequestURI = configuracoes.RequestURI,
                RequestURILogin = configuracoes.TipoAmbiente == TipoAmbiente.Producao ? configuracoes.RequestURILoginProducao : configuracoes.RequestURILoginHomologacao,
                TagRetorno = configuracoes.WebTagRetorno,
                GZipCompress = configuracoes.GZIPCompress,
                WebSoapString = configuracoes.WebSoapString,
                MetodoAPI = configuracoes.MetodoAPI,
                Token = configuracoes.MunicipioToken,
                WebAction = configuracoes.WebActionProducao,
                MunicipioSenha = configuracoes.MunicipioSenha,
                MunicipioUsuario = configuracoes.MunicipioUsuario,
                PadraoNFSe = configuracoes.PadraoNFSe,
                LoginConexao = configuracoes.LoginConexao,
                ResponseMediaType = configuracoes.ResponseMediaType,
                CodigoTom = configuracoes.CodigoTom,
                Servico = configuracoes.Servico,
                UsaCertificadoDigital = configuracoes.UsaCertificadoDigital,
                Host = configuracoes.TipoAmbiente == TipoAmbiente.Producao ? configuracoes.HostProducao : configuracoes.HostHomologacao,
                ApiKey = configuracoes.ApiKey,
                HttpContent = configuracoes.HttpContent,
            };

        public APIConfig MapExplicitEnvironment(Configuracao configuracoes) =>
            new APIConfig
            {
                ContentType = configuracoes.WebContentType,
                RequestURI = configuracoes.TipoAmbiente == TipoAmbiente.Producao ? configuracoes.RequestURIProducao : configuracoes.RequestURIHomologacao,
                TagRetorno = configuracoes.WebTagRetorno,
                WebSoapString = configuracoes.WebSoapString,
                MetodoAPI = configuracoes.MetodoAPI,
                Token = configuracoes.MunicipioToken,
                WebAction = configuracoes.WebActionProducao,
                MunicipioSenha = configuracoes.MunicipioSenha,
                MunicipioUsuario = configuracoes.MunicipioUsuario,
                ResponseMediaType = configuracoes.ResponseMediaType,
                Servico = configuracoes.Servico,
                UsaCertificadoDigital = configuracoes.UsaCertificadoDigital,
                Host = configuracoes.TipoAmbiente == TipoAmbiente.Producao ? configuracoes.HostProducao : configuracoes.HostHomologacao,
                ApiKey = configuracoes.ApiKey,
                HttpContent = configuracoes.HttpContent,
            };
    }
}
