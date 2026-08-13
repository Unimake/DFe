using System;
using System.Net.Http;

namespace Unimake.Business.DFe.Servicos.CIOT.Provedores.EFrete
{
    internal static class EFreteAuthenticator
    {
        internal static void AjustarConfiguracao(Configuracao configuracao)
        {
            configuracao.MunicipioToken = null;
            configuracao.UsaCertificadoDigital = string.IsNullOrWhiteSpace(configuracao.EFreteToken) &&
                string.IsNullOrWhiteSpace(configuracao.EFreteUsuario) && string.IsNullOrWhiteSpace(configuracao.EFreteSenha);
        }

        internal static void Preparar(Configuracao configuracao)
        {
            if (!string.IsNullOrWhiteSpace(configuracao.EFreteToken))
            {
                configuracao.UsaCertificadoDigital = false;
                return;
            }

            var informouUsuario = !string.IsNullOrWhiteSpace(configuracao.EFreteUsuario);
            var informouSenha = !string.IsNullOrWhiteSpace(configuracao.EFreteSenha);
            if (!informouUsuario && !informouSenha)
            {
                configuracao.UsaCertificadoDigital = true;
                return;
            }
            if (!informouUsuario || !informouSenha || string.IsNullOrWhiteSpace(configuracao.EFreteIntegrador))
            {
                throw new InvalidOperationException("Para autenticação eFrete por credenciais, informe EFreteUsuario, EFreteSenha e EFreteIntegrador.");
            }

            var apiConfig = new APIConfig
            {
                RequestURI = configuracao.TipoAmbiente == TipoAmbiente.Producao ? configuracao.RequestURILoginProducao : configuracao.RequestURILoginHomologacao,
                MetodoAPI = "get",
                ContentType = "application/json",
                TagRetorno = "prop:innertext",
                UsaCertificadoDigital = false,
                UsaWinHttpHandler = true,
                HttpContent = new StringContent(EFreteMapper.CriarJsonLogin(configuracao), System.Text.Encoding.UTF8, "application/json")
            };
            using (var consumidor = new ConsumirAPI())
            {
                consumidor.ExecutarServico(apiConfig, null);
                var token = EFreteMapper.ObterTokenLogin(consumidor.RetornoServicoRawString);
                if (string.IsNullOrWhiteSpace(token)) throw new InvalidOperationException("A eFrete não retornou token para as credenciais informadas.");
                configuracao.EFreteToken = token;
                configuracao.UsaCertificadoDigital = false;
            }
        }
    }
}
