using System;
using System.Text;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.ConsumirServico.Compatibility
{
    internal sealed class ConfiguracaoWSSoapMapper
    {
        public WSSoap Map(Configuracao configuracoes) =>
            new WSSoap
            {
                EnderecoWeb = configuracoes.TipoAmbiente == TipoAmbiente.Producao ? configuracoes.WebEnderecoProducao : configuracoes.WebEnderecoHomologacao,
                ActionWeb = configuracoes.TipoAmbiente == TipoAmbiente.Producao ? configuracoes.WebActionProducao : configuracoes.WebActionHomologacao,
                TagRetorno = configuracoes.TipoAmbiente == TipoAmbiente.Homologacao && !string.IsNullOrEmpty(configuracoes.WebTagRetornoHomologacao) ? configuracoes.WebTagRetornoHomologacao : configuracoes.WebTagRetorno,
                EncodingRetorno = configuracoes.WebEncodingRetorno,
                GZIPCompress = configuracoes.GZIPCompress,
                VersaoSoap = configuracoes.WebSoapVersion,
                SoapString = configuracoes.WebSoapString,
                ContentType = configuracoes.WebContentType,
                TimeOutWebServiceConnect = configuracoes.TimeOutWebServiceConnect,
                PadraoNFSe = configuracoes.PadraoNFSe,
                UsaCertificadoDigital = configuracoes.UsaCertificadoDigital,
                TipoAmbiente = configuracoes.TipoAmbiente,
                ConverteSenhaBase64 = configuracoes.ConverteSenhaBase64,
                MunicipioSenha = configuracoes.ConverteSenhaBase64 ? Convert.ToBase64String(Encoding.UTF8.GetBytes(configuracoes.MunicipioSenha)) : configuracoes.MunicipioSenha,
                MunicipioUsuario = configuracoes.MunicipioUsuario,
                Token = configuracoes.MunicipioToken,
                EncriptaTagAssinatura = configuracoes.EncriptaTagAssinatura,
                Servico = configuracoes.Servico,
                TemCDATA = configuracoes.TemCDATA,
                Proxy = configuracoes.HasProxy
                    ? Proxy.DefinirServidor(configuracoes.ProxyAutoDetect, configuracoes.ProxyUser, configuracoes.ProxyPassword)
                    : null
            };
    }
}
