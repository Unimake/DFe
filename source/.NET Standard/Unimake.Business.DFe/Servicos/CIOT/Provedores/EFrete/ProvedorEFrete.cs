using System.Net.Http;
using System.Xml;
using Unimake.Business.DFe.Xml;

namespace Unimake.Business.DFe.Servicos.CIOT.Provedores.EFrete
{
    internal sealed class ProvedorEFrete : IProvedorCIOT
    {
        public bool UsaValidacaoSchema => false;

        public bool EnviaConteudoEmRequisicaoGet => true;

        public bool RecriaConteudoAposPrepararExecucao => true;

        public void Configurar(Configuracao configuracao, string nomeServico, Servico servico)
        {
            EFreteValidator.ValidarServicoSuportado(servico);
            if (!configuracao.Definida)
            {
                configuracao.Load(nomeServico, "EF.xml");
                configuracao.Definida = true;
            }
            EFreteAuthenticator.AjustarConfiguracao(configuracao);
        }

        public HttpContent CriarHttpContent(XMLBase xml, Servico servico, Configuracao configuracao) =>
            new StringContent(EFreteMapper.CriarJson(xml, servico, configuracao), System.Text.Encoding.UTF8, "application/json");

        public void Validar(XMLBase xml, Servico servico, Configuracao configuracao) => EFreteValidator.Validar(xml, servico, configuracao);

        public void PrepararExecucao(Configuracao configuracao) => EFreteAuthenticator.Preparar(configuracao);

        public XmlDocument NormalizarRetorno(string retorno, Servico servico) => EFreteMapper.NormalizarRetorno(retorno, servico);
    }
}
