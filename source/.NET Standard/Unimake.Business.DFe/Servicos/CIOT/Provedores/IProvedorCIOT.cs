using System.Net.Http;
using System.Xml;
using Unimake.Business.DFe.Xml;

namespace Unimake.Business.DFe.Servicos.CIOT.Provedores
{
    internal interface IProvedorCIOT
    {
        bool UsaValidacaoSchema { get; }

        bool EnviaConteudoEmRequisicaoGet { get; }

        void Configurar(Configuracao configuracao, string nomeServico, Servico servico);

        HttpContent CriarHttpContent(XMLBase xml, Servico servico, Configuracao configuracao);

        void Validar(XMLBase xml, Servico servico, Configuracao configuracao);

        void PrepararExecucao(Configuracao configuracao);

        XmlDocument NormalizarRetorno(string retorno, Servico servico);
    }
}
