using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe.BugFixes
{
    public class ResultErroNacionalTest
    {
        [Fact]
        [Trait("DFe", "NFSe")]
        public void ResultErroNacional_DeveNormalizarGrupoErrosPlural()
        {
            const string descricao = "Conjunto de Série, Número, Código do Município Emissor e CNPJ/CPF informado nesta DPS já existe em uma NFS-e gerada a partir de uma DPS enviada anteriormente.";
            var servico = CriarServicoComRetorno("<temp><tipoAmbiente>2</tipoAmbiente><versaoAplicativo>SefinNacional_1.6.0</versaoAplicativo><dataHoraProcessamento>2026-08-19T16:49:56.7527615-03:00</dataHoraProcessamento><idDPS>DPS352340420152243000013600017000000000000020</idDPS><erros><Codigo>E0014</Codigo><Descricao>" + descricao + "</Descricao></erros></temp>");

            var resultErro = servico.ResultErro;

            Assert.NotNull(resultErro.Erro);
            Assert.Equal("E0014", resultErro.Erro.Codigo);
            Assert.Equal(descricao, resultErro.Erro.Descricao);
            Assert.Equal("E0014", resultErro.Erros.Codigo);
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void ResultErroNacional_DeveContinuarAceitandoGrupoErroSingular()
        {
            var servico = CriarServicoComRetorno("<temp><tipoAmbiente>2</tipoAmbiente><versaoAplicativo>SefinNacional_1.6.0</versaoAplicativo><dataHoraProcessamento>2026-08-19T16:49:56.7527615-03:00</dataHoraProcessamento><erro><codigo>E0001</codigo><descricao>Erro singular.</descricao></erro></temp>");

            var resultErro = servico.ResultErro;

            Assert.NotNull(resultErro.Erro);
            Assert.Equal("E0001", resultErro.Erro.Codigo);
            Assert.Equal("Erro singular.", resultErro.Erro.Descricao);
        }

        private static GerarNfse CriarServicoComRetorno(string retorno)
        {
            var xml = new XmlDocument();
            xml.LoadXml(retorno);

            return new GerarNfse
            {
                Configuracoes = new Configuracao
                {
                    PadraoNFSe = PadraoNFSe.NACIONAL,
                    TipoAmbiente = TipoAmbiente.Homologacao
                },
                RetornoWSString = retorno,
                RetornoWSXML = xml
            };
        }
    }
}
