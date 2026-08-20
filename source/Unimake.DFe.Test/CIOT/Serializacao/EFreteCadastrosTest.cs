using Newtonsoft.Json.Linq;
using System;
using System.IO;
using System.Linq;
using System.Xml;
using System.Xml.Linq;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CIOT.Provedores.EFrete;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;
using Unimake.Exceptions;
using Xunit;

namespace Unimake.DFe.Test.CIOT.Serializacao
{
    public class EFreteCadastrosTest
    {
        [Theory]
        [InlineData("efrete-gravar-motorista.xml", Servico.CIOTGravarMotorista)]
        [InlineData("efrete-gravar-proprietario.xml", Servico.CIOTGravarProprietario)]
        [InlineData("efrete-gravar-veiculo.xml", Servico.CIOTGravarVeiculo)]
        [Trait("DFe", "CIOT")]
        public void EnvioFazRoundTripEValidaSchema(string arquivo, Servico servico)
        {
            var esperado = XDocument.Load(Caminho(arquivo));
            var objeto = LerEnvio(arquivo, servico);
            var gerado = XDocument.Parse(objeto.GerarXML().OuterXml);
            Assert.True(XNode.DeepEquals(Normalizar(esperado.Root), Normalizar(gerado.Root)));

            var validador = new ValidarSchema();
            validador.Validar(objeto.GerarXML(), EFreteSchemaResolver.ObterSchemaArquivo(servico), CIOTNamespace.PortalANTT);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void RetornosFazemRoundTrip()
        {
            Assert.True(CompararRetorno<RetGravarMotorista>("efrete-ret-gravar-motorista.xml").Sucesso);
            Assert.Equal(TipoPessoaCIOT.Juridica, CompararRetorno<RetGravarProprietario>("efrete-ret-gravar-proprietario.xml").Proprietario.TipoPessoa);
            Assert.Equal(TipoRodadoCIOT.Truck, CompararRetorno<RetGravarVeiculo>("efrete-ret-gravar-veiculo.xml").Veiculo.TipoRodado);
        }

        [Theory]
        [InlineData(Servico.CIOTGravarMotorista, "efrete-gravar-motorista.xml", 2, "motoristas/gravar")]
        [InlineData(Servico.CIOTGravarProprietario, "efrete-gravar-proprietario.xml", 4, "proprietarios/gravarV2")]
        [InlineData(Servico.CIOTGravarVeiculo, "efrete-gravar-veiculo.xml", 1, "veiculos/gravar")]
        [Trait("DFe", "CIOT")]
        public void MapeiaJsonEConfiguraEndpoint(Servico servico, string arquivo, int versao, string endpoint)
        {
            var xml = LerEnvio(arquivo, servico);
            var configuracao = ConfiguracaoEFrete();
            var json = JObject.Parse(EFreteMapper.CriarJson(xml, servico, configuracao));
            Assert.Equal(versao, json.Value<int>("Versao"));
            Assert.Equal("INTEGRADOR-TESTE", json.Value<string>("Integrador"));
            Assert.Equal("TOKEN-TESTE", json.Value<string>("Token"));
            Assert.Null(json["ProvedorCIOT"]);
            if (servico == Servico.CIOTGravarProprietario) Assert.Equal("012345678", json.Value<string>("RNTRC"));
            if (servico == Servico.CIOTGravarVeiculo)
            {
                Assert.Equal("012345678", json["Veiculo"].Value<string>("RNTRC"));
                Assert.Equal("Granelera", json["Veiculo"].Value<string>("TipoCarroceria"));
            }

            CriarServico(servico, File.ReadAllText(Caminho(arquivo)), configuracao);
            Assert.Contains(endpoint, configuracao.RequestURI, StringComparison.OrdinalIgnoreCase);
            Assert.Equal("post", configuracao.MetodoAPI);
        }

        [Theory]
        [InlineData("GravarMotorista")]
        [InlineData("GravarProprietario")]
        [InlineData("GravarVeiculo")]
        [Trait("DFe", "CIOT")]
        public void ANTTRecusaCadastroAntesDoEndpoint(string raiz)
        {
            var arquivo = "efrete-" + Separar(raiz.Substring(6)) + ".xml";
            var xml = File.ReadAllText(Caminho(arquivo)).Replace("<ProvedorCIOT>EFrete</ProvedorCIOT>", "<ProvedorCIOT>ANTT</ProvedorCIOT>");
            var configuracao = new Configuracao { TipoAmbiente = TipoAmbiente.Homologacao };
            var excecao = Assert.Throws<NotSupportedException>(() => CriarServico(RetornarServico(raiz), xml, configuracao));
            Assert.Contains("exclusivo da eFrete", excecao.Message);
            Assert.Null(configuracao.RequestURI);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void NormalizaRetornosDeSucessoEErro()
        {
            var sucesso = EFreteMapper.NormalizarRetorno("{\"Sucesso\":true,\"Versao\":4,\"Proprietario\":{\"CNPJ\":\"12345678000199\",\"TipoPessoa\":\"Juridica\",\"RNTRC\":\"012345678\",\"RazaoSocial\":\"TESTE\"}}", Servico.CIOTGravarProprietario);
            var retorno = new RetGravarProprietario().LerXML<RetGravarProprietario>(sucesso);
            Assert.True(retorno.Sucesso);
            Assert.Equal("012345678", retorno.Proprietario.RNTRC);

            var erro = EFreteMapper.NormalizarRetorno("{\"Sucesso\":false,\"Excecao\":{\"Codigo\":\"CAD001\",\"Mensagem\":\"Cadastro recusado\"}}", Servico.CIOTGravarVeiculo);
            var retornoErro = new RetGravarVeiculo().LerXML<RetGravarVeiculo>(erro);
            Assert.Equal("CAD001", retornoErro.Codigo);
            Assert.NotNull(retornoErro.Temp);
        }

        [Theory]
        [InlineData("1", TipoPessoaCIOT.Fisica, "Fisica")]
        [InlineData("2", TipoPessoaCIOT.Juridica, "Juridica")]
        [InlineData("\"Fisica\"", TipoPessoaCIOT.Fisica, "Fisica")]
        [InlineData("\"Juridica\"", TipoPessoaCIOT.Juridica, "Juridica")]
        [Trait("DFe", "CIOT")]
        public void NormalizaTipoPessoaNumericoOuTextual(string valorJson, TipoPessoaCIOT esperado, string valorXml)
        {
            var json = "{\"Sucesso\":true,\"Versao\":4,\"Proprietario\":{\"CNPJ\":\"12345678000199\",\"TipoPessoa\":" + valorJson + ",\"RNTRC\":\"012345678\",\"RazaoSocial\":\"TESTE\"}}";
            var xml = EFreteMapper.NormalizarRetorno(json, Servico.CIOTGravarProprietario);
            var retorno = new RetGravarProprietario().LerXML<RetGravarProprietario>(xml);

            Assert.Equal(esperado, retorno.Proprietario.TipoPessoa);
            Assert.Contains("<TipoPessoa>" + valorXml + "</TipoPessoa>", xml.OuterXml);
        }

        private static XMLBase LerEnvio(string arquivo, Servico servico)
        {
            var doc = new XmlDocument(); doc.Load(Caminho(arquivo));
            if (servico == Servico.CIOTGravarMotorista) return new GravarMotorista().LerXML<GravarMotorista>(doc);
            if (servico == Servico.CIOTGravarProprietario) return new GravarProprietario().LerXML<GravarProprietario>(doc);
            return new GravarVeiculo().LerXML<GravarVeiculo>(doc);
        }

        private static T Ler<T>(string arquivo) where T : XMLBase, new() { var doc = new XmlDocument(); doc.Load(Caminho(arquivo)); return new T().LerXML<T>(doc); }
        private static T CompararRetorno<T>(string arquivo) where T : XMLBase, new()
        {
            var esperado = XDocument.Load(Caminho(arquivo));
            var retorno = Ler<T>(arquivo);
            var gerado = XDocument.Parse(retorno.GerarXML().OuterXml);
            Assert.True(XNode.DeepEquals(Normalizar(esperado.Root), Normalizar(gerado.Root)), "O retorno não preservou integralmente o XML.\nEsperado:\n" + esperado + "\nGerado:\n" + gerado);
            return retorno;
        }
        private static Configuracao ConfiguracaoEFrete() => new Configuracao { TipoAmbiente = TipoAmbiente.Homologacao, EFreteIntegrador = "INTEGRADOR-TESTE", EFreteToken = "TOKEN-TESTE" };
        private static object CriarServico(Servico servico, string xml, Configuracao c)
        {
            if (servico == Servico.CIOTGravarMotorista) return new Unimake.Business.DFe.Servicos.CIOT.GravarMotorista(xml, c);
            if (servico == Servico.CIOTGravarProprietario) return new Unimake.Business.DFe.Servicos.CIOT.GravarProprietario(xml, c);
            return new Unimake.Business.DFe.Servicos.CIOT.GravarVeiculo(xml, c);
        }
        private static Servico RetornarServico(string raiz) => raiz == "GravarMotorista" ? Servico.CIOTGravarMotorista : raiz == "GravarProprietario" ? Servico.CIOTGravarProprietario : Servico.CIOTGravarVeiculo;
        private static string Separar(string nome) => nome == "Motorista" ? "gravar-motorista" : nome == "Proprietario" ? "gravar-proprietario" : "gravar-veiculo";
        private static XElement Normalizar(XElement element) => new XElement(element.Name, element.Attributes().OrderBy(x => x.Name.ToString()), element.Nodes().Where(x => !(x is XText text) || !string.IsNullOrWhiteSpace(text.Value)).Select(x => x is XElement child ? Normalizar(child) : x));
        private static string Caminho(string arquivo) => Path.Combine(@"..\..\..\CIOT\Resources", arquivo);
    }
}
