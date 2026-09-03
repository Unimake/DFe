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
using ServicoPdf = Unimake.Business.DFe.Servicos.CIOT.ObterOperacaoTransportePdf;

namespace Unimake.DFe.Test.CIOT.Serializacao
{
    public class EFreteObterOperacaoTransportePdfTest
    {
        [Theory]
        [InlineData("efrete-obter-operacao-transporte-pdf.xml", true)]
        [InlineData("efrete-obter-operacao-transporte-pdf-sem-viagem.xml", false)]
        [Trait("DFe", "CIOT")]
        public void EnvioFazRoundTripEValidaSchema(string arquivo, bool possuiDocumentoViagem)
        {
            var esperado = XDocument.Load(Caminho(arquivo));
            var objeto = Ler<ObterOperacaoTransportePdf>(arquivo);
            var gerado = XDocument.Parse(objeto.GerarXML().OuterXml);

            Assert.True(XNode.DeepEquals(Normalizar(esperado.Root), Normalizar(gerado.Root)));
            Assert.Equal(possuiDocumentoViagem, objeto.ShouldSerializeDocumentoViagem());
            ValidarSchema(objeto.GerarXML(), true);
        }

        [Theory]
        [InlineData("efrete-ret-obter-operacao-transporte-pdf.xml")]
        [InlineData("efrete-ret-obter-operacao-transporte-pdf-erro.xml")]
        [Trait("DFe", "CIOT")]
        public void RetornoFazRoundTripEValidaSchema(string arquivo)
        {
            var esperado = XDocument.Load(Caminho(arquivo));
            var objeto = Ler<RetObterOperacaoTransportePdf>(arquivo);
            var gerado = XDocument.Parse(objeto.GerarXML().OuterXml);

            Assert.True(XNode.DeepEquals(Normalizar(esperado.Root), Normalizar(gerado.Root)));
            ValidarSchema(objeto.GerarXML(), true);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void SchemaRecusaAusenciaOuOutroProvedorEBase64Invalido()
        {
            var semProvedor = File.ReadAllText(Caminho("efrete-obter-operacao-transporte-pdf.xml")).Replace("  <ProvedorCIOT>EFrete</ProvedorCIOT>\r\n", string.Empty).Replace("  <ProvedorCIOT>EFrete</ProvedorCIOT>\n", string.Empty);
            var antt = File.ReadAllText(Caminho("efrete-obter-operacao-transporte-pdf.xml")).Replace(">EFrete<", ">ANTT<");
            var desconhecido = File.ReadAllText(Caminho("efrete-obter-operacao-transporte-pdf.xml")).Replace(">EFrete<", ">OUTRO<");
            var codigoVazio = File.ReadAllText(Caminho("efrete-obter-operacao-transporte-pdf.xml")).Replace(">992000000126<", "><");
            var retornoInvalido = File.ReadAllText(Caminho("efrete-ret-obter-operacao-transporte-pdf.xml")).Replace("JVBERi0xLjQK", "%%%INVALIDO%%%");

            ValidarSchema(Xml(semProvedor), false);
            ValidarSchema(Xml(antt), false);
            ValidarSchema(Xml(desconhecido), false);
            ValidarSchema(Xml(codigoVazio), false);
            ValidarSchema(Xml(retornoInvalido), false);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void MapeiaJsonEndpointEExcluiMetadadoDoProvedor()
        {
            var envio = Ler<ObterOperacaoTransportePdf>("efrete-obter-operacao-transporte-pdf.xml");
            var configuracao = ConfiguracaoEFrete();
            var json = JObject.Parse(EFreteMapper.CriarJson(envio, Servico.CIOTObterOperacaoTransportePdf, configuracao));

            Assert.Equal("992000000126", json.Value<string>("CodigoIdentificacaoOperacao"));
            Assert.Equal("VIAGEM-TESTE-001", json.Value<string>("DocumentoViagem"));
            Assert.Equal(1, json.Value<int>("Versao"));
            Assert.Equal("INTEGRADOR-TESTE", json.Value<string>("Integrador"));
            Assert.Equal("TOKEN-TESTE", json.Value<string>("Token"));
            Assert.Null(json["ProvedorCIOT"]);

            new ServicoPdf(envio, configuracao);
            Assert.Equal("get", configuracao.MetodoAPI);
            Assert.EndsWith("/services/Pef/ObterOperacaoTransportePdf", configuracao.RequestURI, StringComparison.OrdinalIgnoreCase);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void NormalizaSucessoErroERecusaSucessoSemPdf()
        {
            var sucesso = EFreteMapper.NormalizarRetorno("{\"Sucesso\":true,\"Pdf\":\"JVBERi0xLjQK\",\"Versao\":1}", Servico.CIOTObterOperacaoTransportePdf);
            var resultado = new RetObterOperacaoTransportePdf().LerXML<RetObterOperacaoTransportePdf>(sucesso);
            Assert.True(resultado.Sucesso);
            Assert.Equal("JVBERi0xLjQK", resultado.Pdf);

            var erro = EFreteMapper.NormalizarRetorno("{\"Sucesso\":false,\"Versao\":1,\"Excecao\":{\"Codigo\":\"PDF001\",\"Mensagem\":\"Operação não localizada.\"}}", Servico.CIOTObterOperacaoTransportePdf);
            var resultadoErro = new RetObterOperacaoTransportePdf().LerXML<RetObterOperacaoTransportePdf>(erro);
            Assert.False(resultadoErro.Sucesso);
            Assert.Equal("PDF001", resultadoErro.Codigo);
            Assert.NotNull(resultadoErro.Temp);

            Assert.Throws<ValidarXMLException>(() => EFreteMapper.NormalizarRetorno("{\"Sucesso\":true,\"Versao\":1}", Servico.CIOTObterOperacaoTransportePdf));
            Assert.Throws<ValidarXMLException>(() => EFreteMapper.NormalizarRetorno(string.Empty, Servico.CIOTObterOperacaoTransportePdf));
            Assert.Throws<ValidarXMLException>(() => EFreteMapper.NormalizarRetorno("{}", Servico.CIOTObterOperacaoTransportePdf));
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void ANTTRecusaServicoAntesDoEndpoint()
        {
            var xml = File.ReadAllText(Caminho("efrete-obter-operacao-transporte-pdf.xml")).Replace(">EFrete<", ">ANTT<");
            var configuracao = new Configuracao { TipoAmbiente = TipoAmbiente.Homologacao };
            var excecao = Assert.Throws<NotSupportedException>(() => new ServicoPdf(xml, configuracao));
            Assert.Contains("exclusivo da eFrete", excecao.Message);
            Assert.Null(configuracao.RequestURI);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void AusenciaDoProvedorTambemRecusaServicoComoANTT()
        {
            var xml = File.ReadAllText(Caminho("efrete-obter-operacao-transporte-pdf.xml")).Replace("  <ProvedorCIOT>EFrete</ProvedorCIOT>\r\n", string.Empty).Replace("  <ProvedorCIOT>EFrete</ProvedorCIOT>\n", string.Empty);
            var configuracao = new Configuracao { TipoAmbiente = TipoAmbiente.Homologacao, ProvedorCIOT = ProvedorCIOT.EFrete };
            Assert.Throws<NotSupportedException>(() => new ServicoPdf(xml, configuracao));
            Assert.Equal(ProvedorCIOT.ANTT, configuracao.ProvedorCIOT);
            Assert.Null(configuracao.RequestURI);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void GravarPdfDecodificaExatamenteOsBytesRetornados()
        {
            var pasta = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
            Directory.CreateDirectory(pasta);
            try
            {
                var servico = new ServicoPdf();
                var propriedade = typeof(Unimake.Business.DFe.Servicos.ServicoBase).GetProperty("RetornoWSXML");
                propriedade.SetValue(servico, Xml("<RetObterOperacaoTransportePdf xmlns=\"http://www.antt.gov.br/ciot\"><Pdf>JVBERi0xLjQK</Pdf><Sucesso>true</Sucesso><Versao>1</Versao></RetObterOperacaoTransportePdf>"));
                servico.GravarPDF(pasta, "operacao.pdf");

                Assert.Equal(Convert.FromBase64String("JVBERi0xLjQK"), File.ReadAllBytes(Path.Combine(pasta, "operacao.pdf")));
            }
            finally
            {
                if (Directory.Exists(pasta)) Directory.Delete(pasta, true);
            }
        }

        private static T Ler<T>(string arquivo) where T : XMLBase, new()
        {
            var documento = new XmlDocument();
            documento.Load(Caminho(arquivo));
            return new T().LerXML<T>(documento);
        }

        private static XmlDocument Xml(string conteudo)
        {
            var documento = new XmlDocument();
            documento.LoadXml(conteudo);
            return documento;
        }

        private static void ValidarSchema(XmlDocument documento, bool esperado)
        {
            var validador = new ValidarSchema();
            validador.Validar(documento, EFreteSchemaResolver.ObterSchemaArquivo(Servico.CIOTObterOperacaoTransportePdf), CIOTNamespace.PortalANTT);
            Assert.Equal(esperado, validador.Success);
        }

        private static Configuracao ConfiguracaoEFrete() => new Configuracao
        {
            TipoAmbiente = TipoAmbiente.Homologacao,
            EFreteIntegrador = "INTEGRADOR-TESTE",
            EFreteToken = "TOKEN-TESTE"
        };

        private static XElement Normalizar(XElement element) => new XElement(element.Name, element.Attributes().OrderBy(x => x.Name.ToString()), element.Nodes().Where(x => !(x is XText text) || !string.IsNullOrWhiteSpace(text.Value)).Select(x => x is XElement child ? Normalizar(child) : x));

        private static string Caminho(string arquivo) => Path.Combine(@"..\..\..\CIOT\Resources", arquivo);
    }
}
