using System;
using System.IO;
using System.Linq;
using System.Xml;
using System.Xml.Linq;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Validator.AACBS;
using Unimake.Business.DFe.Xml.AACBS;
using Unimake.DFe.Test.AACBS.Validacao;
using Xunit;

namespace Unimake.DFe.Test.AACBS.Serializacao
{
    [Trait("DFe", "AACBS")]
    public class AACBSExemplosUsoTest
    {
        public static TheoryData<string, Type, Servico> Raizes => AACBSFixture.Raizes;

        [Theory]
        [MemberData(nameof(Raizes))]
        public void ExemplosPublicadosValidamERetornamAoMesmoXml(string raiz, Type modelo, Servico servico)
        {
            var exemplo = CarregarExemplo(raiz);
            Assert.Equal(raiz, exemplo.DocumentElement.LocalName);
            Assert.Equal(AACBSFixture.Namespace, exemplo.DocumentElement.NamespaceURI);
            var schema = AACBSFixture.ValidarSchemaDe(exemplo, raiz);
            Assert.True(schema.Success, $"{servico}: {schema.ErrorMessage}");
            Assert.True(new AACBSValidator(exemplo).Validate());

            var objeto = AACBSFixture.Desserializar(exemplo, modelo);
            CompararConteudo(exemplo, objeto.GerarXML());
#if INTEROP
            CompararConteudo(exemplo, AACBSFixture.Ler(objeto.GerarXMLString()));
#endif
        }

        [Fact]
        public void ArquivoPagamentosPodeSerLidoPorDomGravadoERelidoComoTexto()
        {
            var original = CarregarExemplo("retArquivoPagamentos");
            var pagamentos = new RetArquivoPagamentos().LerXML<RetArquivoPagamentos>(original);
            var path = Path.GetTempFileName();
            try
            {
                using (var writer = XmlWriter.Create(path, new XmlWriterSettings { NewLineHandling = NewLineHandling.Entitize }))
                {
                    pagamentos.GerarXML().WriteTo(writer);
                }

                var salvo = File.ReadAllText(path);
                CompararConteudo(original, AACBSFixture.Ler(salvo));

                var relido = XMLUtility.Deserializar<RetArquivoPagamentos>(salvo);
                Assert.Equal("2028-02-29T23:59:59.123456799-03:00", relido.GeradoEmField);
                Assert.Equal(TimeSpan.FromHours(-3), relido.GeradoEm.Offset);
                Assert.Equal(9.99m, relido.Apuracao[0].Pagamentos[0].Composicao[0].Principal);
                CompararConteudo(original, relido.GerarXML());
            }
            finally
            {
                File.Delete(path);
            }
        }

#if INTEROP
        [Fact]
        public void ColecaoComDeSolicitacoesReconstruiEnvioDarfAdquirente()
        {
            var exemplo = CarregarExemplo("gerarDarfAdquirente");
            var lido = XMLUtility.Deserializar<GerarDarfAdquirente>(exemplo);
            var envio = new GerarDarfAdquirente { Cnpj = lido.Cnpj };

            Assert.Equal(0, envio.GetSolicitacoesCount());
            Assert.Equal(3, lido.GetSolicitacoesCount());
            for (var indice = 0; indice < lido.GetSolicitacoesCount(); indice++)
            {
                envio.AddSolicitacoes(lido.GetSolicitacoes(indice));
            }

            Assert.Equal(3, envio.GetSolicitacoesCount());
            Assert.Equal("0", envio.GetSolicitacoes(0).ValorDarfParcialField);
            Assert.Null(envio.GetSolicitacoes(1).ValorDarfParcialField);
            Assert.Equal("9999999999999999.99", envio.GetSolicitacoes(2).ValorDarfParcialField);
            CompararConteudo(exemplo, AACBSFixture.Ler(envio.GerarXMLString()));
        }

        [Fact]
        public void ColecoesComPreservamHierarquiaNaGravacaoDoArquivoDePagamentos()
        {
            var exemplo = CarregarExemplo("retArquivoPagamentos");
            var arquivo = XMLUtility.Deserializar<RetArquivoPagamentos>(exemplo);
            Assert.Equal(2, arquivo.GetApuracaoCount());
            var periodo = arquivo.GetApuracao(0);
            Assert.Equal(2, periodo.GetPagamentosCount());
            var pagamento = periodo.GetPagamentos(0);
            Assert.Equal(2, pagamento.GetComposicaoCount());

            var composicao = pagamento.GetComposicao(0);
            Assert.Equal("9.99", composicao.PrincipalField);
            pagamento.AddComposicao(composicao);
            periodo.AddPagamentos(pagamento);
            arquivo.AddApuracao(periodo);
            Assert.Equal(3, pagamento.GetComposicaoCount());
            Assert.Equal(3, periodo.GetPagamentosCount());
            Assert.Equal(3, arquivo.GetApuracaoCount());

            var xml = AACBSFixture.Ler(arquivo.GerarXMLString());
            var schema = AACBSFixture.ValidarSchemaDe(xml, "retArquivoPagamentos");
            Assert.True(schema.Success, schema.ErrorMessage);
            var relido = XMLUtility.Deserializar<RetArquivoPagamentos>(xml);
            Assert.Equal(3, relido.GetApuracaoCount());
            Assert.Equal(3, relido.GetApuracao(2).GetPagamentosCount());
            Assert.Equal(3, relido.GetApuracao(2).GetPagamentos(2).GetComposicaoCount());
            Assert.Equal("9.99", relido.GetApuracao(2).GetPagamentos(2).GetComposicao(2).PrincipalField);
        }
#endif

        private static XmlDocument CarregarExemplo(string raiz)
        {
            var diretorio = Path.GetDirectoryName(typeof(AACBSExemplosUsoTest).Assembly.Location);
            var exemplo = new XmlDocument { XmlResolver = null };
            exemplo.Load(Path.Combine(diretorio, "AACBS", "Exemplos", raiz + ".xml"));
            return exemplo;
        }

        private static void CompararConteudo(XmlDocument esperado, XmlDocument atual)
        {
            var raizEsperada = LerConteudo(esperado);
            var raizAtual = LerConteudo(atual);
            Assert.True(XNode.DeepEquals(raizEsperada, raizAtual), $"Conteúdo divergente em {raizEsperada.Name}.");
        }

        private static XElement LerConteudo(XmlDocument documento)
        {
            using var reader = new XmlNodeReader(documento.DocumentElement);
            var raiz = XElement.Load(reader);
            raiz.DescendantsAndSelf().Attributes().Where(atributo => atributo.IsNamespaceDeclaration).Remove();

            // O XSD de autenticar admite somente atributos. A forma expandida vazia equivale à autocontida.
            if (raiz.Name == XName.Get("autenticar", AACBSFixture.Namespace) && !raiz.HasElements &&
                raiz.Value.All(caractere => caractere is ' ' or '\t' or '\r' or '\n'))
            {
                raiz.RemoveNodes();
            }

            return raiz;
        }
    }
}
