using System;
using System.IO;
using System.Linq;
using System.Xml;
using System.Xml.Linq;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Validator.AACBS;
using Unimake.Business.DFe.Validator.Abstractions;
using Unimake.Business.DFe.Xml.AACBS;
using Xunit;

namespace Unimake.DFe.Test.AACBS.Validacao
{
    [Trait("DFe", "AACBS")]
    public class AACBSLimitesContratoTest
    {
        [Theory]
        [InlineData("retArquivoDebitos", true)]
        [InlineData("retArquivoCreditos", true)]
        [InlineData("retArquivoPagamentos", false)]
        [InlineData("retArquivoRecolhimentos", false)]
        public void SchemasAceitamNiDeQuatorzeDigitosSomenteNosDebitosECreditos(string raiz, bool aceitaQuatorze)
        {
            var arquivo = AACBSFixture.Carregar(raiz);
            var ni = AACBSFixture.Procurar(arquivo, "ni");
            ni.InnerText = "00000001";
            ConferirSchemasDoArquivoEEnvelope(arquivo, raiz, true);

            ni.InnerText = "00000001000001";
            ConferirSchemasDoArquivoEEnvelope(arquivo, raiz, aceitaQuatorze);
        }

        [Theory]
        [InlineData("ni", "000000010")]
        [InlineData("ni", "00000001000")]
        [InlineData("ni", "0000000100000")]
        [InlineData("ni", "000000010000001")]
        [InlineData("ni", "0000000100000\u0661")]
        [InlineData("niConsumidor", "00000001")]
        public void SchemasDeDebitosRejeitamIdentificadorForaDoContrato(string campo, string texto)
        {
            const string raiz = "retArquivoDebitos";
            var arquivo = AACBSFixture.Carregar(raiz);
            AACBSFixture.Procurar(arquivo, campo).InnerText = texto;

            ConferirSchemasDoArquivoEEnvelope(arquivo, raiz, false);
        }

        [Theory]
        [InlineData("ni", "000000010")]
        [InlineData("ni", "0000000100")]
        [InlineData("ni", "00000001000")]
        [InlineData("ni", "000000010000")]
        [InlineData("ni", "0000000100000")]
        [InlineData("ni", "000000010000001")]
        [InlineData("ni", "0000000100000\u0661")]
        [InlineData("ni", "00000001+00001")]
        [InlineData("ni", "00000001-00001")]
        [InlineData("ni", "0000000100000A")]
        [InlineData("ni", " 00000001 ")]
        [InlineData("ni", "00000001000001 ")]
        [InlineData("ni", "\t00000001000001")]
        [InlineData("niConsumidor", "00000001")]
        public void SchemasDeCreditosRejeitamIdentificadorForaDoContrato(string campo, string texto)
        {
            const string raiz = "retArquivoCreditos";
            var arquivo = AACBSFixture.Carregar(raiz);
            AACBSFixture.Procurar(arquivo, campo).InnerText = texto;

            ConferirSchemasDoArquivoEEnvelope(arquivo, raiz, false);
        }

        [Theory]
        [InlineData("consultarDebitos")]
        [InlineData("consultarCreditos")]
        [InlineData("consultarPagamentos")]
        [InlineData("consultarRecolhimentos")]
        [InlineData("baixarArquivoRetorno")]
        public void EnviosContinuamExigindoCnpjDeOitoDigitos(string raiz)
        {
            var envio = AACBSFixture.Carregar(raiz);
            var cnpj = AACBSFixture.Procurar(envio, "cnpj");
            Assert.Equal("00000001", cnpj.InnerText);
            Assert.True(Validar(envio).Validado);

            cnpj.InnerText = "00000001000001";
            Assert.False(Validar(envio).Validado);
        }

        private static void ConferirSchemasDoArquivoEEnvelope(XmlDocument arquivo, string raiz, bool sucessoEsperado)
        {
            const string raizEnvelope = "retBaixarArquivoRetorno";
            var envelope = AACBSFixture.Carregar(raizEnvelope);
            envelope.DocumentElement.ReplaceChild(envelope.ImportNode(arquivo.DocumentElement, true),
                AACBSFixture.Procurar(envelope, "retArquivoDebitos"));

            Assert.Equal(sucessoEsperado, AACBSFixture.ValidarSchemaDe(arquivo, raiz).Success);
            Assert.Equal(sucessoEsperado, AACBSFixture.ValidarSchemaDe(envelope, raizEnvelope).Success);
            Assert.Equal(sucessoEsperado, Validar(arquivo).Validado);
            Assert.Equal(sucessoEsperado, Validar(envelope).Validado);
        }

        [Theory]
        [InlineData("9999-12-31T23:59:59.99999994Z")]
        [InlineData("9999-12-31T23:59:59.99999995Z")]
        public void PreservaInstanteNoLimiteSemArredondarAntesDaProjecao(string instante)
        {
            var xml = AACBSFixture.Carregar("retArquivoDebitos");
            AACBSFixture.Procurar(xml, "geradoEm").InnerText = instante;
            Assert.True(Validar(xml).Validado);
            var modelo = XMLUtility.Deserializar<RetArquivoDebitos>(xml);
            Assert.Equal(instante, modelo.GeradoEmField);
            Assert.Equal(DateTimeOffset.MaxValue, modelo.GeradoEm);
            Assert.Equal(instante, XMLUtility.Deserializar<RetArquivoDebitos>(modelo.GerarXML()).GeradoEmField);
        }

        [Theory]
        [InlineData("2027-02-29T12:00:00Z")]
        [InlineData("2028-02-29T25:00:00Z")]
        [InlineData("0000-01-01T00:00:00Z")]
        public void FormatoLexicoNaoSubstituiValidacaoDeCalendarioEHora(string instante)
        {
            var xml = AACBSFixture.Carregar("retArquivoDebitos");
            AACBSFixture.Procurar(xml, "geradoEm").InnerText = instante;
            Assert.False(Validar(xml).Validado);
            Assert.Throws<XmlException>(() => XMLUtility.Deserializar<RetArquivoDebitos>(xml));
        }

        [Theory]
        [InlineData("gerarDarfAdquirente", "solicitacoes/item[1]/chaveDFE")]
        [InlineData("retArquivoDebitos", "apuracao/item[1]/debitos/item[1]/chave")]
        [InlineData("retArquivoCreditos", "apuracao/item[1]/creditos/item[1]/chave")]
        [InlineData("retArquivoPagamentos", "apuracao/item[1]/pagamentos/item[1]/composicao/item[1]/chaveDFE")]
        [InlineData("retArquivoRecolhimentos", "apuracao/item[1]/pagamentos/item[1]/composicao/item[1]/chaveDFE")]
        public void LimiteDaChaveContaPontosDeCodigoEmTodasAsFamilias(string raiz, string caminho)
        {
            var suplementar = char.ConvertFromUtf32(0x1F600);
            var xml = AACBSFixture.Carregar(raiz);
            var campo = AACBSFixture.Procurar(xml, caminho);
            foreach (var valor in new[] { new string('A', 99) + suplementar, string.Concat(Enumerable.Repeat(suplementar, 100)) })
            {
                campo.InnerText = valor;
                var validacao = Validar(xml);
                Assert.True(validacao.Validado, validacao.MensagemRetorno);
            }
            foreach (var valor in new[] { new string('A', 100) + suplementar, string.Concat(Enumerable.Repeat(suplementar, 101)), new string('A', 101) })
            {
                campo.InnerText = valor;
                Assert.False(Validar(xml).Validado);
            }

            campo.InnerText = new string(' ', 100);
            Assert.Equal(raiz != "gerarDarfAdquirente", Validar(xml).Validado);

            campo.InnerText = new string(' ', 101);
            var excesso = Validar(xml);
            Assert.False(excesso.Validado);
            if (raiz != "gerarDarfAdquirente")
            {
                Assert.Contains("100 caracteres Unicode", excesso.MensagemRetorno);
            }

            foreach (var quantidade in new[] { 99, 100 })
            {
                campo.InnerText = new string('A', quantidade);
                campo.AppendChild(xml.CreateComment("comentário entre trechos de texto"));
                campo.AppendChild(xml.CreateWhitespace(" "));

                var validacao = Validar(xml);
                Assert.Equal(quantidade == 99, validacao.Validado);
                if (quantidade == 100)
                {
                    Assert.Contains("100 caracteres Unicode", validacao.MensagemRetorno);
                }
            }
        }

        [Fact]
        public void ValidadorAACBSPreservaEspacosParaRegrasAdicionais()
        {
            var xml = AACBSFixture.Carregar("consultarSituacaoSolicitacao");
            AACBSFixture.Procurar(xml, "tiqueteSolicitacao").InnerText = " ";
            var validador = new AACBSValidator(xml);
            var valorLido = "inicial";
            validador.ValidateTag(elemento => elemento.Name.LocalName == "tiqueteSolicitacao",
                elemento => valorLido = elemento.Value);

            Assert.True(validador.Validate());
            Assert.Equal(" ", valorLido);
        }

        [Fact]
        public void ValidadorBaseMantemLeituraPadraoDeEspacos()
        {
            var validador = new ValidadorDeTeste { Xml = "<valor> </valor>" };
            var valorLido = "inicial";
            validador.ValidateTag(validador.CanValidate, elemento => valorLido = elemento.Value);

            Assert.True(validador.Validate());
            Assert.Equal(string.Empty, valorLido);
        }

#if INTEROP
        [Theory]
        [InlineData(false)]
        [InlineData(true)]
        public void EntradasComPreservamEspacoAntesDeValidar(bool arquivo)
        {
            var consulta = new ConsultarSituacaoSolicitacao { TiqueteSolicitacao = " " };
            var invalido = new ConsultarDebitos { Cnpj = "00000001", UrlRetorno = " " };
            ConferirCom(consulta.GerarXMLString(), "consultarSituacaoSolicitacao", arquivo, true);
            ConferirCom(invalido.GerarXMLString(), "consultarDebitos", arquivo, false);
        }

        private static void ConferirCom(string xml, string raiz, bool arquivo, bool esperado)
        {
            var validator = new ValidarSchema();
            var schema = "AACBS." + raiz + "_v1.00.xsd";
            if (arquivo)
            {
                var path = Path.GetTempFileName();
                try
                {
                    File.WriteAllText(path, xml);
                    validator.Validar(path, schema, AACBSFixture.Namespace);
                }
                finally { File.Delete(path); }
            }
            else validator.ValidarString(xml, schema, AACBSFixture.Namespace);
            Assert.Equal(esperado, validator.Success);
        }
#endif

        private static ValidarEstruturaXML.ResultadoValidacao Validar(XmlDocument document) =>
            new ValidarEstruturaXML().ValidarServico(document, new Configuracao { CodigoUF = (int)UFBrasil.AN });

        private sealed class ValidadorDeTeste : XmlValidatorBase
        {
            public override bool CanValidate(XElement element) => element.Name.LocalName == "valor";
        }
    }
}
