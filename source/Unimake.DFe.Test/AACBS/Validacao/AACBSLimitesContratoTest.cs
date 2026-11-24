using System;
using System.IO;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.AACBS;
using Xunit;

namespace Unimake.DFe.Test.AACBS.Validacao
{
    [Trait("DFe", "AACBS")]
    public class AACBSLimitesContratoTest
    {
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
    }
}
