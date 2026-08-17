using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;
using Xunit;

namespace Unimake.DFe.Test.CIOT.Serializacao
{
    public class ProvedorCIOTSerializacaoTest
    {
        public static IEnumerable<object[]> Envios => new[]
        {
            Caso("DeclaracaoOperacaoTransporte", "declaracaoOperacaoTransporte.xml", "declaracaoOperacaoTransporte_v1.00.xsd"),
            Caso("CancelamentoOperacaoTransporte", "cancelamentoOperacaoTransporte.xml", "cancelamentoOperacaoTransporte_v1.00.xsd"),
            Caso("EncerramentoOperacaoTransporte", "encerramentoOperacaoTransporte.xml", "encerramentoOperacaoTransporte_v1.00.xsd"),
            Caso("RetificacaoOperacaoTransporte", "retificacaoOperacaoTransporte.xml", "retificacaoOperacaoTransporte_v1.00.xsd"),
            Caso("GerarIdOperacaoTransporte", "gerarIdOperacaoTransporte.xml", "gerarIdOperacaoTransporte_v1.00.xsd"),
            Caso("ConsultarCIOTGerado", "consultarCIOTGerado.xml", "consultarCIOTGerado_v1.00.xsd"),
            Caso("ConsultarSituacaoTransportador", "consultarSituacaoTransportador.xml", "consultarSituacaoTransportador_v1.00.xsd"),
            Caso("ConsultarFrotaTransportador", "consultarFrotaTransportador.xml", "consultarFrotaTransportador_v1.00.xsd"),
            Caso("ConsultarExcecao", "consultarExcecao.xml", "consultarExcecao_v1.00.xsd")
        };

        [Theory]
        [MemberData(nameof(Envios))]
        [Trait("DFe", "CIOT")]
        public void SerializaDesserializaEOmiteProvedorNosNoveEnvios(string raiz, string arquivo, string schema)
        {
            var documentoANTT = Carregar(arquivo);
            var envio = Desserializar(raiz, documentoANTT);
            var propriedade = envio.GetType().GetProperty(nameof(Configuracao.ProvedorCIOT));

            Assert.NotNull(propriedade);
            Assert.Equal(ProvedorCIOT.ANTT, propriedade.GetValue(envio));
            Assert.Equal("ProvedorCIOT", PrimeiroElemento(documentoANTT).LocalName);
            ValidarSchema(documentoANTT, schema);

            var roundTripANTT = envio.GerarXML();
            Assert.Equal("ANTT", PrimeiroElemento(roundTripANTT).InnerText);
            Assert.Equal(documentoANTT.InnerText, roundTripANTT.InnerText);
            Assert.Equal(NomesElementos(documentoANTT), NomesElementos(roundTripANTT));

            propriedade.SetValue(envio, ProvedorCIOT.EFrete);
            var documentoEFrete = envio.GerarXML();
            Assert.Equal("ProvedorCIOT", PrimeiroElemento(documentoEFrete).LocalName);
            Assert.Equal("EFrete", PrimeiroElemento(documentoEFrete).InnerText);
            var envioEFrete = Desserializar(raiz, documentoEFrete);
            Assert.Equal(ProvedorCIOT.EFrete, propriedade.GetValue(envioEFrete));
            Assert.Equal(documentoEFrete.InnerText, envioEFrete.GerarXML().InnerText);
            Assert.Equal(NomesElementos(documentoEFrete), NomesElementos(envioEFrete.GerarXML()));
            ValidarSchema(documentoEFrete, schema);

            propriedade.SetValue(envio, null);
            var documentoSemProvedor = envio.GerarXML();
            Assert.Empty(documentoSemProvedor.GetElementsByTagName("ProvedorCIOT").Cast<XmlNode>());
            var envioSemProvedor = Desserializar(raiz, documentoSemProvedor);
            Assert.Null(propriedade.GetValue(envioSemProvedor));
            Assert.Equal(documentoSemProvedor.InnerText, envioSemProvedor.GerarXML().InnerText);
            Assert.Equal(NomesElementos(documentoSemProvedor), NomesElementos(envioSemProvedor.GerarXML()));
            ValidarSchema(documentoSemProvedor, schema);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void RetornosNaoExpoemProvedorCIOT()
        {
            var retornos = new[]
            {
                typeof(RetDeclaracaoOperacaoTransporte), typeof(RetCancelamentoOperacaoTransporte),
                typeof(RetEncerramentoOperacaoTransporte), typeof(RetRetificacaoOperacaoTransporte),
                typeof(RetGerarIdOperacaoTransporte), typeof(RetConsultarCIOTGerado),
                typeof(RetConsultarSituacaoTransportador), typeof(RetConsultarFrotaTransportador),
                typeof(RetConsultarExcecao)
            };

            Assert.All(retornos, tipo => Assert.Null(tipo.GetProperty(nameof(Configuracao.ProvedorCIOT))));
        }

        private static object[] Caso(string raiz, string arquivo, string schema) => new object[] { raiz, arquivo, schema };

        private static XmlDocument Carregar(string arquivo)
        {
            var documento = new XmlDocument();
            documento.Load(@"..\..\..\CIOT\Resources\" + arquivo);
            return documento;
        }

        private static XmlElement PrimeiroElemento(XmlDocument documento) =>
            documento.DocumentElement.ChildNodes.Cast<XmlNode>().OfType<XmlElement>().First();

        private static string[] NomesElementos(XmlDocument documento) =>
            documento.DocumentElement.ChildNodes.Cast<XmlNode>().OfType<XmlElement>().Select(x => x.LocalName).ToArray();

        private static XMLBase Desserializar(string raiz, XmlDocument documento)
        {
            switch (raiz)
            {
                case "DeclaracaoOperacaoTransporte": return new DeclaracaoOperacaoTransporte().LerXML<DeclaracaoOperacaoTransporte>(documento);
                case "CancelamentoOperacaoTransporte": return new CancelamentoOperacaoTransporte().LerXML<CancelamentoOperacaoTransporte>(documento);
                case "EncerramentoOperacaoTransporte": return new EncerramentoOperacaoTransporte().LerXML<EncerramentoOperacaoTransporte>(documento);
                case "RetificacaoOperacaoTransporte": return new RetificacaoOperacaoTransporte().LerXML<RetificacaoOperacaoTransporte>(documento);
                case "GerarIdOperacaoTransporte": return new GerarIdOperacaoTransporte().LerXML<GerarIdOperacaoTransporte>(documento);
                case "ConsultarCIOTGerado": return new ConsultarCIOTGerado().LerXML<ConsultarCIOTGerado>(documento);
                case "ConsultarSituacaoTransportador": return new ConsultarSituacaoTransportador().LerXML<ConsultarSituacaoTransportador>(documento);
                case "ConsultarFrotaTransportador": return new ConsultarFrotaTransportador().LerXML<ConsultarFrotaTransportador>(documento);
                case "ConsultarExcecao": return new ConsultarExcecao().LerXML<ConsultarExcecao>(documento);
                default: throw new ArgumentOutOfRangeException(nameof(raiz));
            }
        }

        private static void ValidarSchema(XmlDocument documento, string schema)
        {
            var validador = new ValidarSchema();
            validador.Validar(documento, "CIOT." + schema, "http://www.antt.gov.br/ciot");
            Assert.True(validador.Success, validador.ErrorMessage);
        }
    }
}
