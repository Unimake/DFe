using System.IO;
using System.Linq;
using System.Xml;
using System.Xml.Linq;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CIOT.Provedores.EFrete;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;
using Xunit;

namespace Unimake.DFe.Test.CIOT.Serializacao
{
    public class EFreteSerializacaoDesserializacaoTest
    {
        [Fact]
        [Trait("DFe", "CIOT")]
        public void SerializaDesserializaRetornoDeclaracaoEFrete()
        {
            var retorno = Comparar<RetDeclaracaoOperacaoTransporte>(@"..\..\..\CIOT\Resources\efrete-ret-declaracao-operacao-transporte.xml");
            Assert.Equal("992000000126", retorno.IdOperacaoTransporte);
            Assert.Equal("PROTO-EFRETE-DECL-001", retorno.Protocolo);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\efrete-consultar-ciot-gerado.xml", false)]
        [InlineData(@"..\..\..\CIOT\Resources\efrete-ret-consultar-ciot-gerado.xml", true)]
        public void SerializaDesserializaConsultaCIOTGeradoEFrete(string caminho, bool retorno)
        {
            if (retorno)
            {
                var resultado = Comparar<RetConsultarCIOTGerado>(caminho);
                Assert.Equal("EmViagem", resultado.EstadoCIOT);
                Assert.Equal("PROTO-EFRETE-CONS-001", resultado.Protocolo);
            }
            else
            {
                var requisicao = Comparar<ConsultarCIOTGerado>(caminho);
                Assert.Equal("CIOT-LOTACAO-0001", requisicao.IdOperacaoCliente);
                Validar(requisicao, Servico.CIOTConsultarCIOTGerado);
            }
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\efrete-cancelamento-operacao-transporte.xml", false)]
        [InlineData(@"..\..\..\CIOT\Resources\efrete-ret-cancelamento-operacao-transporte.xml", true)]
        public void SerializaDesserializaCancelamentoEFrete(string caminho, bool retorno)
        {
            if (retorno)
            {
                var resultado = Comparar<RetCancelamentoOperacaoTransporte>(caminho);
                Assert.Equal("PROTO-EFRETE-CANC-001", resultado.Protocolo);
            }
            else
            {
                var requisicao = Comparar<CancelamentoOperacaoTransporte>(caminho);
                Validar(requisicao, Servico.CIOTCancelamentoOperacaoTransporte);
            }
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\efrete-encerramento-operacao-transporte.xml", false)]
        [InlineData(@"..\..\..\CIOT\Resources\efrete-ret-encerramento-operacao-transporte.xml", true)]
        public void SerializaDesserializaEncerramentoEFrete(string caminho, bool retorno)
        {
            if (retorno)
            {
                var resultado = Comparar<RetEncerramentoOperacaoTransporte>(caminho);
                Assert.Equal("PROTO-EFRETE-ENC-001", resultado.Protocolo);
                Assert.Equal(default, resultado.DataEncerramento);
            }
            else
            {
                var requisicao = Comparar<EncerramentoOperacaoTransporte>(caminho);
                Validar(requisicao, Servico.CIOTEncerramentoOperacaoTransporte);
            }
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\efrete-consultar-situacao-transportador.xml", false)]
        [InlineData(@"..\..\..\CIOT\Resources\efrete-ret-consultar-situacao-transportador.xml", true)]
        public void SerializaDesserializaSituacaoTransportadorEFrete(string caminho, bool retorno)
        {
            if (retorno)
            {
                var resultado = Comparar<RetConsultarSituacaoTransportador>(caminho);
                Assert.True(resultado.RNTRCAtivo);
                Assert.True(resultado.EquiparadoTAC);
            }
            else
            {
                var requisicao = Comparar<ConsultarSituacaoTransportador>(caminho);
                Assert.Equal(2, requisicao.PlacasConsulta.Count);
                Validar(requisicao, Servico.CIOTConsultarSituacaoTransportador);
            }
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\efrete-consultar-frota-transportador.xml", false)]
        [InlineData(@"..\..\..\CIOT\Resources\efrete-ret-consultar-frota-transportador.xml", true)]
        public void SerializaDesserializaFrotaTransportadorEFrete(string caminho, bool retorno)
        {
            if (retorno)
            {
                var resultado = Comparar<RetConsultarFrotaTransportador>(caminho);
                Assert.Equal(2, resultado.Frota.Count);
                Assert.True(resultado.Frota[0].SituacaoVeiculoFrotaTransportador);
                Assert.False(resultado.Frota[1].SituacaoVeiculoFrotaTransportador);
            }
            else
            {
                var requisicao = Comparar<ConsultarFrotaTransportador>(caminho);
                Assert.Equal(2, requisicao.Placas.Count);
                Validar(requisicao, Servico.CIOTConsultarFrotaTransportador);
            }
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\efrete-declaracao-carga-lotacao-completa.xml", TipoOperacaoTransporteCIOT.CargaLotacao)]
        [InlineData(@"..\..\..\CIOT\Resources\efrete-declaracao-carga-fracionada.xml", TipoOperacaoTransporteCIOT.CargaFracionada)]
        [InlineData(@"..\..\..\CIOT\Resources\efrete-declaracao-tac-agregado.xml", TipoOperacaoTransporteCIOT.TACAgregado)]
        public void DesserializaSerializaEComparaEstruturaCompleta(string caminho, TipoOperacaoTransporteCIOT modalidade)
        {
            Assert.True(File.Exists(caminho), "Modelo XML eFrete não localizado: " + caminho);

            var esperado = XDocument.Load(caminho);
            var declaracao = new DeclaracaoOperacaoTransporte().LerXML<DeclaracaoOperacaoTransporte>(ParaXmlDocument(esperado));
            var gerado = XDocument.Parse(declaracao.GerarXML().OuterXml);

            Assert.Equal(modalidade, declaracao.TipoOperacao);
            Assert.True(
                XNode.DeepEquals(Normalizar(esperado.Root), Normalizar(gerado.Root)),
                "O XML gerado não preservou integralmente elementos, ordem e valores do modelo eFrete.\nEsperado:\n" + esperado + "\nGerado:\n" + gerado);

            EFreteValidator.Validar(declaracao, Servico.CIOTDeclaracaoOperacaoTransporte, CriarConfiguracao());
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void ModeloLotacaoContemTodosOsGruposEFrete()
        {
            var declaracao = Ler(@"..\..\..\CIOT\Resources\efrete-declaracao-carga-lotacao-completa.xml");

            Assert.Equal("CIOT-LOTACAO-0001", declaracao.IdOperacaoCliente);
            Assert.Equal("12345678000270", declaracao.FilialCNPJ);
            Assert.Equal(2, declaracao.Veiculos.Count);
            Assert.Equal(6, new[] { declaracao.Contratado, declaracao.Contratante, declaracao.Destinatario, declaracao.Subcontratante, declaracao.Consignatario, declaracao.TomadorServico }.Count(x => x != null));
            Assert.NotNull(declaracao.Motorista.Celular);
            Assert.Equal("transportador@example.com", declaracao.Contratado.EMail);
            Assert.NotNull(declaracao.Contratado.Telefones.Celular);
            Assert.NotNull(declaracao.Contratado.Telefones.Fixo);
            Assert.NotNull(declaracao.Contratado.Telefones.Fax);
            Assert.NotNull(declaracao.Impostos);
            Assert.Equal(2, declaracao.ObservacoesAoTransportador.Count);
            Assert.Equal(2, declaracao.ObservacoesAoCredenciado.Count);
            Assert.Equal(2, declaracao.InfPagamento.Count);
            Assert.NotNull(declaracao.InfPagamento.Single(x => x.IdPagamentoCliente == "ID-PAG-BANCO-001").NumeroConta);
            Assert.NotNull(declaracao.InfPagamento.Single(x => x.IdPagamentoCliente == "ID-PAG-PIX-002").ChavePix);
            Assert.Single(declaracao.OrigemDestino);
            Assert.Single(declaracao.OrigemDestino[0].NotasFiscais);
            Assert.NotNull(declaracao.OrigemDestino[0].NotasFiscais[0].DiferencaDeFrete);
            Assert.NotNull(declaracao.OrigemDestino[0].NotasFiscais[0].ToleranciaDePerdaDeMercadoria);
            Assert.True(declaracao.InfIndicadoresOperacionais.IndAltoDesempenho);
            Assert.True(declaracao.InfIndicadoresOperacionais.IndRetornoVazio);
            Assert.True(declaracao.InfIndicadoresOperacionais.ComposicaoVeicular);
            Assert.False(declaracao.TomadorServico.ResponsavelPeloPagamentoSpecified);
            Assert.DoesNotContain(declaracao.GerarXML().GetElementsByTagName("ResponsavelPeloPagamento").Cast<XmlNode>(),
                x => x.ParentNode.LocalName == "TomadorServico");
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void ModeloFracionadoContemContratantesAdicionais()
        {
            var declaracao = Ler(@"..\..\..\CIOT\Resources\efrete-declaracao-carga-fracionada.xml");

            Assert.Equal(2, declaracao.DadosCarga.ContratantesCargFrac.Count);
            Assert.Equal("11111111000191", declaracao.DadosCarga.ContratantesCargFrac[0]);
            Assert.Equal("22222222000122", declaracao.DadosCarga.ContratantesCargFrac[1]);
            Assert.Equal("eFRETE", declaracao.TipoPagamentoEFrete);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void ModeloTacAgregadoNaoSerializaGruposProibidos()
        {
            var declaracao = Ler(@"..\..\..\CIOT\Resources\efrete-declaracao-tac-agregado.xml");
            var xml = declaracao.GerarXML();

            Assert.Empty(declaracao.OrigemDestino);
            Assert.Null(declaracao.DadosCarga);
            Assert.Null(declaracao.Destinatario);
            Assert.Null(declaracao.InfIndicadoresOperacionais);
            Assert.Empty(xml.GetElementsByTagName("DataInicioViagem").Cast<XmlNode>());
            Assert.Equal("987654321", declaracao.Contratante.RNTRC);
        }

        private static Configuracao CriarConfiguracao() => new Configuracao
        {
            TipoAmbiente = TipoAmbiente.Homologacao,
            ProvedorCIOT = ProvedorCIOT.EFrete,
            EFreteIntegrador = "INTEGRADOR-SINTETICO",
            EFreteToken = "TOKEN-SINTETICO"
        };

        private static DeclaracaoOperacaoTransporte Ler(string caminho)
        {
            var documento = XDocument.Load(caminho);
            return new DeclaracaoOperacaoTransporte().LerXML<DeclaracaoOperacaoTransporte>(ParaXmlDocument(documento));
        }

        private static T Comparar<T>(string caminho) where T : XMLBase, new()
        {
            Assert.True(File.Exists(caminho), "Modelo XML eFrete não localizado: " + caminho);
            var esperado = XDocument.Load(caminho);
            var objeto = new T().LerXML<T>(ParaXmlDocument(esperado));
            var gerado = XDocument.Parse(objeto.GerarXML().OuterXml);

            Assert.True(
                XNode.DeepEquals(Normalizar(esperado.Root), Normalizar(gerado.Root)),
                "O XML gerado não preservou integralmente o modelo eFrete.\nEsperado:\n" + esperado + "\nGerado:\n" + gerado);
            return objeto;
        }

        private static void Validar(XMLBase requisicao, Servico servico) => EFreteValidator.Validar(requisicao, servico, CriarConfiguracao());

        private static XmlDocument ParaXmlDocument(XDocument documento)
        {
            var resultado = new XmlDocument();
            resultado.LoadXml(documento.ToString(SaveOptions.DisableFormatting));
            return resultado;
        }

        private static XElement Normalizar(XElement elemento) => new XElement(
            elemento.Name,
            elemento.Attributes().Where(x => !x.IsNamespaceDeclaration),
            elemento.Nodes().Select(x => x is XElement filho ? (object)Normalizar(filho) : x));
    }
}
