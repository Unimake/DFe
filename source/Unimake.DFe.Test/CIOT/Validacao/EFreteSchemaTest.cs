using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CIOT.Provedores;
using Unimake.Business.DFe.Servicos.CIOT.Provedores.EFrete;
using Unimake.Exceptions;
using Xunit;
using ConsultaServico = Unimake.Business.DFe.Servicos.CIOT.ConsultarCIOTGerado;

namespace Unimake.DFe.Test.CIOT.Validacao
{
    public class EFreteSchemaTest
    {
        private const string NamespaceCIOT = "http://www.antt.gov.br/ciot";

        public static IEnumerable<object[]> XmlsValidos => new[]
        {
            Caso("efrete-declaracao-carga-lotacao-completa.xml", Servico.CIOTDeclaracaoOperacaoTransporte),
            Caso("efrete-declaracao-carga-fracionada.xml", Servico.CIOTDeclaracaoOperacaoTransporte),
            Caso("efrete-declaracao-tac-agregado.xml", Servico.CIOTDeclaracaoOperacaoTransporte),
            Caso("efrete-consultar-ciot-gerado.xml", Servico.CIOTConsultarCIOTGerado),
            Caso("efrete-cancelamento-operacao-transporte.xml", Servico.CIOTCancelamentoOperacaoTransporte),
            Caso("efrete-encerramento-operacao-transporte.xml", Servico.CIOTEncerramentoOperacaoTransporte),
            Caso("efrete-consultar-situacao-transportador.xml", Servico.CIOTConsultarSituacaoTransportador),
            Caso("efrete-consultar-frota-transportador.xml", Servico.CIOTConsultarFrotaTransportador)
        };

        [Theory]
        [MemberData(nameof(XmlsValidos))]
        [Trait("DFe", "CIOT")]
        public void XmlEFreteValidoPassaNoSchemaEspecifico(string arquivo, Servico servico)
        {
            var documento = new XmlDocument();
            documento.Load(CaminhoRecurso(arquivo));
            var schema = EFreteSchemaResolver.ObterSchemaArquivo(servico);

            Assert.False(string.IsNullOrWhiteSpace(schema));
            var validador = new ValidarSchema();
            validador.Validar(documento, schema, NamespaceCIOT);

            Assert.True(validador.Success, validador.ErrorMessage);
        }

        [Theory]
        [InlineData("", "")]
        [InlineData("<ProvedorCIOT>EFrete</ProvedorCIOT>", "<TagDesconhecida>1</TagDesconhecida>")]
        [InlineData("<ProvedorCIOT>ANTT</ProvedorCIOT>", "")]
        [Trait("DFe", "CIOT")]
        public void SchemaEFreteRejeitaEstruturaOuProvedorInvalido(string provedor, string elementoAdicional)
        {
            var xml = File.ReadAllText(CaminhoRecurso("efrete-consultar-ciot-gerado.xml"));
            if (string.IsNullOrEmpty(provedor))
            {
                xml = xml.Replace("\t<ProvedorCIOT>EFrete</ProvedorCIOT>\r\n", string.Empty).Replace("\t<ProvedorCIOT>EFrete</ProvedorCIOT>\n", string.Empty);
            }
            else
            {
                xml = xml.Replace("<ProvedorCIOT>EFrete</ProvedorCIOT>", provedor + elementoAdicional);
            }

            var documento = new XmlDocument();
            documento.LoadXml(xml);
            var validador = new ValidarSchema();
            validador.Validar(documento, EFreteSchemaResolver.ObterSchemaArquivo(Servico.CIOTConsultarCIOTGerado), NamespaceCIOT);

            Assert.False(validador.Success);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void SchemaEFreteRejeitaTipoOperacaoForaDaEnumeracao()
        {
            var xml = File.ReadAllText(CaminhoRecurso("efrete-declaracao-carga-lotacao-completa.xml"))
                .Replace("<TipoOperacao>1</TipoOperacao>", "<TipoOperacao>99</TipoOperacao>");
            var documento = new XmlDocument();
            documento.LoadXml(xml);
            var validador = new ValidarSchema();
            validador.Validar(documento, EFreteSchemaResolver.ObterSchemaArquivo(Servico.CIOTDeclaracaoOperacaoTransporte), NamespaceCIOT);

            Assert.False(validador.Success);
        }

        [Theory]
        [InlineData("<TipoEmbalagem>Granel</TipoEmbalagem>", "<TipoEmbalagem>Volumes</TipoEmbalagem>")]
        [InlineData("<TipoDeCalculo>QuebraSomenteUltrapassado</TipoDeCalculo>", "<TipoDeCalculo>ComQuebra</TipoDeCalculo>")]
        [InlineData("<ValorParcela>3000.00</ValorParcela>", "<ValorParcela>0</ValorParcela>")]
        [InlineData("<UnidadeDeMedidaDaMercadoria>Kg</UnidadeDeMedidaDaMercadoria>", "<UnidadeDeMedidaDaMercadoria>CX</UnidadeDeMedidaDaMercadoria>")]
        [Trait("DFe", "CIOT")]
        public void SchemaEFreteRejeitaValoresForaDoContrato81(string valorValido, string valorInvalido)
        {
            var xml = File.ReadAllText(CaminhoRecurso("efrete-declaracao-carga-lotacao-completa.xml"))
                .Replace(valorValido, valorInvalido);
            var documento = new XmlDocument();
            documento.LoadXml(xml);
            var validador = new ValidarSchema();
            validador.Validar(documento, EFreteSchemaResolver.ObterSchemaArquivo(Servico.CIOTDeclaracaoOperacaoTransporte), NamespaceCIOT);

            Assert.False(validador.Success);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void SchemaEFreteRejeitaDataInvalida()
        {
            var xml = File.ReadAllText(CaminhoRecurso("efrete-declaracao-carga-lotacao-completa.xml"))
                .Replace("<DataFimViagem>2026-08-14</DataFimViagem>", "<DataFimViagem>2026-99-99</DataFimViagem>");
            var documento = new XmlDocument();
            documento.LoadXml(xml);
            var validador = new ValidarSchema();
            validador.Validar(documento, EFreteSchemaResolver.ObterSchemaArquivo(Servico.CIOTDeclaracaoOperacaoTransporte), NamespaceCIOT);

            Assert.False(validador.Success);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void SchemaEFreteRejeitaElementosForaDeOrdem()
        {
            var documento = new XmlDocument();
            documento.Load(CaminhoRecurso("efrete-declaracao-carga-lotacao-completa.xml"));
            var idOperacao = documento.GetElementsByTagName("IdOperacaoCliente", NamespaceCIOT)[0];
            var matriz = documento.GetElementsByTagName("MatrizCNPJ", NamespaceCIOT)[0];
            documento.DocumentElement.InsertBefore(matriz, idOperacao);
            var validador = new ValidarSchema();
            validador.Validar(documento, EFreteSchemaResolver.ObterSchemaArquivo(Servico.CIOTDeclaracaoOperacaoTransporte), NamespaceCIOT);

            Assert.False(validador.Success);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void ServicoEFreteValidaSchemaAntesDoEFreteValidator()
        {
            var xml = File.ReadAllText(CaminhoRecurso("efrete-consultar-ciot-gerado.xml"))
                .Replace("<IdOperacaoCliente>", "<TagDesconhecida>1</TagDesconhecida><IdOperacaoCliente>");
            var configuracao = new Configuracao { TipoAmbiente = TipoAmbiente.Homologacao };

            var exception = Assert.Throws<ValidarXMLException>(() => new ConsultaServico(xml, configuracao));

            Assert.Contains("TagDesconhecida", exception.Message);
            Assert.DoesNotContain("EFreteIntegrador", exception.Message);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void EFreteValidatorContinuaExecutandoDepoisDoSchema()
        {
            var xml = File.ReadAllText(CaminhoRecurso("efrete-consultar-ciot-gerado.xml"));
            var configuracao = new Configuracao { TipoAmbiente = TipoAmbiente.Homologacao };

            var exception = Assert.Throws<ValidarXMLException>(() => new ConsultaServico(xml, configuracao));

            Assert.Contains("EFreteIntegrador", exception.Message);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void ProvedorANTTContinuaSemSchemaEspecifico()
        {
            var provedorANTT = ProvedorCIOTFactory.Criar(ProvedorCIOT.ANTT);
            var provedorEFrete = ProvedorCIOTFactory.Criar(ProvedorCIOT.EFrete);

            Assert.Null(provedorANTT.ObterSchemaArquivo(Servico.CIOTConsultarCIOTGerado));
            Assert.Equal(
                "CIOT.EFrete.ciotEFrete_v1.00.xsd",
                provedorEFrete.ObterSchemaArquivo(Servico.CIOTConsultarCIOTGerado));
        }

        [Theory]
        [InlineData(Servico.CIOTGerarIdOperacaoTransporte)]
        [InlineData(Servico.CIOTRetificacaoOperacaoTransporte)]
        [InlineData(Servico.CIOTConsultarExcecao)]
        [Trait("DFe", "CIOT")]
        public void ServicosNaoSuportadosNaoPossuemSchemaEFrete(Servico servico)
        {
            Assert.Null(EFreteSchemaResolver.ObterSchemaArquivo(servico));
        }

        private static object[] Caso(string arquivo, Servico servico) => new object[] { arquivo, servico };

        private static string CaminhoRecurso(string arquivo) => Path.Combine(@"..\..\..\CIOT\Resources", arquivo);
    }
}
