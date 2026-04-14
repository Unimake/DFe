#pragma warning disable CS1591

using System.Collections.Generic;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.DFe.Test.Utility;
using Xunit;
using ConsultaEventosXml = Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.ConsultaEventosNFSeChaveAcesso;
using ConsultaEventosServico = Unimake.Business.DFe.Servicos.NFSe.ConsultaEventosNFSeChaveAcesso;

namespace Unimake.DFe.Test.NFSe
{
    /// <summary>
    /// Testar o serviço: ConsultarEventosNFSeChaveAcesso
    /// </summary>
    public class ConsultarEventosNFSeChaveAcessoTest
    {
        /// <summary>
        /// Monta os parâmetros, de forma dinâmica, para o cenário de testes
        /// </summary>
        public static IEnumerable<object[]> Parametros => TestUtility.PreparaDadosCenario("ConsultaEventosNFSeChaveAcesso");

        #region Testes de Integração

        /// <summary>
        /// Teste de comunicação com o webservice - Consultar Eventos da NFSe por Chave de Acesso
        /// Valida a resposta através da propriedade Result
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [MemberData(nameof(Parametros))]
        public void TesteConsultarEventosNFSeChaveAcesso(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codigoMunicipio)
        {
            var nomeXMLEnvio = "ConsultarEventosNFSeChaveAcesso-cons-chaveacesso.xml";
            var arqXML = $"..\\..\\..\\NFSe\\Resources\\{padraoNFSe}\\{versaoSchema}\\{nomeXMLEnvio}";

            Assert.True(File.Exists(arqXML), $"Arquivo {arqXML} não foi localizado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(arqXML);

            var xmlString = conteudoXML.InnerXml;
            Assert.NotEmpty(xmlString);
            Assert.Contains("ChaveNFSe", xmlString);

            var consulta = XMLUtility.Deserializar<ConsultaEventosXml>(conteudoXML.InnerXml);
            Assert.NotNull(consulta);
            Assert.NotEmpty(consulta.ChaveNFSe);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital,
                CodigoMunicipio = codigoMunicipio,
                Servico = Servico.NFSeConsultarEventosNFSeChaveAcesso,
                SchemaVersao = versaoSchema
            };

            var servico = new ConsultaEventosServico(conteudoXML, configuracao);
            servico.Executar();

            Assert.NotNull(servico.Result);

            if (!string.IsNullOrWhiteSpace(servico.Result?.StatusProcessamento))
            {
                Assert.NotEmpty(servico.Result.StatusProcessamento);
            }

            if (servico.Result?.LoteDFe?.Count > 0)
            {
                var primeiroDocumento = servico.Result.LoteDFe[0];
                Assert.NotEmpty(primeiroDocumento.ChaveAcesso);
                Assert.NotEmpty(primeiroDocumento.TipoEvento);
            }

            if (servico.Result?.Erros?.Count > 0)
            {
                var primeiroErro = servico.Result.Erros[0];
                Assert.NotEmpty(primeiroErro.Codigo);
                Assert.NotEmpty(primeiroErro.Descricao);
            }

            Assert.Multiple(() => TestUtility.AnalisaResultado(servico));
        }

        #endregion Testes de Integração

        /// <summary>
        /// Teste de serialização/deserialização e execução do serviço via objeto
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [MemberData(nameof(Parametros))]
        public void ConsultarEventosNFSeChaveAcessoExecutar(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codigoMunicipio)
        {
            var parametros = new ConsultaEventosXml
            {
                ChaveNFSe = "12345678901234567890123456789012345678901234567890"
            };

            var parametrosXML = parametros.GerarXML().OuterXml;

            Assert.NotEmpty(parametrosXML);
            Assert.Contains("12345678901234567890123456789012345678901234567890", parametrosXML);

            var parametrosDesserializado = XMLUtility.Deserializar<ConsultaEventosXml>(parametrosXML);
            Assert.NotNull(parametrosDesserializado);
            Assert.Equal("12345678901234567890123456789012345678901234567890", parametrosDesserializado.ChaveNFSe);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital,
                CodigoMunicipio = codigoMunicipio,
                Servico = Servico.NFSeConsultarEventosNFSeChaveAcesso,
                SchemaVersao = versaoSchema
            };

            var servico = new ConsultaEventosServico(parametros, configuracao);
            servico.Executar();

            Assert.NotNull(servico.Result);

            if (servico.Result?.LoteDFe?.Count > 0)
            {
                Assert.NotEmpty(servico.Result.LoteDFe);

                foreach (var evento in servico.Result.LoteDFe)
                {
                    Assert.NotNull(evento);
                    Assert.NotEmpty(evento.ChaveAcesso);
                }
            }

            if (servico.Result?.Erros?.Count > 0)
            {
                Assert.NotEmpty(servico.Result.Erros);
            }
        }
    }
}
