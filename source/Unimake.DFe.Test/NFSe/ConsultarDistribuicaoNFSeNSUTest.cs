#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta;
using NFSeNacional = Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.NFSe;
using Unimake.DFe.Test.Utility;
using Xunit;

namespace Unimake.DFe.Test.NFSe
{
    /// <summary>
    /// Testar o serviço: ConsultarDistribuicaoNFSeNSU
    /// </summary>
    public class ConsultarDistribuicaoNFSeNSUTest
    {
        /// <summary>
        /// Monta os parâmetros, de forma dinâmica, para o cenário de testes
        /// </summary>
        public static IEnumerable<object[]> Parametros => TestUtility.PreparaDadosCenario("ConsultarDistribuicaoNFSeNSU");

        #region Testes de Integração

        /// <summary>
        /// Teste de comunicação com o webservice - Consultar Distribuição de NFSe por NSU
        /// Valida a resposta através da propriedade Result
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [MemberData(nameof(Parametros))]
        public void TesteConsultarDistribuicaoNFSeNSU(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codigoMunicipio)
        {
            var nomeXMLEnvio = "ConsultarDistribuicaoNFSeNSU-cons-nsunfse.xml";
            var arqXML = $"..\\..\\..\\NFSe\\Resources\\{padraoNFSe}\\{versaoSchema}\\{nomeXMLEnvio}";

            Assert.True(File.Exists(arqXML), $"Arquivo {arqXML} não foi localizado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(arqXML);

            var xmlString = conteudoXML.InnerXml;
            Assert.NotEmpty(xmlString);
            Assert.Contains("NSU", xmlString);
            Assert.Contains("tipoNSU", xmlString);
            Assert.Contains("lote", xmlString);

            var distribuicao = XMLUtility.Deserializar<DistribuicaoNFSe>(conteudoXML.InnerXml);
            Assert.NotNull(distribuicao);
            Assert.NotEmpty(distribuicao.NSU);
            Assert.NotEmpty(distribuicao.TipoNSU);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital,
                CodigoMunicipio = codigoMunicipio,
                Servico = Servico.NFSeConsultarDistribuicaoNFSeNSU,
                SchemaVersao = versaoSchema
            };

            var consultarNSU = new ConsultarDistribuicaoNFSeNSU(conteudoXML, configuracao);
            consultarNSU.Executar();

            Assert.NotNull(consultarNSU.Result);
            Assert.NotNull(consultarNSU.Result.StatusProcessamento);
            Assert.NotEmpty(consultarNSU.Result.StatusProcessamento);

            if (consultarNSU.Result.LoteDFe?.Count > 0)
            {
                var primeiroDocumento = consultarNSU.Result.LoteDFe[0];
                Assert.True(primeiroDocumento.NSU > 0);
                Assert.NotEmpty(primeiroDocumento.ChaveAcesso);
                Assert.NotEmpty(primeiroDocumento.TipoDocumento);
                Assert.NotNull(primeiroDocumento.ArquivoXml);
                Assert.NotNull(primeiroDocumento.ArquivoXml.InfNFSe);
                
                var conteudoDescompactado = primeiroDocumento.ConteudoXML;
                Assert.NotEmpty(conteudoDescompactado);
            }

            if (consultarNSU.Result.Erros?.Count > 0)
            {
                var primeiroErro = consultarNSU.Result.Erros[0];
                Assert.NotEmpty(primeiroErro.Codigo);
                Assert.NotEmpty(primeiroErro.Descricao);
            }

            Assert.Multiple(() => TestUtility.AnalisaResultado(consultarNSU));
        }

        /// <summary>
        /// Teste de gravação de XMLs - Salva os XMLs descompactados em pasta no HD
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [MemberData(nameof(Parametros))]
        public void TesteConsultarDistribuicaoNFSeNSUGravarXML(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codigoMunicipio)
        {
            var nomeXMLEnvio = "ConsultarDistribuicaoNFSeNSU-cons-nsunfse.xml";
            var arqXML = $"..\\..\\..\\NFSe\\Resources\\{padraoNFSe}\\{versaoSchema}\\{nomeXMLEnvio}";

            Assert.True(File.Exists(arqXML), $"Arquivo {arqXML} não foi localizado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital,
                CodigoMunicipio = codigoMunicipio,
                Servico = Servico.NFSeConsultarDistribuicaoNFSeNSU,
                SchemaVersao = versaoSchema
            };

            var consultarNSU = new ConsultarDistribuicaoNFSeNSU(conteudoXML, configuracao);
            consultarNSU.Executar();

            Assert.NotNull(consultarNSU.Result);

            var pastaTemp = Path.Combine(Path.GetTempPath(), $"ConsultarDistribuicaoNFSeNSU_{DateTime.Now:yyyyMMddHHmmss}");
            Directory.CreateDirectory(pastaTemp);

            try
            {
                if (consultarNSU.NFSesRecebidas != null && consultarNSU.NFSesRecebidas.Count > 0)
                {
                    consultarNSU.GravarXMLNFSe(pastaTemp);

                    var arquivosGravados = Directory.GetFiles(pastaTemp, "*.xml");
                    Assert.NotEmpty(arquivosGravados);
                    Assert.Equal(consultarNSU.NFSesRecebidas.Count, arquivosGravados.Length);

                    foreach (var arquivo in arquivosGravados)
                    {
                        var conteudo = File.ReadAllText(arquivo);
                        Assert.NotEmpty(conteudo);
                        Assert.Contains("NFSe", conteudo); 
                    }
                }

                if (consultarNSU.NFSesDesserializadas != null && consultarNSU.NFSesDesserializadas.Count > 0)
                {
                    Assert.Equal(consultarNSU.NFSesRecebidas.Count, consultarNSU.NFSesDesserializadas.Count);

                    foreach (var nfse in consultarNSU.NFSesDesserializadas)
                    {
                        Assert.NotNull(nfse);
                    }
                }
            }
            finally
            {
                try
                {
                    Directory.Delete(pastaTemp, true);
                }
                catch { }
            }
        }

        #endregion Testes de Integração

        [Theory]
        [Trait("DFe", "NFSe")]
        [MemberData(nameof(Parametros))]
        public void ConsultarDistribuicaoNFSeNSUExecutar(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codigoMunicipio)
        {
            var parametros = new DistribuicaoNFSe
            {
                NSU = "000000000000001",
                TipoNSU = "DISTRIBUICAO",
                Lote = "false"
            };

            var parametrosXML = parametros.GerarXML().OuterXml;

            Assert.NotEmpty(parametrosXML);
            Assert.Contains("000000000000001", parametrosXML);
            Assert.Contains("DISTRIBUICAO", parametrosXML);

            var parametrosDesserializado = XMLUtility.Deserializar<DistribuicaoNFSe>(parametrosXML);
            Assert.NotNull(parametrosDesserializado);
            Assert.Equal("000000000000001", parametrosDesserializado.NSU);
            Assert.Equal("DISTRIBUICAO", parametrosDesserializado.TipoNSU);
            Assert.Equal("false", parametrosDesserializado.Lote);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital,
                CodigoMunicipio = codigoMunicipio,
                Servico = Servico.NFSeConsultarDistribuicaoNFSeNSU,
                SchemaVersao = versaoSchema
            };

            var servico = new ConsultarDistribuicaoNFSeNSU(parametros, configuracao);
            servico.Executar();

            Assert.NotNull(servico.Result);
            Assert.NotEmpty(servico.Result.StatusProcessamento);
            Assert.NotEmpty(servico.Result.TipoAmbiente);

            if (servico.NFSesRecebidas != null && servico.NFSesRecebidas.Count > 0)
            {
                Assert.NotEmpty(servico.NFSesRecebidas);

                if (servico.NFSesDesserializadas != null && servico.NFSesDesserializadas.Count > 0)
                {
                    Assert.Equal(servico.NFSesRecebidas.Count, servico.NFSesDesserializadas.Count);

                    foreach (var nfseDesserializada in servico.NFSesDesserializadas)
                    {
                        Assert.NotNull(nfseDesserializada);
                        Assert.NotNull(nfseDesserializada.InfNFSe);
                    }
                }
            }

            if (servico.Result.Erros != null && servico.Result.Erros.Count > 0)
            {
                Assert.NotEmpty(servico.Result.Erros);
            }
        }
    }
}