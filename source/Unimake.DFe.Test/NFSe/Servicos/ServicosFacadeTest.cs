using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe.Servicos
{
    /// <summary>
    /// Testes das fachadas que centralizam o consumo dos serviços municipais de NFSe.
    /// </summary>
    public class ServicosFacadeTest
    {
        private const string NamespaceServicosNFSe = "Unimake.Business.DFe.Servicos.NFSe";

        /// <summary>
        /// Mapeamentos das fachadas para as tags legadas de configuração.
        /// </summary>
        public static IEnumerable<object[]> Mapeamentos => new[]
        {
            Mapear(typeof(GerarNfse), Servico.NFSeGerarNfse, "GerarNfse"),
            Mapear(typeof(GerarNfse), Servico.NFSeRecepcionarLoteRps, "RecepcionarLoteRps"),
            Mapear(typeof(GerarNfse), Servico.NFSeRecepcionarLoteRpsSincrono, "RecepcionarLoteRpsSincrono"),
            Mapear(typeof(GerarNfse), Servico.NFSeGerarNfseIndicativoDecisaoJudicial, "GerarNfseIndicativoDecisaoJudicial"),
            Mapear(typeof(GerarNfse), Servico.NFSeEnviarLoteNotas, "EnviarLoteNotas"),
            Mapear(typeof(GerarNfse), Servico.NFSeEnvioLoteRps, "EnvioLoteRps"),
            Mapear(typeof(GerarNfse), Servico.NFSeEnvioRps, "EnvioRps"),
            Mapear(typeof(GerarNfse), Servico.NFSeEmissaoNota, "EmissaoNota"),
            Mapear(typeof(GerarNfse), Servico.NFSeTesteEnvioLoteRps, "TesteEnvioLoteRps"),
            Mapear(typeof(CancelarNfse), Servico.NFSeCancelarNfse, "CancelarNfse"),
            Mapear(typeof(CancelarNfse), Servico.NFSeCancelamentoNfe, "CancelamentoNfe"),
            Mapear(typeof(CancelarNfse), Servico.NFSeCancelarNotaFiscal, "CancelarNotaFiscal"),
            Mapear(typeof(ConsultarNfse), Servico.NFSeConsultarNfse, "ConsultarNfse"),
            Mapear(typeof(ConsultarNfse), Servico.NFSeConsultarNfseFaixa, "ConsultarNfseFaixa"),
            Mapear(typeof(ConsultarNfse), Servico.NFSeConsultarNfseServicoPrestado, "ConsultarNfseServicoPrestado"),
            Mapear(typeof(ConsultarNfse), Servico.NFSeConsultarNotaFiscal, "ConsultarNotaFiscal"),
            Mapear(typeof(ConsultarNfse), Servico.NFSeConsultarNotaValida, "ConsultarNotaValida"),
            Mapear(typeof(ConsultarNfse), Servico.NFSeObterNotaFiscalXml, "ObterNotaFiscalXml"),
            Mapear(typeof(ConsultarNfse), Servico.NFSeConsultaNFeEmitidas, "ConsultaNFeEmitidas"),
            Mapear(typeof(ConsultarNfse), Servico.NFSeConsultarNotaPrestador, "ConsultarNotaPrestador"),
            Mapear(typeof(ConsultarNfsePorRps), Servico.NFSeConsultarNfsePorRps, "ConsultarNfsePorRps")
        };

        /// <summary>
        /// Classes legadas e fachadas recomendadas nas mensagens de obsolescência.
        /// </summary>
        public static IEnumerable<object[]> ClassesObsoletas => new[]
        {
            Obsoleta("RecepcionarLoteRps", "GerarNfse"),
            Obsoleta("RecepcionarLoteRpsSincrono", "GerarNfse"),
            Obsoleta("GerarNfseIndicativoDecisaoJudicial", "GerarNfse"),
            Obsoleta("EnviarLoteNotas", "GerarNfse"),
            Obsoleta("EnvioLoteRps", "GerarNfse"),
            Obsoleta("EnvioRps", "GerarNfse"),
            Obsoleta("EmissaoNota", "GerarNfse"),
            Obsoleta("TesteEnvioLoteRps", "GerarNfse"),
            Obsoleta("CancelamentoNfe", "CancelarNfse"),
            Obsoleta("CancelarNotaFiscal", "CancelarNfse"),
            Obsoleta("ConsultarNfseFaixa", "ConsultarNfse"),
            Obsoleta("ConsultarNfseServicoPrestado", "ConsultarNfse"),
            Obsoleta("ConsultarNotaFiscal", "ConsultarNfse"),
            Obsoleta("ConsultarNotaValida", "ConsultarNfse"),
            Obsoleta("ObterNotaFiscalXml", "ConsultarNfse"),
            Obsoleta("ConsultaNFeEmitidas", "ConsultarNfse"),
            Obsoleta("ConsultarNotaPrestador", "ConsultarNfse"),
            Obsoleta("CancelaNota", "CancelarNfse"),
            Obsoleta("ConsultarRpsServicoPrestado", "ConsultarNfsePorRps")
        };

        /// <summary>
        /// Confirma todos os roteamentos entre enum, fachada e tag de configuração.
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [MemberData(nameof(Mapeamentos))]
        public void FachadaDeveResolverTagDoServico(Type tipoFachada, Servico servico, string tagEsperada)
        {
            var configuracao = new Configuracao { Servico = servico };
            var fachada = (Unimake.Business.DFe.Servicos.ServicoBase)Activator.CreateInstance(tipoFachada);
            fachada.Configuracoes = configuracao;
            fachada.RetornoWSString = "estado-preservado";

            var tagResolvida = ObterNomeTagServico(fachada);

            Assert.Multiple(
                () => Assert.Equal(tagEsperada, tagResolvida),
                () => Assert.Same(configuracao, fachada.Configuracoes),
                () => Assert.Equal("estado-preservado", fachada.RetornoWSString));
        }

        /// <summary>
        /// Confirma que o novo enum carrega o endpoint judicial do ambiente nacional.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFSe")]
        public void GerarNfseDeveCarregarEndpointDeDecisaoJudicial()
        {
            var configuracao = new Configuracao
            {
                CodigoMunicipio = 1001058,
                SchemaVersao = "1.01",
                Servico = Servico.NFSeGerarNfseIndicativoDecisaoJudicial,
                TipoAmbiente = TipoAmbiente.Homologacao,
                TipoDFe = TipoDFe.NFSe
            };
            var fachada = new GerarNfse { Configuracoes = configuracao };
            DefinirConteudoXML(fachada, "<NFSe xmlns=\"http://www.sped.fazenda.gov.br/nfse\" />");

            DefinirConfiguracao(fachada);

            Assert.Multiple(
                () => Assert.Equal(132, (int)configuracao.Servico),
                () => Assert.Equal(PadraoNFSe.NACIONAL, configuracao.PadraoNFSe),
                () => Assert.Contains("decisao-judicial/nfse", configuracao.RequestURI));
        }

        /// <summary>
        /// Confirma que serviço fora do grupo falha antes do carregamento da configuração.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFSe")]
        public void FachadaDeveRejeitarServicoForaDoGrupo()
        {
            var fachada = new GerarNfse
            {
                Configuracoes = new Configuracao { Servico = Servico.NFSeCancelarNfse }
            };

            var exception = Assert.Throws<TargetInvocationException>(() => ObterNomeTagServico(fachada));
            var innerException = Assert.IsType<InvalidOperationException>(exception.InnerException);

            Assert.Multiple(
                () => Assert.Contains(nameof(GerarNfse), innerException.Message),
                () => Assert.Contains(nameof(Servico.NFSeCancelarNfse), innerException.Message),
                () => Assert.Contains(nameof(Servico.NFSeGerarNfse), innerException.Message));
        }

        /// <summary>
        /// Confirma que uma configuração previamente definida não é recarregada nem validada pela fachada.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFSe")]
        public void FachadaDevePreservarConfiguracaoDefinida()
        {
            var configuracao = new Configuracao
            {
                Definida = true,
                RequestURI = "https://endpoint.example.com",
                Servico = Servico.NFSeCancelarNfse
            };
            var fachada = new GerarNfse { Configuracoes = configuracao };

            DefinirConfiguracao(fachada);

            Assert.Multiple(
                () => Assert.Same(configuracao, fachada.Configuracoes),
                () => Assert.Equal("https://endpoint.example.com", configuracao.RequestURI),
                () => Assert.True(configuracao.Definida));
        }

        /// <summary>
        /// Confirma que tipos legados continuam resolvendo a configuração pelo próprio nome.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFSe")]
        public void ClasseJudicialLegadaDeveContinuarAceitandoEnumAntigo()
        {
            var tipoLegado = ObterTipoServico("GerarNfseIndicativoDecisaoJudicial");
            var servicoLegado = (Unimake.Business.DFe.Servicos.ServicoBase)Activator.CreateInstance(tipoLegado);
            servicoLegado.Configuracoes = new Configuracao { Servico = Servico.NFSeGerarNfse };

            Assert.Equal("GerarNfseIndicativoDecisaoJudicial", ObterNomeTagServico(servicoLegado));
        }

        /// <summary>
        /// Confirma que as classes absorvidas geram somente aviso e indicam a fachada recomendada.
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [MemberData(nameof(ClassesObsoletas))]
        public void ClasseLegadaDeveSerObsoletaComAviso(string nomeClasse, string fachadaRecomendada)
        {
            var atributo = ObterTipoServico(nomeClasse).GetCustomAttribute<ObsoleteAttribute>();

            Assert.Multiple(
                () => Assert.NotNull(atributo),
                () => Assert.False(atributo.IsError),
                () => Assert.Contains(fachadaRecomendada, atributo.Message));
        }

        /// <summary>
        /// Confirma a obsolescência dos enums que não possuem endpoint configurado.
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData("NFSeCancelaNota", "CancelarNfse")]
        [InlineData("NFSeConsultarRpsServicoPrestado", "ConsultarNfsePorRps")]
        public void EnumSemEndpointDeveSerObsoletoComAviso(string nomeEnum, string fachadaRecomendada)
        {
            var campo = typeof(Servico).GetField(nomeEnum);
            var atributo = campo.GetCustomAttribute<ObsoleteAttribute>();

            Assert.Multiple(
                () => Assert.NotNull(atributo),
                () => Assert.False(atributo.IsError),
                () => Assert.Contains(fachadaRecomendada, atributo.Message));
        }

        /// <summary>
        /// Confirma que toda classe concreta de serviço possui tag de configuração ou exceção documentada.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFSe")]
        public void ClassesDeServicoDevemPossuirTagDeConfiguracao()
        {
            var assembly = typeof(GerarNfse).Assembly;
            var tags = CarregarTagsConfiguradas(assembly);
            var excecoes = new HashSet<string>(StringComparer.Ordinal)
            {
                "CancelaNota",
                "ConsultarRpsServicoPrestado"
            };
            var classesSemTag = assembly
                .GetTypes()
                .Where(type => type.IsClass &&
                               !type.IsAbstract &&
                               type.IsPublic &&
                               type.Namespace == NamespaceServicosNFSe &&
                               typeof(Unimake.Business.DFe.Servicos.NFSe.ServicoBase).IsAssignableFrom(type))
                .Select(type => type.Name)
                .Where(nome => !tags.Contains(nome) && !excecoes.Contains(nome))
                .OrderBy(nome => nome)
                .ToArray();

            Assert.Multiple(
                () => Assert.Equal(46, tags.Count),
                () => Assert.Empty(classesSemTag));
        }

        private static object[] Mapear(Type tipoFachada, Servico servico, string tag) =>
            new object[] { tipoFachada, servico, tag };

        private static object[] Obsoleta(string nomeClasse, string fachada) =>
            new object[] { nomeClasse, fachada };

        private static Type ObterTipoServico(string nomeClasse) =>
            typeof(GerarNfse).Assembly.GetType($"{NamespaceServicosNFSe}.{nomeClasse}", true);

        private static string ObterNomeTagServico(Unimake.Business.DFe.Servicos.ServicoBase servico)
        {
            var metodo = servico.GetType().GetMethod("ObterNomeTagServico", BindingFlags.Instance | BindingFlags.NonPublic);
            Assert.NotNull(metodo);

            return (string)metodo.Invoke(servico, null);
        }

        private static void DefinirConfiguracao(Unimake.Business.DFe.Servicos.ServicoBase servico)
        {
            var metodo = servico.GetType().GetMethod("DefinirConfiguracao", BindingFlags.Instance | BindingFlags.NonPublic);
            Assert.NotNull(metodo);
            metodo.Invoke(servico, null);
        }

        private static void DefinirConteudoXML(Unimake.Business.DFe.Servicos.ServicoBase servico, string xml)
        {
            var propriedade = typeof(Unimake.Business.DFe.Servicos.ServicoBase)
                .GetProperty("ConteudoXML", BindingFlags.Instance | BindingFlags.NonPublic);
            var documento = new XmlDocument();
            documento.LoadXml(xml);

            Assert.NotNull(propriedade);
            propriedade.SetValue(servico, documento);
        }

        private static HashSet<string> CarregarTagsConfiguradas(Assembly assembly)
        {
            var tags = new HashSet<string>(StringComparer.Ordinal);
            var recursos = assembly
                .GetManifestResourceNames()
                .Where(nome => nome.Contains(".Servicos.Config.NFSe.") && nome.EndsWith(".xml", StringComparison.OrdinalIgnoreCase));

            foreach (var recurso in recursos)
            {
                using (var stream = assembly.GetManifestResourceStream(recurso))
                {
                    var documento = new XmlDocument();
                    documento.Load(stream);

                    foreach (XmlNode node in documento.SelectNodes("//Servicos/*"))
                    {
                        tags.Add(node.Name);
                    }
                }
            }

            return tags;
        }
    }
}
