using System;
using System.IO;
using System.Net;
using System.Reflection;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Unimake.DFe.Test.NFSe.Utilitarios;
using Xunit;

namespace Unimake.DFe.Test.NFSe.Servicos
{
    /// <summary>
    /// Testar os serviços de consulta de parâmetros municipais NFSe NACIONAL
    /// </summary>
    public class ConsultaParametrosMunicipaisTest
    {
        #region Testes de Convênio Municipal

        /// <summary>
        /// Consultar convênio municipal para saber se a conexão com o webservice está ocorrendo corretamente
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Categoria", "Integracao")]
        [InlineData(TipoAmbiente.Homologacao, PadraoNFSe.NACIONAL, HttpStatusCode.OK)]
        [InlineData(TipoAmbiente.Producao, PadraoNFSe.NACIONAL, HttpStatusCode.OK)]
        public void ConsultarConvenioMunicipal(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, HttpStatusCode statusEsperado)
        {
            if (padraoNFSe != PadraoNFSe.NACIONAL)
                return;

            var nomeXMLEnvio = "ConsultarConvenioMunicipalEnvio-ped-convenio.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\1.01\\" + nomeXMLEnvio;

            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeConsultarConvenioMunicipal,
                SchemaVersao = "1.01"
            };

            var consultarConvenio = new ConsultarConvenioMunicipal(conteudoXML, configuracao);
            ExecutarEValidarStatus(consultarConvenio, statusEsperado);
        }

        #endregion

        #region Testes de Alíquotas

        /// <summary>
        /// Consultar alíquotas municipais
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Categoria", "Integracao")]
        [InlineData(TipoAmbiente.Homologacao, PadraoNFSe.NACIONAL, HttpStatusCode.NotFound)]
        public void ConsultarAliquotasMunicipais(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, HttpStatusCode statusEsperado)
        {
            if (padraoNFSe != PadraoNFSe.NACIONAL)
                return;

            var nomeXMLEnvio = "ConsultarAliquotasMunicipaisEnvio-ped-aliquotas.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\1.01\\" + nomeXMLEnvio;

            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeConsultarAliquotasMunicipais,
                SchemaVersao = "1.01"
            };

            var consultarAliquotas = new ConsultarAliquotasMunicipais(conteudoXML, configuracao);
            ExecutarEValidarStatus(consultarAliquotas, statusEsperado);
        }

        #endregion

        #region Testes de Histórico de Alíquotas

        /// <summary>
        /// Consultar histórico de alíquotas municipais
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Categoria", "Integracao")]
        [InlineData(TipoAmbiente.Homologacao, PadraoNFSe.NACIONAL, HttpStatusCode.NotFound)]
        public void ConsultarHistoricoAliquotasMunicipais(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, HttpStatusCode statusEsperado)
        {
            if (padraoNFSe != PadraoNFSe.NACIONAL)
                return;

            var nomeXMLEnvio = "ConsultarHistoricoAliquotasMunicipaisEnvio-ped-historico.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\1.01\\" + nomeXMLEnvio;

            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeConsultarHistoricoAliquotasMunicipais,
                SchemaVersao = "1.01"
            };

            var consultarHistorico = new ConsultarHistoricoAliquotasMunicipais(conteudoXML, configuracao);
            ExecutarEValidarStatus(consultarHistorico, statusEsperado);
        }

        #endregion

        #region Testes de Regimes Especiais

        /// <summary>
        /// Consultar regimes especiais municipais
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Categoria", "Integracao")]
        [InlineData(TipoAmbiente.Homologacao, PadraoNFSe.NACIONAL, HttpStatusCode.NotFound)]
        public void ConsultarRegimesEspeciaisMunicipais(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, HttpStatusCode statusEsperado)
        {
            if (padraoNFSe != PadraoNFSe.NACIONAL)
                return;

            var nomeXMLEnvio = "ConsultarRegimesEspeciaisMunicipaisEnvio-ped-regimes.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\1.01\\" + nomeXMLEnvio;

            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeConsultarRegimesEspeciaisMunicipais,
                SchemaVersao = "1.01"
            };

            var consultarRegimes = new ConsultarRegimesEspeciaisMunicipais(conteudoXML, configuracao);
            ExecutarEValidarStatus(consultarRegimes, statusEsperado);
        }

        #endregion

        #region Testes de Retenções

        /// <summary>
        /// Consultar retenções municipais
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Categoria", "Integracao")]
        [InlineData(TipoAmbiente.Homologacao, PadraoNFSe.NACIONAL, HttpStatusCode.NotFound)]
        public void ConsultarRetencoesMunicipais(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, HttpStatusCode statusEsperado)
        {
            if (padraoNFSe != PadraoNFSe.NACIONAL)
                return;

            var nomeXMLEnvio = "ConsultarRetencoesMunicipaisEnvio-ped-retencoes.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\1.01\\" + nomeXMLEnvio;

            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeConsultarRetencoesMunicipais,
                SchemaVersao = "1.01"
            };

            var consultarRetencoes = new ConsultarRetencoesMunicipais(conteudoXML, configuracao);
            ExecutarEValidarStatus(consultarRetencoes, statusEsperado);
        }

        #endregion

        #region Testes de Benefício Municipal

        /// <summary>
        /// Consultar benefício municipal
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Categoria", "Integracao")]
        [InlineData(TipoAmbiente.Homologacao, PadraoNFSe.NACIONAL, HttpStatusCode.BadRequest)]
        public void ConsultarBeneficioMunicipal(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, HttpStatusCode statusEsperado)
        {
            if (padraoNFSe != PadraoNFSe.NACIONAL)
                return;

            var nomeXMLEnvio = "ConsultarBeneficioMunicipalEnvio-ped-beneficio.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\1.01\\" + nomeXMLEnvio;

            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeConsultarBeneficioMunicipal,
                SchemaVersao = "1.01"
            };

            var consultarBeneficio = new ConsultarBeneficioMunicipal(conteudoXML, configuracao);
            ExecutarEValidarStatus(consultarBeneficio, statusEsperado);
        }

        /// <summary>
        /// Teste com construtor direto para benefício municipal
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Categoria", "Integracao")]
        [InlineData(TipoAmbiente.Homologacao, PadraoNFSe.NACIONAL, HttpStatusCode.BadRequest)]
        public void ConsultarBeneficioMunicipalDireto(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, HttpStatusCode statusEsperado)
        {
            if (padraoNFSe != PadraoNFSe.NACIONAL)
                return;

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeConsultarBeneficioMunicipal,
                SchemaVersao = "1.01"
            };

            var consultarBeneficio = new ConsultarBeneficioMunicipal(4204608, "1234.56.78.901", new DateTime(2025, 11, 15), configuracao);
            ExecutarEValidarStatus(consultarBeneficio, statusEsperado);
        }

        #endregion

        #region Testes de Configuração

        /// <summary>
        /// Confirma a configuração determinística dos serviços GET do ADN que exigem mTLS.
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Categoria", "Unidade")]
        [InlineData("ConsultarConvenioMunicipal", "https://adn.nfse.gov.br/parametrizacao/{codigoMunicipio}/convenio", "https://adn.producaorestrita.nfse.gov.br/parametrizacao/{codigoMunicipio}/convenio")]
        [InlineData("ConsultarAliquotasMunicipais", "https://adn.nfse.gov.br/parametrizacao/{codigoMunicipio}/{codigoServico}/{competencia}/aliquota", "https://adn.producaorestrita.nfse.gov.br/parametrizacao/{codigoMunicipio}/{codigoServico}/{competencia}/aliquota")]
        [InlineData("ConsultarHistoricoAliquotasMunicipais", "https://adn.nfse.gov.br/parametrizacao/{codigoMunicipio}/{codigoServico}/historicoaliquotas", "https://adn.producaorestrita.nfse.gov.br/parametrizacao/{codigoMunicipio}/{codigoServico}/historicoaliquotas")]
        [InlineData("ConsultarRegimesEspeciaisMunicipais", "https://adn.nfse.gov.br/parametrizacao/{codigoMunicipio}/{codigoServico}/{competencia}/regimes_especiais", "https://adn.producaorestrita.nfse.gov.br/parametrizacao/{codigoMunicipio}/{codigoServico}/{competencia}/regimes_especiais")]
        [InlineData("ConsultarRetencoesMunicipais", "https://adn.nfse.gov.br/parametrizacao/{codigoMunicipio}/{competencia}/retencoes", "https://adn.producaorestrita.nfse.gov.br/parametrizacao/{codigoMunicipio}/{competencia}/retencoes")]
        [InlineData("ConsultarBeneficioMunicipal", "https://adn.nfse.gov.br/parametrizacao/{codigoMunicipio}/{numeroBeneficio}/{competencia}/beneficio", "https://adn.producaorestrita.nfse.gov.br/parametrizacao/{codigoMunicipio}/{numeroBeneficio}/{competencia}/beneficio")]
        [InlineData("ConsultarDistribuicaoNFSeNSU", "https://adn.nfse.gov.br/contribuintes/DFe/{NSU}", "https://adn.producaorestrita.nfse.gov.br/contribuintes/DFe/{NSU}")]
        [InlineData("ConsultaEventosNFSeChaveAcesso", "https://adn.nfse.gov.br/contribuintes/NFSe/{ChaveNFSe}/Eventos", "https://adn.producaorestrita.nfse.gov.br/contribuintes/NFSe/{ChaveNFSe}/Eventos")]
        public void ConfiguracaoNacionalDeveExigirCertificadoDigital(string nomeServico, string endpointProducao, string endpointHomologacao)
        {
            var configuracao = CriarConfiguracao(TipoAmbiente.Homologacao);

            configuracao.Load(nomeServico);

            Assert.Multiple(
                () => Assert.True(configuracao.UsaCertificadoDigital),
                () => Assert.Equal("GET", configuracao.MetodoAPI.ToUpperInvariant()),
                () => Assert.Equal(endpointProducao, configuracao.RequestURIProducao),
                () => Assert.Equal(endpointHomologacao, configuracao.RequestURIHomologacao));
        }

        #endregion

        #region Testes de Validação

        /// <summary>
        /// Teste de validação - parâmetros obrigatórios não informados
        /// </summary>
        [Fact]
        [Trait("DFe", "NFSe")]
        [Trait("Categoria", "Unidade")]
        public void ValidarParametrosObrigatorios()
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = TipoAmbiente.Homologacao,
                Servico = Servico.NFSeConsultarConvenioMunicipal,
                SchemaVersao = "1.01"
            };

            var xmlSemMunicipio = new XmlDocument();
            xmlSemMunicipio.LoadXml(@"<?xml version=""1.0"" encoding=""utf-8""?>
<ConsultaParametros versao=""1.01"" xmlns=""http://www.sped.fazenda.gov.br/nfse"">
    <infConsulta>
        <tipoParametro>convenio</tipoParametro>
    </infConsulta>
</ConsultaParametros>");

            var exception = Assert.Throws<InvalidOperationException>(() => new ConsultarConvenioMunicipal(xmlSemMunicipio, configuracao));

            Assert.Equal("O código do município deve ser informado.", exception.Message);
        }

        /// <summary>
        /// Teste de validação de URLs construídas
        /// </summary>
        [Fact]
        [Trait("DFe", "NFSe")]
        [Trait("Categoria", "Unidade")]
        public void ValidarConstrucaoURLs()
        {
            var competencia = new DateTime(2025, 11, 15);

            ValidarRequestURI(
                new ConsultarConvenioMunicipal(4106902, CriarConfiguracao(TipoAmbiente.Homologacao)),
                "https://adn.producaorestrita.nfse.gov.br/parametrizacao/4106902/convenio");
            ValidarRequestURI(
                new ConsultarAliquotasMunicipais(4106902, "01.01", competencia, CriarConfiguracao(TipoAmbiente.Homologacao)),
                "https://adn.producaorestrita.nfse.gov.br/parametrizacao/4106902/01.01/2025-11-15T00:00:00/aliquota");
            ValidarRequestURI(
                new ConsultarHistoricoAliquotasMunicipais(4106902, "01.01", CriarConfiguracao(TipoAmbiente.Homologacao)),
                "https://adn.producaorestrita.nfse.gov.br/parametrizacao/4106902/01.01/historicoaliquotas");
            ValidarRequestURI(
                new ConsultarRegimesEspeciaisMunicipais(4106902, "01.01", competencia, CriarConfiguracao(TipoAmbiente.Homologacao)),
                "https://adn.producaorestrita.nfse.gov.br/parametrizacao/4106902/01.01/2025-11-15T00:00:00/regimes_especiais");
            ValidarRequestURI(
                new ConsultarRetencoesMunicipais(4106902, competencia, CriarConfiguracao(TipoAmbiente.Homologacao)),
                "https://adn.producaorestrita.nfse.gov.br/parametrizacao/4106902/2025-11-15T00:00:00/retencoes");
            ValidarRequestURI(
                new ConsultarBeneficioMunicipal(4106902, "123456", competencia, CriarConfiguracao(TipoAmbiente.Homologacao)),
                "https://adn.producaorestrita.nfse.gov.br/parametrizacao/4106902/123456/2025-11-15T00:00:00/beneficio");
        }

        #endregion

        private static Configuracao CriarConfiguracao(TipoAmbiente tipoAmbiente)
        {
            return new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                CodigoMunicipio = 1001058,
                SchemaVersao = "1.01"
            };
        }

        private static void ExecutarEValidarStatus(Unimake.Business.DFe.Servicos.ServicoBase servico, HttpStatusCode statusEsperado)
        {
            try
            {
                servico.Executar();
            }
            catch
            {
                if (servico.HttpStatusCode == 0 || servico.HttpStatusCode != statusEsperado)
                {
                    throw;
                }
            }

            Assert.NotEqual((HttpStatusCode)0, servico.HttpStatusCode);
            Assert.Equal(statusEsperado, servico.HttpStatusCode);
        }

        private static void ValidarRequestURI(Unimake.Business.DFe.Servicos.ServicoBase servico, string requestURIEsperada)
        {
            var definirConfiguracao = servico.GetType().GetMethod("DefinirConfiguracao", BindingFlags.Instance | BindingFlags.NonPublic);

            Assert.NotNull(definirConfiguracao);
            definirConfiguracao.Invoke(servico, null);

            Assert.Equal(requestURIEsperada, servico.Configuracoes.RequestURI);
        }
    }
}
