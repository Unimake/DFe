using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe
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
        [InlineData(TipoAmbiente.Homologacao, PadraoNFSe.NACIONAL)]
        [InlineData(TipoAmbiente.Producao, PadraoNFSe.NACIONAL)]
        public void ConsultarConvenioMunicipal(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe)
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
            Assert.Multiple(() => TestUtility.AnalisaResultado(consultarConvenio));
        }

        #endregion

        #region Testes de Alíquotas

        /// <summary>
        /// Consultar alíquotas municipais
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData(TipoAmbiente.Homologacao, PadraoNFSe.NACIONAL)]
        [InlineData(TipoAmbiente.Producao, PadraoNFSe.NACIONAL)]
        public void ConsultarAliquotasMunicipais(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe)
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
            Assert.Multiple(() => TestUtility.AnalisaResultado(consultarAliquotas));
        }

        #endregion

        #region Testes de Histórico de Alíquotas

        /// <summary>
        /// Consultar histórico de alíquotas municipais
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData(TipoAmbiente.Homologacao, PadraoNFSe.NACIONAL)]
        [InlineData(TipoAmbiente.Producao, PadraoNFSe.NACIONAL)]
        public void ConsultarHistoricoAliquotasMunicipais(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe)
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
            Assert.Multiple(() => TestUtility.AnalisaResultado(consultarHistorico));
        }

        #endregion

        #region Testes de Regimes Especiais

        /// <summary>
        /// Consultar regimes especiais municipais
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData(TipoAmbiente.Homologacao, PadraoNFSe.NACIONAL)]
        [InlineData(TipoAmbiente.Producao, PadraoNFSe.NACIONAL)]
        public void ConsultarRegimesEspeciaisMunicipais(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe)
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
            Assert.Multiple(() => TestUtility.AnalisaResultado(consultarRegimes));
        }

        #endregion

        #region Testes de Retenções

        /// <summary>
        /// Consultar retenções municipais
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData(TipoAmbiente.Homologacao, PadraoNFSe.NACIONAL)]
        [InlineData(TipoAmbiente.Producao, PadraoNFSe.NACIONAL)]
        public void ConsultarRetencoesMunicipais(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe)
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
            Assert.Multiple(() => TestUtility.AnalisaResultado(consultarRetencoes));
        }

        #endregion

        #region Testes de Benefício Municipal

        /// <summary>
        /// Consultar benefício municipal
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData(TipoAmbiente.Homologacao, PadraoNFSe.NACIONAL)]
        [InlineData(TipoAmbiente.Producao, PadraoNFSe.NACIONAL)]
        public void ConsultarBeneficioMunicipal(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe)
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
            Assert.Multiple(() => TestUtility.AnalisaResultado(consultarBeneficio));
        }

        /// <summary>
        /// Teste com construtor direto para benefício municipal
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData(TipoAmbiente.Homologacao, PadraoNFSe.NACIONAL)]
        [InlineData(TipoAmbiente.Producao, PadraoNFSe.NACIONAL)]
        public void ConsultarBeneficioMunicipalDireto(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe)
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

            var consultarBeneficio = new ConsultarBeneficioMunicipal(4204608, "99.99.99.999", DateTime.Now, configuracao);
            Assert.Multiple(() => TestUtility.AnalisaResultado(consultarBeneficio));
        }

        #endregion

        #region Testes de Validação

        /// <summary>
        /// Teste de validação - parâmetros obrigatórios não informados
        /// </summary>
        [Fact]
        [Trait("DFe", "NFSe")]
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

            Assert.Throws<ArgumentException>(() =>
            {
                var consulta = new ConsultarConvenioMunicipal(xmlSemMunicipio, configuracao);
                consulta.Executar();
            });
        }

        /// <summary>
        /// Teste de validação de URLs construídas
        /// </summary>
        [Fact]
        [Trait("DFe", "NFSe")]
        public void ValidarConstrucaoURLs()
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CodigoMunicipio = 1001058,
                SchemaVersao = "1.01"
            };

            // Teste Convênio
            var consultaConvenio = new ConsultarConvenioMunicipal(4106902, configuracao);
            Assert.Equal(4106902, consultaConvenio.CodigoMunicipio);

            // Teste Alíquotas
            var consultaAliquotas = new ConsultarAliquotasMunicipais(4106902, "01.01", DateTime.Now, configuracao);
            Assert.Equal(4106902, consultaAliquotas.CodigoMunicipio);
            Assert.Equal("01.01", consultaAliquotas.CodigoServico);
            Assert.True(consultaAliquotas.Competencia != default(DateTime));

            // Teste Benefício Municipal
            var consultaBeneficio = new ConsultarBeneficioMunicipal(4106902, "123456", DateTime.Now, configuracao);
            Assert.Equal(4106902, consultaBeneficio.CodigoMunicipio);
            Assert.Equal("123456", consultaBeneficio.NumeroBeneficio);
        }

        #endregion
    }
}