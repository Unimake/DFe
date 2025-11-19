using System;
using System.Collections.Generic;
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
        /// <summary>
        /// Monta os parâmetros para o cenário de testes
        /// </summary>
        public static IEnumerable<object[]> Parametros => TestUtility.PreparaDadosCenario("ConsultarConvenioMunicipal");

        #region Testes de Convênio Municipal

        /// <summary>
        /// Consultar convênio municipal para saber se a conexão com o webservice está ocorrendo corretamente
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [MemberData(nameof(Parametros))]
        public void ConsultarConvenioMunicipal(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codMunicipio)
        {
            if (padraoNFSe != PadraoNFSe.NACIONAL)
                return;

            if (codMunicipio != 1001058)
                return;

            var nomeXMLEnvio = "ConsultarConvenioMunicipalEnvio-ped-convenio.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\" + versaoSchema + "\\" + nomeXMLEnvio;

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
                SchemaVersao = versaoSchema
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
        [MemberData(nameof(Parametros))]
        public void ConsultarAliquotasMunicipais(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codMunicipio)
        {
            if (padraoNFSe != PadraoNFSe.NACIONAL)
                return;

            if (codMunicipio != 1001058)
                return;

            var nomeXMLEnvio = "ConsultarAliquotasMunicipaisEnvio-ped-aliquotas.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\" + versaoSchema + "\\" + nomeXMLEnvio;

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
                SchemaVersao = versaoSchema
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
        [MemberData(nameof(Parametros))]
        public void ConsultarHistoricoAliquotasMunicipais(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codMunicipio)
        {
            if (padraoNFSe != PadraoNFSe.NACIONAL)
                return;

            if (codMunicipio != 1001058)
                return;

            var nomeXMLEnvio = "ConsultarHistoricoAliquotasMunicipaisEnvio-ped-historico.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\" + versaoSchema + "\\" + nomeXMLEnvio;

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
                SchemaVersao = versaoSchema
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
        [MemberData(nameof(Parametros))]
        public void ConsultarRegimesEspeciaisMunicipais(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codMunicipio)
        {
            if (padraoNFSe != PadraoNFSe.NACIONAL)
                return;

            if (codMunicipio != 1001058)
                return;

            var nomeXMLEnvio = "ConsultarRegimesEspeciaisMunicipaisEnvio-ped-regimes.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\" + versaoSchema + "\\" + nomeXMLEnvio;

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
                SchemaVersao = versaoSchema
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
        [MemberData(nameof(Parametros))]
        public void ConsultarRetencoesMunicipais(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codMunicipio)
        {
            if (padraoNFSe != PadraoNFSe.NACIONAL)
                return;

            if (codMunicipio != 1001058)
                return;

            var nomeXMLEnvio = "ConsultarRetencoesMunicipaisEnvio-ped-retencoes.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\" + versaoSchema + "\\" + nomeXMLEnvio;

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
                SchemaVersao = versaoSchema
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
        [MemberData(nameof(Parametros))]
        public void ConsultarBeneficioMunicipal(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codMunicipio)
        {
            if (padraoNFSe != PadraoNFSe.NACIONAL)
                return;

            if (codMunicipio != 1001058)
                return;

            var nomeXMLEnvio = "ConsultarBeneficioMunicipalEnvio-ped-beneficio.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\" + versaoSchema + "\\" + nomeXMLEnvio;

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
                SchemaVersao = versaoSchema
            };

            var consultarBeneficio = new ConsultarBeneficioMunicipal(conteudoXML, configuracao);
            Assert.Multiple(() => TestUtility.AnalisaResultado(consultarBeneficio));
        }

        /// <summary>
        /// Teste com construtor direto para benefício municipal
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [MemberData(nameof(Parametros))]
        public void ConsultarBeneficioMunicipalDireto(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codMunicipio)
        {
            if (padraoNFSe != PadraoNFSe.NACIONAL)
                return;

            if (codMunicipio != 1001058)
                return;

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeConsultarBeneficioMunicipal,
                SchemaVersao = versaoSchema
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
                SchemaVersao = "1.00"
            };

            var xmlSemMunicipio = new XmlDocument();
            xmlSemMunicipio.LoadXml(@"<?xml version=""1.0"" encoding=""utf-8""?>
<ConsultaParametros versao=""1.00"" xmlns=""http://www.sped.fazenda.gov.br/nfse"">
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
                SchemaVersao = "1.00"
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