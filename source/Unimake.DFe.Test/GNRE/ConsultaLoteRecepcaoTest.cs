using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.GNRE;
using Unimake.Business.DFe.Xml.GNRE;
using Xunit;

namespace Unimake.DFe.Test.GNRE
{
    /// <summary>
    /// Consultar uma chave do MDFe somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
    /// Efetua uma consulta por estado + ambiente para garantir que todos estão funcionando.
    /// </summary>
    public class ConsultaLoteRecepcaoGNRETest
    {
        [Theory]
        [Trait("DFe", "GNRE")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ConsultarLoteRecepcaoGNRE(TipoAmbiente tipoAmbiente)
        {

            var xml = new TLoteConsultaGNRE
            {
                Versao = "2.00",
                Consulta = new List<Consulta>
                    {
                        new Consulta
                        {
                            Uf = UFBrasil.PR,
                            EmitenteId = new EmitenteId
                            {
                                CNPJ = "07638784000127",
                            },
                            CodBarras = "12345678911234567891123456789112345678914444",
                            NumControle = "1234567891123456",
                            DocOrigem = new DocOrigem
                            {
                                Tipo = "10",
                                Value = "12"
                            },
                            IdConsulta = "10",
                            TipoConsulta = TipoConsultaGNRE.ConsultaPorCodigoBarra
                        }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.GNRE,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var consultaLoteRecepcao = new ConsultaLoteRecepcao(xml, configuracao);
            consultaLoteRecepcao.Executar();

            Assert.True(consultaLoteRecepcao.Result != null);
            Assert.True(consultaLoteRecepcao.Result.Ambiente.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(consultaLoteRecepcao.Result.SituacaoRecepcao.Codigo.Equals("100") || consultaLoteRecepcao.Result.SituacaoRecepcao.Codigo.Equals("102"), "Código de retorno diferente de 100 e 102.");  //Lote de consulta de guia recebido com sucesso!
        }

        /// <summary>
        /// Teste do construtor que utiliza código de barras para consulta de lote da GNRE
        /// </summary>
        [Theory]
        [Trait("DFe", "GNRE")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaLoteRecepcaoPorCodigoBarras(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.GNRE,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var consultaLoteRecepcao = new ConsultaLoteRecepcao(
                UFBrasil.PR,
                "07638784000127",
                "12345678911234567891123456789112345678914444",
                configuracao);

            consultaLoteRecepcao.Executar();

            Assert.NotNull(consultaLoteRecepcao.Result);
            Assert.Equal(tipoAmbiente, consultaLoteRecepcao.Result.Ambiente);
            Assert.True(
                consultaLoteRecepcao.Result.SituacaoRecepcao.Codigo.Equals("100") ||
                consultaLoteRecepcao.Result.SituacaoRecepcao.Codigo.Equals("102"),
                "Código de retorno diferente de 100 e 102.");
        }

        /// <summary>
        /// Teste do construtor que utiliza número de controle para consulta de lote da GNRE
        /// </summary>
        [Theory]
        [Trait("DFe", "GNRE")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaLoteRecepcaoPorNumeroControle(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.GNRE,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var consultaLoteRecepcao = new ConsultaLoteRecepcao(
                UFBrasil.PR,
                "07638784000127",
                "1234567891123456",
                TipoConsultaGNRE.ConsultaPorNumeroControleGNRE,
                configuracao);

            consultaLoteRecepcao.Executar();

            Assert.NotNull(consultaLoteRecepcao.Result);
            Assert.Equal(tipoAmbiente, consultaLoteRecepcao.Result.Ambiente);
            Assert.True(
                consultaLoteRecepcao.Result.SituacaoRecepcao.Codigo.Equals("100") ||
                consultaLoteRecepcao.Result.SituacaoRecepcao.Codigo.Equals("102"),
                "Código de retorno diferente de 100 e 102.");
        }

        /// <summary>
        /// Teste do construtor completo para consulta de lote da GNRE
        /// </summary>
        [Theory]
        [Trait("DFe", "GNRE")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaLoteRecepcaoCompleta(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.GNRE,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var consultaLoteRecepcao = new ConsultaLoteRecepcao(
                UFBrasil.PR,
                "07638784000127",
                "12345678911234567891123456789112345678914444",
                "1234567891123456",
                "10", // tipo documento origem
                "12", // valor documento origem
                "10", // ID consulta
                TipoConsultaGNRE.ConsultaPorCodigoBarra,
                configuracao);

            consultaLoteRecepcao.Executar();

            Assert.NotNull(consultaLoteRecepcao.Result);
            Assert.Equal(tipoAmbiente, consultaLoteRecepcao.Result.Ambiente);
            Assert.True(
                consultaLoteRecepcao.Result.SituacaoRecepcao.Codigo.Equals("100") ||
                consultaLoteRecepcao.Result.SituacaoRecepcao.Codigo.Equals("102"),
                "Código de retorno diferente de 100 e 102.");
        }
    }
}

