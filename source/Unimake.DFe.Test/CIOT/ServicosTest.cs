using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.CIOT;
using Xunit;
using CIOTCancelamentoOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.CancelamentoOperacaoTransporte;
using CIOTConsultarCIOTGerado = Unimake.Business.DFe.Servicos.CIOT.ConsultarCIOTGerado;
using CIOTConsultarExcecao = Unimake.Business.DFe.Servicos.CIOT.ConsultarExcecao;
using CIOTConsultarFrotaTransportador = Unimake.Business.DFe.Servicos.CIOT.ConsultarFrotaTransportador;
using CIOTConsultarSituacaoTransportador = Unimake.Business.DFe.Servicos.CIOT.ConsultarSituacaoTransportador;
using CIOTDeclaracaoOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.DeclaracaoOperacaoTransporte;
using CIOTEncerramentoOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.EncerramentoOperacaoTransporte;
using CIOTRetificacaoOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.RetificacaoOperacaoTransporte;

namespace Unimake.DFe.Test.CIOT
{
    /// <summary>
    /// Testar os serviços do CIOT
    /// </summary>
    public class ServicosTest
    {
        /// <summary>
        /// Consultar situação do transportador.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "ConsultarSituacaoTransportador")]
        [InlineData(@"..\..\..\CIOT\Resources\consultarSituacaoTransportador.xml")]
        public void ConsultarSituacaoTransportador(string arqXML)
        {
            var objeto = LerXML<ConsultarSituacaoTransportador>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTConsultarSituacaoTransportador(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetConsultarSituacaoTransportador>(servico.Result);
        }

        /// <summary>
        /// Consultar frota do transportador.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "ConsultarFrotaTransportador")]
        [InlineData(@"..\..\..\CIOT\Resources\consultarFrotaTransportador.xml")]
        public void ConsultarFrotaTransportador(string arqXML)
        {
            var objeto = LerXML<ConsultarFrotaTransportador>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTConsultarFrotaTransportador(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetConsultarFrotaTransportador>(servico.Result);
        }

        /// <summary>
        /// Declarar operação de transporte.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "DeclaracaoOperacaoTransporte")]
        [InlineData(@"..\..\..\CIOT\Resources\declaracaoOperacaoTransporte.xml")]
        public void DeclaracaoOperacaoTransporte(string arqXML)
        {
            var objeto = LerXML<DeclaracaoOperacaoTransporte>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTDeclaracaoOperacaoTransporte(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetDeclaracaoOperacaoTransporte>(servico.Result);
        }

        /// <summary>
        /// Cancelar operação de transporte.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "CancelamentoOperacaoTransporte")]
        [InlineData(@"..\..\..\CIOT\Resources\cancelamentoOperacaoTransporte.xml")]
        public void CancelamentoOperacaoTransporte(string arqXML)
        {
            var objeto = LerXML<CancelamentoOperacaoTransporte>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTCancelamentoOperacaoTransporte(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetCancelamentoOperacaoTransporte>(servico.Result);
        }

        /// <summary>
        /// Retificar operação de transporte.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "RetificacaoOperacaoTransporte")]
        [InlineData(@"..\..\..\CIOT\Resources\retificacaoOperacaoTransporte.xml")]
        public void RetificacaoOperacaoTransporte(string arqXML)
        {
            var objeto = LerXML<RetificacaoOperacaoTransporte>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTRetificacaoOperacaoTransporte(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetRetificacaoOperacaoTransporte>(servico.Result);
        }

        /// <summary>
        /// Encerrar operação de transporte.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "EncerramentoOperacaoTransporte")]
        [InlineData(@"..\..\..\CIOT\Resources\encerramentoOperacaoTransporte.xml")]
        public void EncerramentoOperacaoTransporte(string arqXML)
        {
            var objeto = LerXML<EncerramentoOperacaoTransporte>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTEncerramentoOperacaoTransporte(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetEncerramentoOperacaoTransporte>(servico.Result);
        }

        /// <summary>
        /// Consultar exceção do transportador.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "ConsultarExcecao")]
        [InlineData(@"..\..\..\CIOT\Resources\consultarExcecao.xml")]
        public void ConsultarExcecao(string arqXML)
        {
            var objeto = LerXML<ConsultarExcecao>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTConsultarExcecao(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetConsultarExcecao>(servico.Result);
        }

        /// <summary>
        /// Consultar CIOT gerado.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "ConsultarCIOTGerado")]
        [InlineData(@"..\..\..\CIOT\Resources\consultarCIOTGerado.xml")]
        public void ConsultarCIOTGerado(string arqXML)
        {
            var objeto = LerXML<ConsultarCIOTGerado>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTConsultarCIOTGerado(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetConsultarCIOTGerado>(servico.Result);
        }

        /// <summary>
        /// Desserializar retorno de erro da API dentro do retorno tipado.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "CancelamentoOperacaoTransporte")]
        public void RetornoErroCancelamentoOperacaoTransporte()
        {
            var xml = new XmlDocument();
            xml.LoadXml("<temp><error>USUARIO_NAO_AUTORIZADO</error><message>Rejeição: O CPF/CNPJ do certificado digital não corresponde a nenhum transportador cadastrado no RNTRC</message><timestamp>2026-05-26T20:34:17.2862302Z</timestamp><correlationId>ee174fab-71e7-4c1f-97e8-3029216bf457</correlationId><path>/pefServices/api/CancelamentoOperacaoTransporte</path></temp>");

            var servico = new CIOTCancelamentoOperacaoTransporte
            {
                RetornoWSXML = xml
            };

            Assert.Equal("USUARIO_NAO_AUTORIZADO", servico.Result.Temp.Error);
            Assert.Equal("Rejeição: O CPF/CNPJ do certificado digital não corresponde a nenhum transportador cadastrado no RNTRC", servico.Result.Temp.Message);
            Assert.Equal("2026-05-26T20:34:17.2862302Z", servico.Result.Temp.Timestamp);
            Assert.Equal("ee174fab-71e7-4c1f-97e8-3029216bf457", servico.Result.Temp.CorrelationId);
            Assert.Equal("/pefServices/api/CancelamentoOperacaoTransporte", servico.Result.Temp.Path);
            Assert.Equal("RetCancelamentoOperacaoTransporte", servico.RetornoWSXML.DocumentElement.Name);
            Assert.Contains("<temp>", servico.RetornoWSString);
        }

        /// <summary>
        /// Desserializar retorno OK na propriedade Result dos serviços.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        public void ResultRetornoOKServicosCIOT()
        {
            ValidarResultOK(new CIOTConsultarSituacaoTransportador(), @"..\..\..\CIOT\Resources\retConsultarSituacaoTransportador.xml");
            ValidarResultOK(new CIOTConsultarFrotaTransportador(), @"..\..\..\CIOT\Resources\retConsultarFrotaTransportador.xml");
            ValidarResultOK(new CIOTDeclaracaoOperacaoTransporte(), @"..\..\..\CIOT\Resources\retDeclaracaoOperacaoTransporte.xml");
            ValidarResultOK(new CIOTCancelamentoOperacaoTransporte(), @"..\..\..\CIOT\Resources\retCancelamentoOperacaoTransporte.xml");
            ValidarResultOK(new CIOTRetificacaoOperacaoTransporte(), @"..\..\..\CIOT\Resources\retRetificacaoOperacaoTransporte.xml");
            ValidarResultOK(new CIOTEncerramentoOperacaoTransporte(), @"..\..\..\CIOT\Resources\retEncerramentoOperacaoTransporte.xml");
            ValidarResultOK(new CIOTConsultarExcecao(), @"..\..\..\CIOT\Resources\retConsultarExcecao.xml");
            ValidarResultOK(new CIOTConsultarCIOTGerado(), @"..\..\..\CIOT\Resources\retConsultarCIOTGerado.xml");
        }

        /// <summary>
        /// Desserializar retorno de erro na propriedade Result dos serviços.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        public void ResultRetornoErroServicosCIOT()
        {
            ValidarResultErro(new CIOTConsultarSituacaoTransportador(), "/pefServices/api/ConsultarSituacaoTransportador");
            ValidarResultErro(new CIOTConsultarFrotaTransportador(), "/pefServices/api/ConsultarFrotaTransportador");
            ValidarResultErro(new CIOTDeclaracaoOperacaoTransporte(), "/pefServices/api/DeclaracaoOperacaoTransporte");
            ValidarResultErro(new CIOTCancelamentoOperacaoTransporte(), "/pefServices/api/CancelamentoOperacaoTransporte");
            ValidarResultErro(new CIOTRetificacaoOperacaoTransporte(), "/pefServices/api/RetificacaoOperacaoTransporte");
            ValidarResultErro(new CIOTEncerramentoOperacaoTransporte(), "/pefServices/api/EncerramentoOperacaoTransporte");
            ValidarResultErro(new CIOTConsultarExcecao(), "/pefServices/api/ConsultarExcecao");
            ValidarResultErro(new CIOTConsultarCIOTGerado(), "/pefServices/api/ConsultarCIOTGerado");
        }

        private static T LerXML<T>(string arqXML) where T : Unimake.Business.DFe.Xml.XMLBase, new()
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            return new T().LerXML<T>(xml);
        }

        private static void ValidarResultOK(dynamic servico, string arqXML)
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            servico.RetornoWSXML = xml;
            var result = servico.Result;

            Assert.NotNull(result);
            Assert.Null(result.Temp);
            Assert.Equal(xml.DocumentElement.Name, result.GerarXML().DocumentElement.Name);
            Assert.Equal(xml.InnerText, result.GerarXML().InnerText);
            Assert.Equal(xml.DocumentElement.Name, servico.RetornoWSXML.DocumentElement.Name);
            Assert.Equal(servico.RetornoWSXML.OuterXml, servico.RetornoWSString);
        }

        private static void ValidarResultErro(dynamic servico, string path)
        {
            var xml = new XmlDocument();
            xml.LoadXml("<temp><error>USUARIO_NAO_AUTORIZADO</error><message>Rejeição: O CPF/CNPJ do certificado digital não corresponde a nenhum transportador cadastrado no RNTRC</message><timestamp>2026-05-26T20:34:17.2862302Z</timestamp><correlationId>ee174fab-71e7-4c1f-97e8-3029216bf457</correlationId><path>" + path + "</path></temp>");

            servico.RetornoWSXML = xml;
            var result = servico.Result;

            Assert.NotNull(result);
            Assert.Equal("USUARIO_NAO_AUTORIZADO", result.Temp.Error);
            Assert.Equal("Rejeição: O CPF/CNPJ do certificado digital não corresponde a nenhum transportador cadastrado no RNTRC", result.Temp.Message);
            Assert.Equal("2026-05-26T20:34:17.2862302Z", result.Temp.Timestamp);
            Assert.Equal("ee174fab-71e7-4c1f-97e8-3029216bf457", result.Temp.CorrelationId);
            Assert.Equal(path, result.Temp.Path);
            Assert.Equal(result.GetType().Name, servico.RetornoWSXML.DocumentElement.Name);
            Assert.Equal(servico.RetornoWSXML.OuterXml, servico.RetornoWSString);
            Assert.Contains("<temp>", servico.RetornoWSString);
        }

        private static Configuracao CriarConfiguracao()
        {
            return new Configuracao
            {
                TipoDFe = TipoDFe.CIOT,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CodigoUF = (int)UFBrasil.AN,
                CertificadoDigital = PropConfig.CertificadoDigital
            };
        }
    }
}
