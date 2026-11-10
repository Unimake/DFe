using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.Utility.Rede
{
    [Collection("MemoriaDiagnosticoDisponibilidade")]
    public class ContratosOperacaoESocialReinfTest
    {
        private const string Envio = "http://www.esocial.gov.br/schema/lote/eventos/envio/retornoEnvio/v1_1_0";
        private const string Processamento = "http://www.esocial.gov.br/schema/lote/eventos/envio/retornoProcessamento/v1_3_0";
        private const string Identificadores = "http://www.esocial.gov.br/schema/consulta/identificadores-eventos/retorno/v1_0_0";
        private const string Download = "http://www.esocial.gov.br/schema/download/solicitacao/retorno/v1_0_0";
        private const string LoteReinf = "http://www.reinf.esocial.gov.br/schemas/retornoLoteEventosAssincrono/v1_00_00";
        private const string ReciboReinf = "http://www.reinf.esocial.gov.br/schemas/retornoRecibosChaveEvento/v1_05_01";

        // Contratos públicos/XSDs; consultas de identificadores e os dois downloads são aliases
        // de contratos compartilhados. Reinf recepção e consulta de lote também compartilham retorno.
        private static readonly Caso[] Casos =
        {
            new(TipoDFe.ESocial, Servico.ESocialEnviarLoteEventos, "EnviarLoteEventosESocial", Envio, "retornoEnvioLoteEventos"),
            new(TipoDFe.ESocial, Servico.ESocialConsultaEvts, "ConsultaLoteAssincrono", Processamento, "retornoProcessamentoLoteEventos"),
            new(TipoDFe.ESocial, Servico.ESocialConsultaEvts, "ConsultarEvtsEmpregador", Identificadores, "retornoConsultaIdentificadoresEvts"),
            new(TipoDFe.ESocial, Servico.ESocialConsultaEvts, "ConsultarEvtsTabela", Identificadores, "retornoConsultaIdentificadoresEvts"),
            new(TipoDFe.ESocial, Servico.ESocialConsultaEvts, "ConsultarEvtsTrabalhador", Identificadores, "retornoConsultaIdentificadoresEvts"),
            new(TipoDFe.ESocial, Servico.ESocialDownloadEvts, "DownloadPorID", Download, "download"),
            new(TipoDFe.ESocial, Servico.ESocialDownloadEvts, "DownloadPorNrRec", Download, "download"),
            new(TipoDFe.EFDReinf, Servico.EFDReinfRecepcionarLoteAssincrono, "RecepcionarLoteAssincrono", LoteReinf, "retornoLoteEventosAssincrono"),
            new(TipoDFe.EFDReinf, Servico.EFDReinfConsultaLoteAssincrono, "ConsultaLoteAssincrono", LoteReinf, "retornoLoteEventosAssincrono"),
            new(TipoDFe.EFDReinf, Servico.EFDReinfConsultaReciboEvento, "ConsultaReciboEvento", ReciboReinf, "ideStatus")
        };

        public static IEnumerable<object[]> ContratosCruzados()
        {
            foreach (var operacao in Casos)
                foreach (var resposta in Casos.Where(r => r.Tipo == operacao.Tipo))
                    yield return new object[] { operacao.Tipo + "|" + operacao.Nome, resposta.Tipo + "|" + resposta.Nome, operacao.Namespace == resposta.Namespace };
        }

        [Theory, MemberData(nameof(ContratosCruzados)), Trait("Utility", "Disponibilidade")]
        public void ConsumidorRealSoReconheceContratoAdmitidoPelaOperacao(string operacao, string resposta, bool compativel)
        {
            var cfg = CriarConfig(operacao);
            var item = Executar(cfg, XmlResposta(resposta));
            Assert.Equal(compativel, item.RespostaAplicacaoReconhecida);
            Assert.Equal(compativel ? StatusDisponibilidade.Operacional : StatusDisponibilidade.Inconclusivo, item.Status);
            if (!compativel)
            {
                Assert.Equal(TipoFalhaDisponibilidade.Protocolo, item.TipoFalha);
                Assert.Equal(0, item.CodigoRespostaAplicacao);
                Assert.Contains("contrato", item.Limitacao);
            }
        }

        [Theory]
        [InlineData("EnviarLoteEventosESocial", 201, StatusDisponibilidade.Operacional)]
        [InlineData("EnviarLoteEventosESocial", 401, StatusDisponibilidade.Operacional)]
        [InlineData("ConsultaLoteAssincrono", 301, StatusDisponibilidade.Degradado)]
        [InlineData("ConsultarEvtsEmpregador", 301, StatusDisponibilidade.Degradado)]
        [InlineData("DownloadPorID", 301, StatusDisponibilidade.Degradado)]
        [InlineData("RecepcionarLoteAssincrono", 99, StatusDisponibilidade.Degradado)]
        [InlineData("ConsultaReciboEvento", 0, StatusDisponibilidade.Operacional)]
        [Trait("Utility", "Disponibilidade")]
        public void ContratoCorretoPreservaRejeicaoNormalEFalhaPrincipal(string operacao, int codigo, StatusDisponibilidade status)
        {
            var item = Executar(CriarConfig(operacao), XmlResposta(operacao, codigo));
            Assert.True(item.RespostaAplicacaoReconhecida);
            Assert.Equal(status, item.Status);
            Assert.Equal(codigo, item.CodigoRespostaAplicacao);
        }

        [Theory]
        [InlineData("EnviarLoteEventosESocial")]
        [InlineData("DownloadPorID")]
        [InlineData("RecepcionarLoteAssincrono")]
        [InlineData("ConsultaReciboEvento")]
        [Trait("Utility", "Disponibilidade")]
        public void NamespaceOuVersaoDesconhecidosNaoConfirmamServico(string operacao)
        {
            var caso = Casos.First(c => c.Nome == operacao);
            foreach (var ns in new[] { caso.Namespace + "_desconhecido", "urn:outro", "" })
            {
                var xml = new XmlDocument(); xml.LoadXml(XmlResposta(operacao));
                var texto = xml.OuterXml.Replace(caso.Namespace, ns);
                Assert.Equal(StatusDisponibilidade.Inconclusivo, Executar(CriarConfig(operacao), texto).Status);
            }
        }

        [Fact, Trait("Utility", "Disponibilidade")]
        public void NamespaceEstrangeiroNoStatusNaoEmprestaCodigoMesmoComRaizCorreta()
        {
            var xml = "<eSocial xmlns='" + Envio + "'><retornoEnvioLoteEventos><status xmlns='urn:outro'><cdResposta>201</cdResposta></status></retornoEnvioLoteEventos></eSocial>";
            Assert.False(Executar(CriarConfig("EnviarLoteEventosESocial"), xml).RespostaAplicacaoReconhecida);
        }

        [Fact, Trait("Utility", "Disponibilidade")]
        public void PrefixoXmlNaoImpedeReconhecerNamespaceCorreto()
        {
            var xml = "<es:eSocial xmlns:es='" + Envio + "'><es:retornoEnvioLoteEventos><es:status><es:cdResposta>201</es:cdResposta></es:status></es:retornoEnvioLoteEventos></es:eSocial>";
            Assert.Equal(StatusDisponibilidade.Operacional, Executar(CriarConfig("EnviarLoteEventosESocial"), xml).Status);
        }

        [Fact, Trait("Utility", "Disponibilidade")]
        public void EnumDeConsultaAmbiguoSemOperacaoNaoConfirmaOutroContrato()
        {
            var cfg = new Configuracao { TipoDFe = TipoDFe.ESocial, Servico = Servico.ESocialConsultaEvts, ColetarTelemetriaDisponibilidade = true };
            Assert.Equal(StatusDisponibilidade.Inconclusivo, Executar(cfg, XmlResposta("ConsultaLoteAssincrono")).Status);
        }

        [Theory]
        [InlineData("ConsultarLoteEventosResult", "ConsultaLoteAssincrono", true)]
        [InlineData("ConsultarIdentificadoresEventosTabelaResult", "ConsultarEvtsTabela", true)]
        [InlineData("ConsultarLoteEventosResult", "ConsultarEvtsTabela", false)]
        [InlineData("ConsultarIdentificadoresEventosEmpregadorResult", "ConsultaLoteAssincrono", false)]
        [InlineData("WrapperDesconhecido", "ConsultaLoteAssincrono", false)]
        [Trait("Utility", "Disponibilidade")]
        public void ConsultaDeBaixoNivelExigeIdentidadeExplicitaNoWrapper(string wrapper, string resposta, bool compativel)
        {
            var cfg = new Configuracao { TipoDFe = TipoDFe.ESocial, Servico = Servico.ESocialConsultaEvts,
                WebTagRetorno = wrapper, ColetarTelemetriaDisponibilidade = true };
            Assert.Equal(compativel, Executar(cfg, XmlResposta(resposta)).RespostaAplicacaoReconhecida);
        }

        [Theory]
        [InlineData("abc")]
        [InlineData("-1")]
        [InlineData("")]
        [Trait("Utility", "Disponibilidade")]
        public void ContratoCorretoSemCodigoPrincipalValidoPermaneceInconclusivo(string codigo)
        {
            var xml = XmlResposta("EnviarLoteEventosESocial").Replace("<cdResposta>201</cdResposta>", "<cdResposta>" + codigo + "</cdResposta>");
            Assert.Equal(StatusDisponibilidade.Inconclusivo, Executar(CriarConfig("EnviarLoteEventosESocial"), xml).Status);
        }

        [Theory]
        [InlineData(401, TipoFalhaDisponibilidade.Autenticacao)]
        [InlineData(503, TipoFalhaDisponibilidade.HTTP)]
        [Trait("Utility", "Disponibilidade")]
        public void ContratoCruzadoNaoApagaFalhaHttp(int http, TipoFalhaDisponibilidade falha)
        {
            var item = Executar(CriarConfig("RecepcionarLoteAssincrono"), XmlResposta("ConsultaReciboEvento"), (HttpStatusCode)http);
            Assert.False(item.RespostaAplicacaoReconhecida);
            Assert.Equal(falha, item.TipoFalha);
        }

        [Theory]
        [InlineData(false)]
        [InlineData(true)]
        [Trait("Utility", "Disponibilidade")]
        public void ConsultaPublicaESocialPreservaRetornoComContratoCorretoOuCruzado(bool cruzado)
        {
            using var rsa = RSA.Create(2048);
            using var cert = new CertificateRequest("CN=DIAGNOSTICO TESTE", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1)
                .CreateSelfSigned(DateTimeOffset.Now.AddDays(-1), DateTimeOffset.Now.AddDays(1));
            var cfg = new Configuracao { TipoDFe = TipoDFe.ESocial, TipoAmbiente = TipoAmbiente.Homologacao, CertificadoDigital = cert, ColetarTelemetriaDisponibilidade = true };
            var executor = new SoapFake(XmlResposta(cruzado ? "ConsultarEvtsEmpregador" : "ConsultaLoteAssincrono"));
            var canal = new CanalObservacoesDisponibilidade();
            using (SoapTransportExecutorFactory.Override(() => executor))
            using (var sessao = canal.IniciarOperacao(cfg, Guid.NewGuid(), Guid.NewGuid()))
            {
                var servico = new Unimake.Business.DFe.Servicos.ESocial.ConsultaLoteAssincrono("1.6.0000000000000000001", cfg);
                servico.Executar();
                var original = servico.RetornoWSString;
                if (!cruzado) Assert.Equal(201, servico.Result.RetornoProcessamentoLoteEventos.Status.CdResposta);
                sessao.Concluir(servico.RetornoWSXML, servico.HttpStatusCode);
                Assert.Equal(original, servico.RetornoWSString);
            }
            Assert.Equal(1, executor.Chamadas);
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal(cruzado ? StatusDisponibilidade.Inconclusivo : StatusDisponibilidade.Operacional, item.Status);
        }

        private static Configuracao CriarConfig(string nome)
        {
            var caso = ObterCaso(nome);
            var cfg = new Configuracao { TipoDFe = caso.Tipo, Servico = caso.Servico, CodigoUF = (int)UFBrasil.AN,
                TipoAmbiente = TipoAmbiente.Homologacao, SchemaVersao = caso.Tipo == TipoDFe.ESocial ? "1.1.0" : caso.Nome == "ConsultaReciboEvento" ? "1.05.01" : "1.00.00",
                ColetarTelemetriaDisponibilidade = true };
            if (caso.Tipo == TipoDFe.ESocial) cfg.Load(caso.Nome, "AN.xml");
            return cfg;
        }

        private static string XmlResposta(string nome, int? codigo = null)
        {
            var caso = ObterCaso(nome);
            var recibo = caso.Namespace == ReciboReinf;
            var raiz = caso.Tipo == TipoDFe.ESocial ? "eSocial" : "Reinf";
            var valor = codigo ?? (recibo ? 0 : caso.Tipo == TipoDFe.ESocial ? 201 : 2);
            var descricao = recibo ? "descRetorno" : "descResposta";
            var status = (recibo ? "" : "<status>") + "<" + (recibo ? "cdRetorno" : "cdResposta") + ">" + valor + "</" + (recibo ? "cdRetorno" : "cdResposta") + "><" + descricao + ">SEGREDO</" + descricao + ">" + (recibo ? "" : "</status>");
            return "<" + raiz + " xmlns='" + caso.Namespace + "'><" + caso.Grupo + ">" + status + "</" + caso.Grupo + "></" + raiz + ">";
        }

        private static ObservacaoDisponibilidade Executar(Configuracao cfg, string xml, HttpStatusCode http = HttpStatusCode.OK)
        {
            var executor = new ApiFake(xml, http);
            var canal = new CanalObservacoesDisponibilidade();
            using (ApiTransportExecutorFactory.Override(() => executor))
            using (var sessao = canal.IniciarOperacao(cfg, Guid.NewGuid(), Guid.NewGuid()))
            using (var consumidor = new ConsumirAPI())
            {
                consumidor.ExecutarServico(new APIConfig { RequestURI = "https://api.test/SEGREDO?token=SEGREDO", MetodoAPI = "post", TagRetorno = "prop:innertext", UsaCertificadoDigital = false, UseProxy = false }, null);
                var original = consumidor.RetornoServicoXML.OuterXml;
                sessao.Concluir(consumidor.RetornoServicoXML, consumidor.HttpStatusCode);
                Assert.Equal(original, consumidor.RetornoServicoXML.OuterXml);
            }
            Assert.Equal(1, executor.Chamadas);
            Assert.True(canal.TentarLer(out var item));
            Assert.False(canal.TentarLer(out _));
            Assert.Equal(1, item.EntradasTransporte);
            Assert.DoesNotContain("SEGREDO", string.Join("|", typeof(ObservacaoDisponibilidade).GetProperties().Select(p => p.GetValue(item))));
            return item;
        }

        private static Caso ObterCaso(string nome) => Casos.First(c => c.Nome == nome || c.Tipo + "|" + c.Nome == nome);
        private sealed record Caso(TipoDFe Tipo, Servico Servico, string Nome, string Namespace, string Grupo);
        private sealed class ApiFake(string xml, HttpStatusCode http) : IApiTransportExecutor
        {
            internal int Chamadas;
            public TransportResponse Execute(TransportRequest request)
            { Chamadas++; return new TransportResponse { StatusCode = http, HttpResponseMessage = new HttpResponseMessage(http) { Content = new StringContent(xml, Encoding.UTF8, "application/xml") } }; }
        }
        private sealed class SoapFake(string xml) : ISoapTransportExecutor
        {
            internal int Chamadas;
            public TransportResponse Execute(TransportRequest request)
            { Chamadas++; return new TransportResponse { StatusCode = HttpStatusCode.OK, Content = "<soap:Envelope xmlns:soap='http://schemas.xmlsoap.org/soap/envelope/'><soap:Body><ConsultarLoteEventosResult>" + xml + "</ConsultarLoteEventosResult></soap:Body></soap:Envelope>" }; }
        }
    }
}
