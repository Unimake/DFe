using System;
using System.Collections.Generic;
using System.Net;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.Utility.Rede
{
    [Collection("MemoriaDiagnosticoDisponibilidade")]
    public class MigracaoDiagnosticoSessaoTest : IDisposable
    {
        public MigracaoDiagnosticoSessaoTest() => DiagnosticoDisponibilidadeDFe.LimparMemoriaDiagnostico();
        public void Dispose() => DiagnosticoDisponibilidadeDFe.LimparMemoriaDiagnostico();

        private static Configuracao Config(TipoDFe tipo = TipoDFe.NFe) => new Configuracao
        {
            TipoDFe = tipo, Servico = Servico.NFeStatusServico, CodigoUF = 35,
            TipoAmbiente = TipoAmbiente.Homologacao, ColetarTelemetriaDisponibilidade = true
        };

        private static X509Certificate2 Certificado()
        {
            using var rsa = RSA.Create(2048);
            return new CertificateRequest("CN=DIAGNOSTICO TESTE", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1)
                .CreateSelfSigned(DateTimeOffset.Now.AddDays(-1), DateTimeOffset.Now.AddDays(1));
        }

        [Theory]
        [InlineData(107, StatusDisponibilidade.Operacional)]
        [InlineData(108, StatusDisponibilidade.Indisponivel)]
        [InlineData(656, StatusDisponibilidade.Degradado)]
        [Trait("Utility", "Disponibilidade")]
        public void ServicoPublicoComSessaoEntregaUmaObservacaoSemHistoricoDuplicado(int codigo, StatusDisponibilidade status)
        {
            using var certificado = Certificado();
            var config = Config(); config.CertificadoDigital = certificado;
            var executor = new SoapFake(codigo);
            var canal = new CanalObservacoesDisponibilidade();
            using (SoapTransportExecutorFactory.Override(() => executor))
            using (var sessao = canal.IniciarOperacao(config, Guid.NewGuid(), Guid.NewGuid()))
            {
                var servico = new Unimake.Business.DFe.Servicos.NFe.StatusServico(new Unimake.Business.DFe.Xml.NFe.ConsStatServ
                { Versao = "4.00", CUF = UFBrasil.SP, TpAmb = TipoAmbiente.Homologacao }, config);
                servico.Executar();
                Assert.Equal(codigo, servico.Result.CStat);
                var original = servico.RetornoWSString;
                sessao.Concluir(servico.RetornoWSXML, servico.HttpStatusCode);
                Assert.Equal(original, servico.RetornoWSString);
                Assert.Equal(codigo, servico.Result.CStat);
            }
            Assert.Equal(1, executor.Chamadas);
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal(status, item.Status);
            Assert.Equal(codigo, item.CStat);
            Assert.False(canal.TentarLer(out _));
            Assert.Empty(TelemetriaDisponibilidade.Obter(config, new ConfiguracaoDiagnosticoDisponibilidade()));
            Assert.DoesNotContain("SEGREDO", item.Limitacao);
            Assert.Equal(codigo == 656, CacheStatusDisponibilidade.ContextoBloqueado(config, out _));
        }

        [Fact, Trait("Utility", "Disponibilidade")]
        public void TransicaoSemSessaoMantemContratoAnteriorSemSuprimirOutraConfiguracao()
        {
            var config = Config();
            var outro = Config();
            var canal = new CanalObservacoesDisponibilidade();
            using (var sessao = canal.IniciarOperacao(config, Guid.NewGuid(), Guid.NewGuid()))
            {
                Assert.False(TelemetriaDisponibilidade.EstaHabilitada(config));
                Assert.True(TelemetriaDisponibilidade.EstaHabilitada(outro));
                sessao.Concluir(null, 0);
                Assert.True(TelemetriaDisponibilidade.EstaHabilitada(config));
            }
            var xml = new XmlDocument(); xml.LoadXml("<ret><cStat>108</cStat></ret>");
            TelemetriaDisponibilidade.Registrar(config, "https://servico.test/SEGREDO", "SOAP", 1, HttpStatusCode.OK, xml, null);
            Assert.Single(TelemetriaDisponibilidade.Obter(config, new ConfiguracaoDiagnosticoDisponibilidade()));
        }

        [Fact, Trait("Utility", "Disponibilidade")]
        public void VerificacaoManualNovaNaoLeNemApagaHistoricoPassivoEReutilizaCacheSeguro()
        {
            using var certificado = Certificado();
            var config = Config(); config.CertificadoDigital = certificado;
            var xml = new XmlDocument(); xml.LoadXml("<ret><cStat>108</cStat></ret>");
            TelemetriaDisponibilidade.Registrar(config, "https://servico.test/antigo", "SOAP", 1, HttpStatusCode.OK, xml, null);
            var chamadas = 0;
            var diagnostico = new DiagnosticoDisponibilidadeDFe(config, null, new InfraFake(), (cfg, endpoint) =>
            {
                chamadas++;
                return new ResultadoSondaDisponibilidade { Servico = "StatusServico", Endpoint = endpoint,
                    Fonte = FonteEvidenciaDisponibilidade.StatusServico, Status = StatusDisponibilidade.Operacional,
                    CStat = 107, Essencial = true, DataHora = RelogioDisponibilidade.Agora() };
            });
            var resultado = diagnostico.VerificarStatusServico();
            Assert.Equal(StatusDisponibilidade.Operacional, resultado.Status);
            Assert.DoesNotContain(resultado.Sondas.Itens, s => s.Fonte == FonteEvidenciaDisponibilidade.TelemetriaPassiva);
            Assert.Single(TelemetriaDisponibilidade.Obter(config, new ConfiguracaoDiagnosticoDisponibilidade()));
            var segundo = diagnostico.VerificarStatusServico();
            Assert.Equal(1, chamadas);
            Assert.Contains(segundo.Sondas.Itens, s => s.Fonte == FonteEvidenciaDisponibilidade.StatusServico && s.DoCache);
        }

        [Theory]
        [InlineData(656, 200)]
        [InlineData(678, 200)]
        [InlineData(0, 429)]
        [Trait("Utility", "Disponibilidade")]
        public void ConsumoIndevidoDaSessaoBloqueiaStatusMesmoSeFilaEstiverCheia(int codigo, int http)
        {
            var config = Config();
            var canal = new CanalObservacoesDisponibilidade(1);
            using (var anterior = canal.IniciarOperacao(config, Guid.NewGuid(), Guid.NewGuid())) anterior.Concluir(null, 0);
            using (var sessao = canal.IniciarOperacao(config, Guid.NewGuid(), Guid.NewGuid()))
            {
                SessaoObservacaoDisponibilidade.InformarTransporte("https://servico.test/SEGREDO", "SOAP");
                var xml = new XmlDocument(); xml.LoadXml("<ret><cStat>" + codigo + "</cStat></ret>");
                sessao.Concluir(xml, (HttpStatusCode)http);
            }
            Assert.Equal(1, canal.ObservacoesDescartadas);
            Assert.True(CacheStatusDisponibilidade.ContextoBloqueado(config, out _));
            var chamadas = 0;
            var infra = new InfraFake();
            var diagnostico = new DiagnosticoDisponibilidadeDFe(config, null, infra, (_, _) => { chamadas++; throw new Exception("Nunca executar"); });
            var resultado = diagnostico.VerificarStatusServico();
            Assert.Equal(0, chamadas);
            Assert.Equal(0, infra.Chamadas);
            Assert.Contains(resultado.Sondas.Itens, s => s.Fonte == FonteEvidenciaDisponibilidade.StatusServico && s.TipoFalha == TipoFalhaDisponibilidade.ConsumoIndevido);
            Assert.Empty(TelemetriaDisponibilidade.Obter(config, new ConfiguracaoDiagnosticoDisponibilidade()));
        }

        [Theory]
        [InlineData(TipoDFe.NFSe)]
        [InlineData(TipoDFe.ESocial)]
        [InlineData(TipoDFe.CIOT)]
        [Trait("Utility", "Disponibilidade")]
        public void SemStatusOficialNovaApiNaoChamaInfraestruturaNemStatus(TipoDFe tipo)
        {
            var infra = new InfraFake();
            var diagnostico = new DiagnosticoDisponibilidadeDFe(Config(tipo), null, infra,
                (_, _) => throw new Exception("Não pode executar status"));
            Assert.Equal(StatusDisponibilidade.NaoAplicavel, diagnostico.VerificarStatusServico().Status);
            Assert.Equal(0, infra.Chamadas);
        }

        [Fact, Trait("Utility", "Disponibilidade")]
        public void BloqueioPassivoExpiraSemLimparMemoriaOuExecutarSondaExtra()
        {
            var anterior = RelogioDisponibilidade.Agora;
            var agora = DateTime.Now;
            try
            {
                RelogioDisponibilidade.Agora = () => agora;
                var config = Config();
                using (var sessao = new CanalObservacoesDisponibilidade().IniciarOperacao(config, Guid.NewGuid(), Guid.NewGuid()))
                {
                    SessaoObservacaoDisponibilidade.InformarTransporte("https://servico.test", "SOAP");
                    var xml = new XmlDocument(); xml.LoadXml("<ret><cStat>656</cStat></ret>");
                    sessao.Concluir(xml, HttpStatusCode.OK);
                }
                Assert.True(CacheStatusDisponibilidade.ContextoBloqueado(config, out _));
                agora = agora.AddMinutes(61);
                Assert.False(CacheStatusDisponibilidade.ContextoBloqueado(config, out _));
                Assert.Empty(TelemetriaDisponibilidade.Obter(config, new ConfiguracaoDiagnosticoDisponibilidade()));
            }
            finally { RelogioDisponibilidade.Agora = anterior; }
        }

        [Theory]
        [InlineData(TipoDFe.Desconhecido, 2)]
        [InlineData(TipoDFe.ESocial, 2)]
        [InlineData(TipoDFe.NFe, 999)]
        [Trait("Utility", "Disponibilidade")]
        public void IntegracaoSemStatusOuAmbienteInvalidoNaoAmpliaRegistroDeBloqueios(TipoDFe tipo, int ambiente)
        {
            var config = Config(tipo); config.TipoAmbiente = (TipoAmbiente)ambiente;
            var canal = new CanalObservacoesDisponibilidade();
            using (var sessao = canal.IniciarOperacao(config, Guid.NewGuid(), Guid.NewGuid()))
            {
                SessaoObservacaoDisponibilidade.InformarTransporte("https://servico.test", "REST");
                sessao.Concluir(null, (HttpStatusCode)429);
            }
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal(TipoFalhaDisponibilidade.ConsumoIndevido, item.TipoFalha);
            Assert.False(CacheStatusDisponibilidade.ContextoBloqueado(config, out _));
        }

        private sealed class InfraFake : IExecutorInfraestruturaDisponibilidade
        {
            internal int Chamadas;
            public IList<ResultadoSondaDisponibilidade> Executar(Configuracao configuracao, string endpoint, int timeoutMilissegundos)
            { Chamadas++; return new List<ResultadoSondaDisponibilidade>(); }
        }

        private sealed class SoapFake(int codigo) : ISoapTransportExecutor
        {
            internal int Chamadas;
            public TransportResponse Execute(TransportRequest request)
            {
                Chamadas++;
                return new TransportResponse { StatusCode = HttpStatusCode.OK,
                    Content = "<soap:Envelope xmlns:soap='http://www.w3.org/2003/05/soap-envelope'><soap:Body><nfeResultMsg xmlns='http://www.portalfiscal.inf.br/nfe/wsdl/NFeStatusServico4'><retConsStatServ xmlns='http://www.portalfiscal.inf.br/nfe' versao='4.00'><tpAmb>2</tpAmb><verAplic>TESTE</verAplic><cStat>" + codigo + "</cStat><xMotivo>SEGREDO</xMotivo><cUF>35</cUF><dhRecbto>2026-11-09T10:00:00-03:00</dhRecbto></retConsStatServ></nfeResultMsg></soap:Body></soap:Envelope>" };
            }
        }
    }
}
