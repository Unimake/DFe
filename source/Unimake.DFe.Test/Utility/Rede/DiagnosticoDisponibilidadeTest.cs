using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Reflection;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Threading;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.Utility.Rede
{
    /// <summary>Testes determinísticos do diagnóstico de disponibilidade.</summary>
    public class DiagnosticoDisponibilidadeTest : IDisposable
    {
        public DiagnosticoDisponibilidadeTest() => DiagnosticoDisponibilidadeDFe.LimparMemoriaDiagnostico();

        public void Dispose()
        {
            RelogioDisponibilidade.Agora = () => DateTime.Now;
            DiagnosticoDisponibilidadeDFe.LimparMemoriaDiagnostico();
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void TelemetriaFicaDesabilitadaPorPadrao()
        {
            var configuracao = ConfiguracaoBase();
            TelemetriaDisponibilidade.Registrar(configuracao, "https://sefaz.test/ws", "SOAP", 100,
                HttpStatusCode.OK, Retorno(204), null);

            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao).ObterDiagnosticoPassivo();

            Assert.Equal(0, resultado.Sondas.Count);
            Assert.Equal(StatusDisponibilidade.Inconclusivo, resultado.Status);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public async Task RegistroDeTelemetriaNaoEsperaQuandoHistoricoEstaOcupado()
        {
            var configuracao = ConfiguracaoBase();
            configuracao.ColetarTelemetriaDisponibilidade = true;
            var token = TestContext.Current.CancellationToken;

            await ComHistoricoBloqueado(async () =>
            {
                var registro = Task.Run(() => TelemetriaDisponibilidade.Registrar(configuracao,
                    "https://sefaz.test/ws", "SOAP", 100, HttpStatusCode.OK, Retorno(107), null), token);

                await registro.WaitAsync(TimeSpan.FromSeconds(1), token);
            });

            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao).ObterDiagnosticoPassivo();
            Assert.Equal(0, resultado.Sondas.Count);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public async Task ConsumoIndevidoBloqueiaContextoMesmoQuandoAmostraEhDescartada()
        {
            var agora = new DateTime(2026, 7, 20, 10, 0, 0);
            RelogioDisponibilidade.Agora = () => agora;
            var configuracao = ConfiguracaoBase();
            configuracao.ColetarTelemetriaDisponibilidade = true;
            var token = TestContext.Current.CancellationToken;

            await ComHistoricoBloqueado(async () =>
            {
                var registro = Task.Run(() => TelemetriaDisponibilidade.Registrar(configuracao,
                    "https://sefaz.test/ws", "SOAP", 100, HttpStatusCode.OK, Retorno(656), null), token);
                await registro.WaitAsync(TimeSpan.FromSeconds(1), token);
            });

            DateTime bloqueadoAte;
            Assert.True(CacheStatusDisponibilidade.ContextoBloqueado(configuracao, out bloqueadoAte));
            Assert.Equal(agora.AddHours(1), bloqueadoAte);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void TelemetriaNaoCarregaCertificadoParaCriarIdentidade()
        {
            var configuracao = ConfiguracaoBase();
            configuracao.ColetarTelemetriaDisponibilidade = true;
            configuracao.CertificadoArquivo = @"C:\certificados\inexistente.pfx";
            configuracao.CertificadoSenha = "senha-sigilosa";
            var falhaTls = new WebException("Falha TLS", WebExceptionStatus.SecureChannelFailure);

            TelemetriaDisponibilidade.Registrar(configuracao, "https://sefaz.test/ws", "SOAP", 100,
                (HttpStatusCode)0, null, falhaTls);

            Assert.Null(configuracao.CertificadoDigitalCarregado);
            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao).ObterDiagnosticoPassivo();
            var amostra = Assert.Single(resultado.Sondas.Itens);
            Assert.Equal(TipoFalhaDisponibilidade.TLS, amostra.TipoFalha);
            Assert.Null(configuracao.CertificadoDigitalCarregado);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void RespostaFiscalRealComprovaProcessamento()
        {
            var configuracao = ConfiguracaoBase();
            configuracao.ColetarTelemetriaDisponibilidade = true;
            TelemetriaDisponibilidade.Registrar(configuracao, "https://sefaz.test/ws?token=segredo", "SOAP", 100,
                HttpStatusCode.OK, Retorno(204), null);

            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao).ObterDiagnosticoPassivo();

            Assert.Equal(StatusDisponibilidade.Operacional, resultado.Status);
            Assert.Equal(1, resultado.Sondas.Count);
            Assert.Equal(FonteEvidenciaDisponibilidade.TelemetriaPassiva, resultado.Sondas.GetItem(0).Fonte);
            Assert.Equal("https://sefaz.test/ws", resultado.Sondas.GetItem(0).Endpoint);
            Assert.Equal("A mensagem foi processada pela aplicação fiscal.", resultado.Sondas.GetItem(0).XMotivo);
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [MemberData(nameof(RetornosFiscaisComCStat))]
        public void TelemetriaExtraiPrimeiroCStatDosRetornosImplementados(TipoDFe tipoDFe, Servico servico,
            string xmlRetorno, int cStatEsperado, StatusDisponibilidade statusEsperado)
        {
            var configuracao = ConfiguracaoBase();
            configuracao.TipoDFe = tipoDFe;
            configuracao.Servico = servico;
            configuracao.ColetarTelemetriaDisponibilidade = true;
            var retorno = new XmlDocument();
            retorno.LoadXml(xmlRetorno);

            TelemetriaDisponibilidade.Registrar(configuracao, "https://sefaz.test/ws", "SOAP", 100,
                HttpStatusCode.OK, retorno, null);

            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao).ObterDiagnosticoPassivo();
            var amostra = Assert.Single(resultado.Sondas.Itens);
            Assert.Equal(cStatEsperado, amostra.CStat);
            Assert.Equal(statusEsperado, amostra.Status);
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [MemberData(nameof(ServicosMonitorados))]
        public void TelemetriaRegistraCadaServicoDasPastasSolicitadas(TipoDFe tipoDFe, Servico servico)
        {
            var configuracao = ConfiguracaoBase();
            configuracao.TipoDFe = tipoDFe;
            configuracao.Servico = servico;
            configuracao.ColetarTelemetriaDisponibilidade = true;

            Assert.True(TelemetriaDisponibilidade.EstaHabilitada(configuracao));
            TelemetriaDisponibilidade.Registrar(configuracao, "https://sefaz.test/ws", "SOAP", 100,
                HttpStatusCode.OK, Retorno(204), null);

            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao).ObterDiagnosticoPassivo();
            var amostra = Assert.Single(resultado.Sondas.Itens);
            Assert.Equal(servico.ToString(), amostra.Servico);
            Assert.Equal(StatusDisponibilidade.Operacional, amostra.Status);
            Assert.Equal(servico.ToString().IndexOf("Autorizacao", StringComparison.OrdinalIgnoreCase) >= 0 ||
                servico.ToString().IndexOf("StatusServico", StringComparison.OrdinalIgnoreCase) >= 0,
                amostra.Essencial);
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [InlineData(107, StatusDisponibilidade.Operacional, TipoFalhaDisponibilidade.Nenhuma)]
        [InlineData(108, StatusDisponibilidade.Indisponivel, TipoFalhaDisponibilidade.Protocolo)]
        [InlineData(109, StatusDisponibilidade.Indisponivel, TipoFalhaDisponibilidade.Protocolo)]
        [InlineData(999, StatusDisponibilidade.Indisponivel, TipoFalhaDisponibilidade.Protocolo)]
        [InlineData(656, StatusDisponibilidade.Degradado, TipoFalhaDisponibilidade.ConsumoIndevido)]
        [InlineData(678, StatusDisponibilidade.Degradado, TipoFalhaDisponibilidade.ConsumoIndevido)]
        public void ClassificaStatusFiscal(int cStat, StatusDisponibilidade status, TipoFalhaDisponibilidade falha)
        {
            var resultado = new ResultadoSondaDisponibilidade { CStat = cStat };
            ClassificadorDisponibilidade.ClassificarRespostaFiscal(resultado);
            Assert.Equal(status, resultado.Status);
            Assert.Equal(falha, resultado.TipoFalha);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ConsumoIndevidoFicaDegradadoComOrigemEspecifica()
        {
            var configuracao = ConfiguracaoBase();
            configuracao.ColetarTelemetriaDisponibilidade = true;
            TelemetriaDisponibilidade.Registrar(configuracao, "https://sefaz.test/ws", "SOAP", 100,
                HttpStatusCode.OK, Retorno(656), null);

            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao).ObterDiagnosticoPassivo();

            Assert.Equal(StatusDisponibilidade.Degradado, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.ConsumoIndevido, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ConsumoIndevidoPassivoBloqueiaConsultaDiagnosticaPorUmaHora()
        {
            var agora = new DateTime(2026, 7, 20, 10, 0, 0);
            RelogioDisponibilidade.Agora = () => agora;
            var configuracao = ConfiguracaoBase();
            configuracao.ColetarTelemetriaDisponibilidade = true;
            TelemetriaDisponibilidade.Registrar(configuracao, "https://sefaz.test/ws", "SOAP", 100,
                HttpStatusCode.OK, Retorno(656), null);

            DateTime bloqueadoAte;
            Assert.True(CacheStatusDisponibilidade.ContextoBloqueado(configuracao, out bloqueadoAte));
            Assert.Equal(agora.AddHours(1), bloqueadoAte);
            agora = agora.AddHours(1).AddSeconds(1);
            Assert.False(CacheStatusDisponibilidade.ContextoBloqueado(configuracao, out bloqueadoAte));
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ConsumoIndevidoNacionalSuprimeConsultaStatusDaUF()
        {
            var agora = new DateTime(2026, 7, 20, 10, 0, 0);
            RelogioDisponibilidade.Agora = () => agora;
            var configuracaoNacional = ConfiguracaoBase();
            configuracaoNacional.CodigoUF = (int)UFBrasil.AN;
            configuracaoNacional.Servico = Servico.NFeDistribuicaoDFe;
            configuracaoNacional.ColetarTelemetriaDisponibilidade = true;
            TelemetriaDisponibilidade.Registrar(configuracaoNacional, "https://sefaz.test/distribuicao", "SOAP", 100,
                HttpStatusCode.OK, Retorno(656), null);

            using (var certificado = CriarCertificadoValido(agora))
            {
                var configuracaoUF = ConfiguracaoBase();
                configuracaoUF.CertificadoDigital = certificado;
                var execucoesStatus = 0;
                var diagnostico = new DiagnosticoDisponibilidadeDFe(configuracaoUF, null,
                    new ExecutorInfraestruturaFake(), (configuracao, endpoint) =>
                    {
                        execucoesStatus++;
                        return Status(107);
                    });

                var resultado = diagnostico.ConsultarStatusServico();

                Assert.Equal(0, execucoesStatus);
                Assert.Contains(resultado.Sondas.Itens, x => x.Servico == "StatusServico" &&
                    x.TipoFalha == TipoFalhaDisponibilidade.ConsumoIndevido && x.DoCache);
                DateTime bloqueadoAte;
                Assert.True(CacheStatusDisponibilidade.ContextoBloqueado(configuracaoUF, out bloqueadoAte));
                Assert.Equal(agora.AddHours(1), bloqueadoAte);
            }
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [InlineData(0)]
        [InlineData(1)]
        [InlineData(2)]
        public void CertificadoInvalidoSuprimeConsultaStatus(int tipoCertificado)
        {
            var agora = new DateTime(2026, 7, 20, 10, 0, 0);
            RelogioDisponibilidade.Agora = () => agora;
            var certificado = CriarCertificadoInvalido(tipoCertificado, agora);
            try
            {
                var configuracao = ConfiguracaoBase();
                configuracao.CertificadoDigital = certificado;
                var execucoesStatus = 0;
                var diagnostico = new DiagnosticoDisponibilidadeDFe(configuracao, null,
                    new ExecutorInfraestruturaFake(), (configuracaoStatus, endpoint) =>
                    {
                        execucoesStatus++;
                        return Status(107);
                    });

                var resultado = diagnostico.ConsultarStatusServico();

                Assert.Equal(0, execucoesStatus);
                Assert.Equal(StatusDisponibilidade.Inconclusivo, resultado.Status);
                Assert.Equal(OrigemProvavelIndisponibilidade.AmbienteLocal, resultado.OrigemProvavel);
                Assert.Contains(resultado.Sondas.Itens, x => x.Essencial &&
                    x.TipoFalha == TipoFalhaDisponibilidade.Certificado);
                Assert.Equal("Não foi possível acessar a SEFAZ porque o certificado digital não está disponível ou precisa ser revisado.",
                    resultado.Descricao);
            }
            finally
            {
                if (certificado != null)
                {
                    certificado.Dispose();
                }
            }
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void FalhaAoCarregarCertificadoEhClassificadaComoCertificado()
        {
            var configuracao = ConfiguracaoBase();
            configuracao.CertificadoArquivo = @"C:\Clientes\Empresa Sigilosa\certificado-inexistente.pfx";
            configuracao.CertificadoSenha = "senha-sigilosa";
            var executor = new ExecutorInfraestruturaFake();

            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao, null, executor).Executar();

            Assert.Equal(0, executor.Execucoes);
            Assert.Equal(StatusDisponibilidade.Inconclusivo, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.AmbienteLocal, resultado.OrigemProvavel);
            var falha = Assert.Single(resultado.Sondas.Itens);
            Assert.Equal(TipoFalhaDisponibilidade.Certificado, falha.TipoFalha);
            Assert.DoesNotContain("Empresa Sigilosa", falha.Excecao, StringComparison.OrdinalIgnoreCase);
            Assert.Equal("Não foi possível acessar a SEFAZ porque o certificado digital não está disponível ou precisa ser revisado.",
                resultado.Descricao);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void Base64DeCertificadoInvalidoEhClassificadoComoCertificado()
        {
            var configuracao = ConfiguracaoBase();
            configuracao.CertificadoBase64 = "conteudo-base64-invalido";
            configuracao.CertificadoSenha = "senha-sigilosa";
            var executor = new ExecutorInfraestruturaFake();

            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao, null, executor).Executar();

            Assert.Equal(0, executor.Execucoes);
            Assert.Equal(StatusDisponibilidade.Inconclusivo, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.AmbienteLocal, resultado.OrigemProvavel);
            Assert.Contains(resultado.Sondas.Itens,
                x => x.TipoFalha == TipoFalhaDisponibilidade.Certificado && x.Essencial);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void FalhaEstruturalPrevaleceSobreConsumoIndevidoNoExecutar()
        {
            var configuracao = ConfiguracaoBase();
            configuracao.ColetarTelemetriaDisponibilidade = true;
            TelemetriaDisponibilidade.Registrar(configuracao, "https://sefaz.test/ws", "SOAP", 100,
                HttpStatusCode.OK, Retorno(656), null);

            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao, null,
                new ExecutorInfraestruturaFake()).Executar();

            Assert.Equal(StatusDisponibilidade.Inconclusivo, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.AmbienteLocal, resultado.OrigemProvavel);
            Assert.Contains(resultado.Sondas.Itens,
                x => x.TipoFalha == TipoFalhaDisponibilidade.ConsumoIndevido);
            Assert.Contains(resultado.Sondas.Itens,
                x => x.TipoFalha == TipoFalhaDisponibilidade.Certificado && x.Essencial);
            Assert.Equal("Não foi possível acessar a SEFAZ porque o certificado digital não está disponível ou precisa ser revisado.",
                resultado.Descricao);
            DateTime bloqueadoAte;
            Assert.True(CacheStatusDisponibilidade.ContextoBloqueado(configuracao, out bloqueadoAte));
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void EvidenciaExpiraDepoisDaJanelaConfigurada()
        {
            var agora = new DateTime(2026, 7, 20, 10, 0, 0);
            RelogioDisponibilidade.Agora = () => agora;
            var configuracao = ConfiguracaoBase();
            configuracao.ColetarTelemetriaDisponibilidade = true;
            TelemetriaDisponibilidade.Registrar(configuracao, "https://sefaz.test/ws", "SOAP", 100,
                HttpStatusCode.OK, Retorno(204), null);
            agora = agora.AddMinutes(16);

            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao).ObterDiagnosticoPassivo();

            Assert.Equal(0, resultado.Sondas.Count);
            Assert.Equal(StatusDisponibilidade.Inconclusivo, resultado.Status);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void HistoricoMantemNoMaximoVinteAmostrasPorServico()
        {
            var configuracao = ConfiguracaoBase();
            configuracao.ColetarTelemetriaDisponibilidade = true;
            for (var i = 0; i < 25; i++)
            {
                TelemetriaDisponibilidade.Registrar(configuracao, "https://sefaz.test/ws", "SOAP", i,
                    HttpStatusCode.OK, Retorno(204), null);
            }

            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao).ObterDiagnosticoPassivo();

            Assert.Equal(20, resultado.Sondas.Count);
            Assert.Equal(5, resultado.Sondas.GetItem(0).DuracaoMilissegundos);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void LimiteDeLentidaoEhAplicadoNaLeituraDaTelemetria()
        {
            var configuracao = ConfiguracaoBase();
            configuracao.ColetarTelemetriaDisponibilidade = true;
            TelemetriaDisponibilidade.Registrar(configuracao, "https://sefaz.test/ws", "SOAP", 4000,
                HttpStatusCode.OK, Retorno(204), null);

            var tolerante = new DiagnosticoDisponibilidadeDFe(configuracao,
                new ConfiguracaoDiagnosticoDisponibilidade { LimiteLentidaoMilissegundos = 5000 }).ObterDiagnosticoPassivo();
            var padrao = new DiagnosticoDisponibilidadeDFe(configuracao).ObterDiagnosticoPassivo();

            Assert.Equal(StatusDisponibilidade.Operacional, tolerante.Status);
            Assert.Equal(4000, padrao.Sondas.GetItem(0).DuracaoMilissegundos);
            Assert.Equal(StatusDisponibilidade.Degradado, padrao.Sondas.GetItem(0).Status);
            Assert.Equal(StatusDisponibilidade.Degradado, padrao.Status);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void DiagnosticoPassivoNaoExecutaInfraestrutura()
        {
            var executor = new ExecutorInfraestruturaFake();
            var diagnostico = new DiagnosticoDisponibilidadeDFe(ConfiguracaoBase(), null, executor);

            diagnostico.ObterDiagnosticoPassivo();

            Assert.Equal(0, executor.Execucoes);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ExecutarUsaInfraestruturaEReaproveitaCache()
        {
            var executor = new ExecutorInfraestruturaFake();
            var diagnostico = new DiagnosticoDisponibilidadeDFe(ConfiguracaoBase(), null, executor);

            var primeiro = diagnostico.Executar();
            var segundo = diagnostico.Executar();

            Assert.Equal(1, executor.Execucoes);
            Assert.Equal(FonteEvidenciaDisponibilidade.Infraestrutura, primeiro.Sondas.Itens.Last().Fonte);
            Assert.True(segundo.Sondas.Itens.Last().DoCache);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void CacheInfraestruturaSeparaTimeoutsDiferentes()
        {
            var configuracao = ConfiguracaoBase();
            var executor = new ExecutorInfraestruturaFake();
            var curto = new ConfiguracaoDiagnosticoDisponibilidade { TimeoutMilissegundos = 1000 };
            var longo = new ConfiguracaoDiagnosticoDisponibilidade { TimeoutMilissegundos = 10000 };

            var primeiro = CacheInfraestruturaDisponibilidade.ObterOuExecutar(configuracao,
                "https://sefaz.test/ws", curto, executor);
            var segundo = CacheInfraestruturaDisponibilidade.ObterOuExecutar(configuracao,
                "https://sefaz.test/ws", longo, executor);
            var repetido = CacheInfraestruturaDisponibilidade.ObterOuExecutar(configuracao,
                "https://sefaz.test/ws", longo, executor);

            Assert.Equal(2, executor.Execucoes);
            Assert.False(primeiro[0].DoCache);
            Assert.False(segundo[0].DoCache);
            Assert.True(repetido[0].DoCache);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void CacheInfraestruturaSeparaCredenciaisProxySemExpoLasNaChave()
        {
            var primeiroProxy = ConfiguracaoBase();
            primeiroProxy.HasProxy = true;
            primeiroProxy.ProxyUser = "usuario-cache-um";
            primeiroProxy.ProxyPassword = "senha-cache-um";
            var segundoProxy = ConfiguracaoBase();
            segundoProxy.HasProxy = true;
            segundoProxy.ProxyUser = "usuario-cache-dois";
            segundoProxy.ProxyPassword = "senha-cache-dois";
            var terceiroProxy = ConfiguracaoBase();
            terceiroProxy.HasProxy = true;
            terceiroProxy.ProxyUser = primeiroProxy.ProxyUser;
            terceiroProxy.ProxyPassword = "senha-cache-tres";
            var opcoes = new ConfiguracaoDiagnosticoDisponibilidade();
            var executor = new ExecutorInfraestruturaFake();

            var chavePrimeiro = CacheInfraestruturaDisponibilidade.CriarChaveCache(primeiroProxy,
                "https://sefaz.test/ws", opcoes.TimeoutMilissegundos);
            var chaveSegundo = CacheInfraestruturaDisponibilidade.CriarChaveCache(segundoProxy,
                "https://sefaz.test/ws", opcoes.TimeoutMilissegundos);
            var chaveTerceiro = CacheInfraestruturaDisponibilidade.CriarChaveCache(terceiroProxy,
                "https://sefaz.test/ws", opcoes.TimeoutMilissegundos);
            CacheInfraestruturaDisponibilidade.ObterOuExecutar(primeiroProxy,
                "https://sefaz.test/ws", opcoes, executor);
            CacheInfraestruturaDisponibilidade.ObterOuExecutar(segundoProxy,
                "https://sefaz.test/ws", opcoes, executor);
            CacheInfraestruturaDisponibilidade.ObterOuExecutar(terceiroProxy,
                "https://sefaz.test/ws", opcoes, executor);
            var repetido = CacheInfraestruturaDisponibilidade.ObterOuExecutar(terceiroProxy,
                "https://sefaz.test/ws", opcoes, executor);

            Assert.NotEqual(chavePrimeiro, chaveSegundo);
            Assert.NotEqual(chavePrimeiro, chaveTerceiro);
            Assert.DoesNotContain(primeiroProxy.ProxyUser, chavePrimeiro, StringComparison.Ordinal);
            Assert.DoesNotContain(primeiroProxy.ProxyPassword, chavePrimeiro, StringComparison.Ordinal);
            Assert.DoesNotContain(segundoProxy.ProxyUser, chaveSegundo, StringComparison.Ordinal);
            Assert.DoesNotContain(segundoProxy.ProxyPassword, chaveSegundo, StringComparison.Ordinal);
            Assert.DoesNotContain(terceiroProxy.ProxyUser, chaveTerceiro, StringComparison.Ordinal);
            Assert.DoesNotContain(terceiroProxy.ProxyPassword, chaveTerceiro, StringComparison.Ordinal);
            Assert.Equal(3, executor.Execucoes);
            Assert.True(repetido[0].DoCache);
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [InlineData(TipoDFe.NFe, "4.00")]
        [InlineData(TipoDFe.NFCe, "4.00")]
        [InlineData(TipoDFe.CTe, "4.00")]
        [InlineData(TipoDFe.MDFe, "3.00")]
        [InlineData(TipoDFe.NF3e, "1.00")]
        public void DocumentosSuportadosCarregamEndpointSemSondaFiscal(TipoDFe tipoDFe, string versao)
        {
            var configuracao = ConfiguracaoBase();
            configuracao.TipoDFe = tipoDFe;
            configuracao.SchemaVersao = versao;
            var executor = new ExecutorInfraestruturaFake();

            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao, null, executor).Executar();

            Assert.Equal(1, executor.Execucoes);
            Assert.DoesNotContain(resultado.Sondas.Itens, x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico);
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [InlineData(TipoDFe.BPe, Servico.BPeAutorizacao)]
        [InlineData(TipoDFe.CTeOS, Servico.CTeAutorizacaoOS)]
        [InlineData(TipoDFe.DCe, Servico.DCeAutorizacaoSinc)]
        [InlineData(TipoDFe.NFCom, Servico.NFComAutorizacaoSinc)]
        [InlineData(TipoDFe.NFGas, Servico.NFGasAutorizacaoSinc)]
        public void DocumentoSomenteComTelemetriaNaoExecutaSondaAtiva(TipoDFe tipoDFe, Servico servico)
        {
            var configuracao = ConfiguracaoBase();
            configuracao.TipoDFe = tipoDFe;
            configuracao.Servico = servico;
            configuracao.ColetarTelemetriaDisponibilidade = true;
            TelemetriaDisponibilidade.Registrar(configuracao, "https://sefaz.test/ws", "SOAP", 100,
                HttpStatusCode.OK, Retorno(108), null);
            var infraestrutura = new ExecutorInfraestruturaFake();
            var execucoesStatus = 0;
            var diagnostico = new DiagnosticoDisponibilidadeDFe(configuracao, null, infraestrutura,
                (configuracaoStatus, endpoint) =>
                {
                    execucoesStatus++;
                    return Status(107);
                });

            var resultado = diagnostico.ConsultarStatusServico();

            Assert.Equal(0, infraestrutura.Execucoes);
            Assert.Equal(0, execucoesStatus);
            Assert.Equal(StatusDisponibilidade.Indisponivel, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.AutoridadeFiscal, resultado.OrigemProvavel);
            Assert.Contains(resultado.Sondas.Itens, x => x.Servico == servico.ToString() && x.CStat == 108);
            Assert.Contains(resultado.Sondas.Itens, x => x.Servico == "StatusServico" &&
                x.Status == StatusDisponibilidade.NaoAplicavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ConsultaStatusNF3eClassificaParalisacaoComoIndisponibilidadeFiscal()
        {
            var agora = new DateTime(2026, 8, 19, 10, 0, 0);
            RelogioDisponibilidade.Agora = () => agora;
            using (var certificado = CriarCertificadoValido(agora))
            {
                var configuracao = ConfiguracaoBase();
                configuracao.TipoDFe = TipoDFe.NF3e;
                configuracao.CodigoUF = (int)UFBrasil.MT;
                configuracao.TipoAmbiente = TipoAmbiente.Producao;
                configuracao.SchemaVersao = "1.00";
                configuracao.Servico = Servico.NF3eStatusServico;
                configuracao.CertificadoDigital = certificado;
                Configuracao configuracaoRecebida = null;
                var diagnostico = new DiagnosticoDisponibilidadeDFe(configuracao, null,
                    new ExecutorInfraestruturaFake(), (configuracaoStatus, endpoint) =>
                    {
                        configuracaoRecebida = configuracaoStatus;
                        var status = Status(108);
                        status.Endpoint = endpoint;
                        status.Essencial = true;
                        return status;
                    });

                var resultado = diagnostico.ConsultarStatusServico();

                Assert.NotNull(configuracaoRecebida);
                Assert.Equal("1.00", configuracaoRecebida.SchemaVersao);
                Assert.Equal(StatusDisponibilidade.Indisponivel, resultado.Status);
                Assert.Equal(OrigemProvavelIndisponibilidade.AutoridadeFiscal, resultado.OrigemProvavel);
                Assert.Contains(resultado.Sondas.Itens, x =>
                    x.Fonte == FonteEvidenciaDisponibilidade.StatusServico &&
                    x.CStat == 108 &&
                    x.Status == StatusDisponibilidade.Indisponivel);
            }
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [InlineData(TipoDFe.NFSe)]
        [InlineData(TipoDFe.GNRE)]
        public void DocumentoAindaNaoSuportadoFicaNaoAplicavelSemExecutarTransporte(TipoDFe tipoDFe)
        {
            var configuracao = ConfiguracaoBase();
            configuracao.TipoDFe = tipoDFe;
            var infraestrutura = new ExecutorInfraestruturaFake();
            var execucoesStatus = 0;
            var diagnostico = new DiagnosticoDisponibilidadeDFe(configuracao, null, infraestrutura,
                (configuracaoStatus, endpoint) =>
                {
                    execucoesStatus++;
                    return Status(107);
                });

            var resultado = diagnostico.ConsultarStatusServico();

            Assert.Equal(0, infraestrutura.Execucoes);
            Assert.Equal(0, execucoesStatus);
            Assert.Equal(StatusDisponibilidade.NaoAplicavel, resultado.Status);
            Assert.Equal("Este diagnóstico não se aplica ao documento, ambiente ou local configurado.",
                resultado.Descricao);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public async Task CacheStatusUnificaChamadasConcorrentes()
        {
            var execucoes = 0;
            Func<ResultadoSondaDisponibilidade> executar = () =>
            {
                Interlocked.Increment(ref execucoes);
                Thread.Sleep(50);
                return Status(107);
            };
            var tarefas = Enumerable.Range(0, 8).Select(_ => Task.Run(() =>
                CacheStatusDisponibilidade.ObterOuExecutar("NFe|PR|H", TimeSpan.FromMinutes(5), executar),
                TestContext.Current.CancellationToken)).ToArray();

            await Task.WhenAll(tarefas);

            Assert.Equal(1, execucoes);
            Assert.Equal(7, tarefas.Count(x => x.Result.DoCache));
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void CacheStatusRespeitaIntervaloMinimoDeCincoMinutos()
        {
            var agora = new DateTime(2026, 7, 20, 10, 0, 0);
            RelogioDisponibilidade.Agora = () => agora;
            var execucoes = 0;
            Func<ResultadoSondaDisponibilidade> executar = () => { execucoes++; return Status(107); };

            CacheStatusDisponibilidade.ObterOuExecutar("NFe|PR|H", TimeSpan.FromMinutes(5), executar);
            agora = agora.AddMinutes(4);
            var cache = CacheStatusDisponibilidade.ObterOuExecutar("NFe|PR|H", TimeSpan.FromMinutes(5), executar);
            agora = agora.AddMinutes(2);
            var renovado = CacheStatusDisponibilidade.ObterOuExecutar("NFe|PR|H", TimeSpan.FromMinutes(5), executar);

            Assert.True(cache.DoCache);
            Assert.False(renovado.DoCache);
            Assert.Equal(2, execucoes);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ConsultaStatusCorrelacionaTimeoutsSemAumentarQuantidadeDeChamadas()
        {
            var agora = new DateTime(2026, 8, 5, 14, 0, 0);
            RelogioDisponibilidade.Agora = () => agora;
            using (var certificado = CriarCertificadoValido(agora))
            {
                var configuracao = ConfiguracaoBase();
                configuracao.TipoDFe = TipoDFe.NFCe;
                configuracao.CodigoUF = (int)UFBrasil.SP;
                configuracao.TipoAmbiente = TipoAmbiente.Producao;
                configuracao.CertificadoDigital = certificado;
                var execucoesStatus = 0;
                var diagnostico = new DiagnosticoDisponibilidadeDFe(configuracao, null,
                    new ExecutorInfraestruturaFake(), (configuracaoStatus, endpoint) =>
                    {
                        execucoesStatus++;
                        return new ResultadoSondaDisponibilidade
                        {
                            Servico = "StatusServico",
                            Endpoint = endpoint,
                            Protocolo = "SOAP",
                            Fonte = FonteEvidenciaDisponibilidade.StatusServico,
                            DataHora = agora,
                            Status = StatusDisponibilidade.Degradado,
                            TipoFalha = TipoFalhaDisponibilidade.Timeout,
                            Essencial = true
                        };
                    });

                var primeira = diagnostico.ConsultarStatusServico();
                agora = agora.AddMinutes(1);
                var cache = diagnostico.ConsultarStatusServico();
                agora = agora.AddMinutes(5);
                var segundaExecucao = diagnostico.ConsultarStatusServico();

                Assert.Equal(StatusDisponibilidade.Degradado, primeira.Status);
                Assert.Equal(OrigemProvavelIndisponibilidade.Indeterminada, primeira.OrigemProvavel);
                Assert.Equal("O serviço da SEFAZ não respondeu no tempo esperado. Uma nova medição é necessária para confirmar a indisponibilidade.",
                    primeira.Descricao);
                Assert.Equal(StatusDisponibilidade.Degradado, cache.Status);
                Assert.Contains(cache.Sondas.Itens,
                    x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico && x.DoCache);
                Assert.Equal(StatusDisponibilidade.Indisponivel, segundaExecucao.Status);
                Assert.Equal(OrigemProvavelIndisponibilidade.AutoridadeFiscal,
                    segundaExecucao.OrigemProvavel);
                Assert.Equal(2, segundaExecucao.Sondas.Itens.Count(
                    x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico &&
                         x.TipoFalha == TipoFalhaDisponibilidade.Timeout));
                Assert.Equal(2, execucoesStatus);
            }
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void CacheStatusCompartilhaErroNaoCatalogadoComoIndisponibilidadeFiscal()
        {
            var agora = new DateTime(2026, 7, 20, 10, 0, 0);
            RelogioDisponibilidade.Agora = () => agora;
            var execucoes = 0;

            var primeiro = CacheStatusDisponibilidade.ObterOuExecutar("NFe|PR|H", "certificado-a",
                TimeSpan.FromMinutes(5), () =>
                {
                    execucoes++;
                    return Status(999);
                });
            var segundo = CacheStatusDisponibilidade.ObterOuExecutar("NFe|PR|H", "certificado-b",
                TimeSpan.FromMinutes(5), () =>
                {
                    execucoes++;
                    return Status(107);
                });

            Assert.Equal(StatusDisponibilidade.Indisponivel, primeiro.Status);
            Assert.Equal(999, segundo.CStat);
            Assert.Equal(StatusDisponibilidade.Indisponivel, segundo.Status);
            Assert.True(segundo.DoCache);
            Assert.Equal(1, execucoes);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void CacheStatusNaoCompartilhaFalhaLocalEntreContextos()
        {
            var agora = new DateTime(2026, 7, 20, 10, 0, 0);
            RelogioDisponibilidade.Agora = () => agora;
            var execucoes = 0;
            Func<ResultadoSondaDisponibilidade> falharTls = () =>
            {
                execucoes++;
                return new ResultadoSondaDisponibilidade
                {
                    Servico = "StatusServico",
                    Endpoint = "https://sefaz.test/ws",
                    Protocolo = "SOAP",
                    Fonte = FonteEvidenciaDisponibilidade.StatusServico,
                    DataHora = agora,
                    Status = StatusDisponibilidade.Inconclusivo,
                    TipoFalha = TipoFalhaDisponibilidade.TLS,
                    Essencial = true
                };
            };

            var primeiro = CacheStatusDisponibilidade.ObterOuExecutar("NFe|PR|H", "certificado-a|proxy-a|1000",
                TimeSpan.FromMinutes(5), falharTls);
            var segundo = CacheStatusDisponibilidade.ObterOuExecutar("NFe|PR|H", "certificado-b|proxy-b|10000",
                TimeSpan.FromMinutes(5), () => { execucoes++; return Status(107); });
            agora = agora.AddMinutes(6);
            var renovado = CacheStatusDisponibilidade.ObterOuExecutar("NFe|PR|H", "certificado-b|proxy-b|10000",
                TimeSpan.FromMinutes(5), () => { execucoes++; return Status(107); });

            Assert.Equal(TipoFalhaDisponibilidade.TLS, primeiro.TipoFalha);
            Assert.Equal(TipoFalhaDisponibilidade.Protocolo, segundo.TipoFalha);
            Assert.Equal(StatusDisponibilidade.Inconclusivo, segundo.Status);
            Assert.True(segundo.DoCache);
            Assert.DoesNotContain("TLS", segundo.XMotivo, StringComparison.OrdinalIgnoreCase);
            Assert.Equal(StatusDisponibilidade.Operacional, renovado.Status);
            Assert.Equal(2, execucoes);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ConsultaStatusNaoRepassaFalhaTlsParaOutroCertificado()
        {
            var agora = new DateTime(2026, 7, 20, 10, 0, 0);
            RelogioDisponibilidade.Agora = () => agora;
            using (var certificadoA = CriarCertificadoValido(agora))
            using (var certificadoB = CriarCertificadoValido(agora))
            {
                var configuracaoA = ConfiguracaoBase();
                configuracaoA.CertificadoDigital = certificadoA;
                var configuracaoB = ConfiguracaoBase();
                configuracaoB.CertificadoDigital = certificadoB;
                var execucoesStatus = 0;
                var diagnosticoA = new DiagnosticoDisponibilidadeDFe(configuracaoA, null,
                    new ExecutorInfraestruturaFake(), (configuracaoStatus, endpoint) =>
                    {
                        execucoesStatus++;
                        return new ResultadoSondaDisponibilidade
                        {
                            Servico = "StatusServico",
                            Endpoint = endpoint,
                            Protocolo = "SOAP",
                            Fonte = FonteEvidenciaDisponibilidade.StatusServico,
                            DataHora = agora,
                            Status = StatusDisponibilidade.Inconclusivo,
                            TipoFalha = TipoFalhaDisponibilidade.TLS,
                            Essencial = true
                        };
                    });
                var diagnosticoB = new DiagnosticoDisponibilidadeDFe(configuracaoB, null,
                    new ExecutorInfraestruturaFake(), (configuracaoStatus, endpoint) =>
                    {
                        execucoesStatus++;
                        return Status(107);
                    });

                var primeiro = diagnosticoA.ConsultarStatusServico();
                var segundo = diagnosticoB.ConsultarStatusServico();

                Assert.Contains(primeiro.Sondas.Itens,
                    x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico &&
                         x.TipoFalha == TipoFalhaDisponibilidade.TLS);
                Assert.DoesNotContain(segundo.Sondas.Itens,
                    x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico &&
                         x.TipoFalha == TipoFalhaDisponibilidade.TLS);
                Assert.Contains(segundo.Sondas.Itens,
                    x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico && x.DoCache &&
                         x.Status == StatusDisponibilidade.Inconclusivo);
                Assert.Equal(1, execucoesStatus);
            }
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ConsumoIndevidoBloqueiaNovaSondaPorUmaHora()
        {
            var agora = new DateTime(2026, 7, 20, 10, 0, 0);
            RelogioDisponibilidade.Agora = () => agora;
            var execucoes = 0;
            var primeiro = CacheStatusDisponibilidade.ObterOuExecutar("CTe|PR|P", TimeSpan.FromMinutes(5), () =>
            {
                execucoes++;
                return Status(678);
            });
            agora = agora.AddMinutes(30);
            var segundo = CacheStatusDisponibilidade.ObterOuExecutar("CTe|PR|P", TimeSpan.FromMinutes(5), () =>
            {
                execucoes++;
                return Status(107);
            });

            Assert.Equal(TipoFalhaDisponibilidade.ConsumoIndevido, primeiro.TipoFalha);
            Assert.Equal(1, execucoes);
            Assert.True(segundo.DoCache);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void SanitizacaoRemoveCredenciaisConsultaEIdentificadoresLongos()
        {
            const string url = "https://usuario:senha@sefaz.test/ws?token=segredo";
            var endpoint = ClassificadorDisponibilidade.SanitizarEndpoint(url);
            var excecao = ClassificadorDisponibilidade.SanitizarExcecao(new Exception("Falha 123456789 em " + url));

            Assert.Equal("https://sefaz.test/ws", endpoint);
            Assert.DoesNotContain("senha", excecao, StringComparison.Ordinal);
            Assert.DoesNotContain("segredo", excecao, StringComparison.Ordinal);
            Assert.DoesNotContain("123456789", excecao, StringComparison.Ordinal);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void SanitizacaoRemoveIdentificadoresFormatadosCaminhosESegredos()
        {
            const string mensagem = "CNPJ 12.345.678/0001-90 CPF 123.456.789-09 senha=segredo " +
                @"arquivo C:\Clientes\Empresa Sigilosa\certificado.pfx";

            var sanitizada = ClassificadorDisponibilidade.SanitizarMensagem(mensagem);
            var endpointInvalido = ClassificadorDisponibilidade.SanitizarEndpoint(
                "usuario:senha@sefaz.test/ws#token-secreto");

            Assert.DoesNotContain("12.345.678/0001-90", sanitizada, StringComparison.Ordinal);
            Assert.DoesNotContain("123.456.789-09", sanitizada, StringComparison.Ordinal);
            Assert.DoesNotContain("segredo", sanitizada, StringComparison.OrdinalIgnoreCase);
            Assert.DoesNotContain("Empresa Sigilosa", sanitizada, StringComparison.OrdinalIgnoreCase);
            Assert.Equal(string.Empty, endpointInvalido);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ExecutorProxySanitizaMensagemAntesDeArmazenar()
        {
            const string endpoint = "https://usuario:senha@sefaz.test/ws?token=segredo";
            var configuracao = ConfiguracaoBase();
            configuracao.HasProxy = true;
            var executor = new ExecutorInfraestruturaDisponibilidade((url, certificado, timeout, proxy, metodo) =>
                new Unimake.Net.HttpConnectionResult
                {
                    ResponseReceived = false,
                    FailureType = Unimake.Net.HttpConnectionFailureType.Proxy,
                    ErrorMessage = "Falha 123456789 em " + endpoint + "\r\nconteúdo que não deve ser armazenado"
                });

            var resultados = executor.Executar(configuracao, endpoint, 10000);

            var resultado = Assert.Single(resultados);
            Assert.Equal("Proxy", resultado.Servico);
            Assert.Equal("https://sefaz.test/ws", resultado.Endpoint);
            Assert.Equal(TipoFalhaDisponibilidade.Proxy, resultado.TipoFalha);
            Assert.Equal("Falha *** em https://sefaz.test/ws", resultado.Excecao);
            Assert.DoesNotContain("usuario", resultado.Excecao, StringComparison.Ordinal);
            Assert.DoesNotContain("senha", resultado.Excecao, StringComparison.Ordinal);
            Assert.DoesNotContain("segredo", resultado.Excecao, StringComparison.Ordinal);
            Assert.DoesNotContain("123456789", resultado.Excecao, StringComparison.Ordinal);
            Assert.DoesNotContain("conteúdo", resultado.Excecao, StringComparison.Ordinal);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void AgregadorDistingueIndisponibilidadeParcial()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(new ResultadoSondaDisponibilidade { Servico = "Autorizacao", Endpoint = "https://sefaz.test/aut", Fonte = FonteEvidenciaDisponibilidade.TelemetriaPassiva, Status = StatusDisponibilidade.Operacional });
            resultado.Sondas.Add(new ResultadoSondaDisponibilidade { Servico = "Evento", Endpoint = "https://sefaz.test/evento", Fonte = FonteEvidenciaDisponibilidade.TelemetriaPassiva, Status = StatusDisponibilidade.Indisponivel });

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.ParcialmenteIndisponivel, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.Parcial, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void AgregadorExigeDuasDasUltimasTresFalhasRemotas()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(FalhaRemota(1));
            resultado.Sondas.Add(FalhaRemota(2));

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.Indisponivel, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.AutoridadeFiscal, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void TimeoutsComFalhaLocalNaoSaoAtribuidosSefaz()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(FalhaTimeout(1, true));
            resultado.Sondas.Add(FalhaTimeout(2, true));
            resultado.Sondas.Add(Infraestrutura(TipoFalhaDisponibilidade.DNS, StatusDisponibilidade.Inconclusivo));

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.Inconclusivo, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.AmbienteLocal, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void TimeoutsSemComprovacaoDaInfraestruturaFicamInconclusivos()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(FalhaTimeout(1, true));
            resultado.Sondas.Add(FalhaTimeout(2, true));

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.Inconclusivo, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.Indeterminada, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void TimeoutIsoladoComInfraestruturaSaudavelFicaDegradado()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(FalhaTimeout(1, true));
            resultado.Sondas.Add(Infraestrutura(TipoFalhaDisponibilidade.Nenhuma,
                StatusDisponibilidade.Operacional));

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.Degradado, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.Indeterminada, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void TimeoutFiscalComTimeoutTcpNoMesmoEndpointFicaDegradadoNaPrimeiraMedicao()
        {
            const string endpoint = "https://sefaz.test/ws";
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(FalhaTimeout(1, true));
            resultado.Sondas.Add(InfraestruturaEndpoint("DNS", TipoFalhaDisponibilidade.Nenhuma,
                StatusDisponibilidade.Operacional, endpoint));
            resultado.Sondas.Add(InfraestruturaEndpoint("TCP", TipoFalhaDisponibilidade.Timeout,
                StatusDisponibilidade.Inconclusivo, endpoint));

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.Degradado, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.Indeterminada, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void TimeoutsFiscaisRepetidosComTimeoutTcpNoMesmoEndpointIndicamSefaz()
        {
            const string endpoint = "https://sefaz.test/ws";
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(FalhaTimeout(1, true));
            resultado.Sondas.Add(FalhaTimeout(2, true));
            resultado.Sondas.Add(InfraestruturaEndpoint("DNS", TipoFalhaDisponibilidade.Nenhuma,
                StatusDisponibilidade.Operacional, endpoint));
            resultado.Sondas.Add(InfraestruturaEndpoint("TCP", TipoFalhaDisponibilidade.Timeout,
                StatusDisponibilidade.Inconclusivo, endpoint));

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.Indisponivel, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.AutoridadeFiscal, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void TimeoutTcpDeOutroEndpointNaoConfirmaFalhaFiscal()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(FalhaTimeout(1, true));
            resultado.Sondas.Add(FalhaTimeout(2, true));
            resultado.Sondas.Add(InfraestruturaEndpoint("DNS", TipoFalhaDisponibilidade.Nenhuma,
                StatusDisponibilidade.Operacional, "https://sefaz.test/ws"));
            resultado.Sondas.Add(InfraestruturaEndpoint("TCP", TipoFalhaDisponibilidade.Timeout,
                StatusDisponibilidade.Inconclusivo, "https://outro-endpoint.test/ws"));

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.Inconclusivo, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.Indeterminada, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ClassificadorReconheceConexaoRecusadaPeloEndpointRemoto()
        {
            var resultado = new ResultadoSondaDisponibilidade();
            var excecao = new WebException("Falha no transporte.",
                new SocketException((int)SocketError.ConnectionRefused),
                WebExceptionStatus.UnknownError, null);

            ClassificadorDisponibilidade.PreencherFalha(excecao, resultado);

            Assert.Equal(TipoFalhaDisponibilidade.ConexaoRecusada, resultado.TipoFalha);
            Assert.Equal(StatusDisponibilidade.Degradado, resultado.Status);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void TelemetriaCTeSimpClassificaHttp503ComoIndisponibilidadeDoServico()
        {
            DiagnosticoDisponibilidadeDFe.LimparMemoriaDiagnostico();
            var configuracao = ConfiguracaoBase();
            configuracao.TipoDFe = TipoDFe.CTe;
            configuracao.CodigoUF = (int)UFBrasil.MT;
            configuracao.TipoAmbiente = TipoAmbiente.Homologacao;
            configuracao.Servico = Servico.CTeAutorizacaoSimp;
            configuracao.ColetarTelemetriaDisponibilidade = true;

            var falha = new WebException(
                "The remote server returned an error: (503) Service Unavailable.",
                WebExceptionStatus.ProtocolError);
            TelemetriaDisponibilidade.Registrar(
                configuracao,
                "https://sefaz.mt.gov.br/cte/services/CTeRecepcaoSimpV4",
                "SOAP",
                766,
                HttpStatusCode.ServiceUnavailable,
                null,
                falha);

            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao).ObterDiagnosticoPassivo();
            var sonda = Assert.Single(resultado.Sondas.Itens);

            Assert.Equal("CTeAutorizacaoSimp", sonda.Servico);
            Assert.True(sonda.Essencial);
            Assert.Equal(503, sonda.HttpStatusCode);
            Assert.Equal(TipoFalhaDisponibilidade.HTTP, sonda.TipoFalha);
            Assert.Equal(StatusDisponibilidade.Indisponivel, sonda.Status);
            Assert.Equal("O endpoint fiscal informou indisponibilidade temporária do serviço (HTTP 503).",
                sonda.XMotivo);
            Assert.Equal(StatusDisponibilidade.Indisponivel, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.AutoridadeFiscal, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void Http503ConfirmadoPrevaleceSobreFalhaLocalPosterior()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(new ResultadoSondaDisponibilidade
            {
                Servico = "CTeAutorizacaoSimp",
                Endpoint = "https://sefaz.mt.gov.br/cte/services/CTeRecepcaoSimpV4",
                Fonte = FonteEvidenciaDisponibilidade.TelemetriaPassiva,
                DataHora = new DateTime(2026, 8, 22, 14, 0, 0),
                Status = StatusDisponibilidade.Indisponivel,
                TipoFalha = TipoFalhaDisponibilidade.HTTP,
                HttpStatusCode = 503,
                Essencial = true
            });
            resultado.Sondas.Add(Infraestrutura(
                TipoFalhaDisponibilidade.DNS,
                StatusDisponibilidade.Inconclusivo));

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.Indisponivel, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.AutoridadeFiscal, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ConexaoRecusadaIsoladaFicaDegradadaComDescricaoClara()
        {
            const string endpoint = "https://sefaz.test/ws";
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(FalhaConexaoRecusada(1, true));
            resultado.Sondas.Add(InfraestruturaEndpoint("DNS", TipoFalhaDisponibilidade.Nenhuma,
                StatusDisponibilidade.Operacional, endpoint));
            resultado.Sondas.Add(InfraestruturaEndpoint("TCP", TipoFalhaDisponibilidade.ConexaoRecusada,
                StatusDisponibilidade.Degradado, endpoint));

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.Degradado, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.Indeterminada, resultado.OrigemProvavel);
            Assert.Equal("O serviço da SEFAZ recusou a conexão. Uma nova medição é necessária para confirmar a indisponibilidade.",
                resultado.Descricao);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void TimeoutSeguidoDeConexaoRecusadaNoMesmoEndpointIndicaSefaz()
        {
            const string endpoint = "https://sefaz.test/ws";
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(FalhaTimeout(1, true));
            resultado.Sondas.Add(FalhaConexaoRecusada(2, true));
            resultado.Sondas.Add(InfraestruturaEndpoint("DNS", TipoFalhaDisponibilidade.Nenhuma,
                StatusDisponibilidade.Operacional, endpoint));
            resultado.Sondas.Add(InfraestruturaEndpoint("TCP", TipoFalhaDisponibilidade.Timeout,
                StatusDisponibilidade.Inconclusivo, endpoint));

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.Indisponivel, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.AutoridadeFiscal, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void TimeoutsComInfraestruturaSaudavelPodemIndicarSefaz()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(FalhaTimeout(1, true));
            resultado.Sondas.Add(FalhaTimeout(2, true));
            resultado.Sondas.Add(Infraestrutura(TipoFalhaDisponibilidade.Nenhuma, StatusDisponibilidade.Operacional));

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.Indisponivel, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.AutoridadeFiscal, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void FalhaExclusivaDeServicoNaoEssencialFicaParcial()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(FalhaHttp(1, false));
            resultado.Sondas.Add(FalhaHttp(2, false));

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.ParcialmenteIndisponivel, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.Parcial, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void IndisponibilidadeFiscalConfirmadaPrevaleceSobreFalhaLocal()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            var status = Status(108);
            status.Essencial = true;
            resultado.Sondas.Add(status);
            resultado.Sondas.Add(Infraestrutura(TipoFalhaDisponibilidade.DNS, StatusDisponibilidade.Inconclusivo));

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.Indisponivel, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.AutoridadeFiscal, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ErroNaoCatalogadoConfirmaIndisponibilidadeFiscalMesmoComFalhaLocal()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            var status = Status(999);
            status.Essencial = true;
            resultado.Sondas.Add(status);
            resultado.Sondas.Add(Infraestrutura(TipoFalhaDisponibilidade.DNS, StatusDisponibilidade.Inconclusivo));

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.Indisponivel, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.AutoridadeFiscal, resultado.OrigemProvavel);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void SucessoMaisRecenteSuperaFalhasAnteriores()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(FalhaRemota(1));
            resultado.Sondas.Add(FalhaRemota(2));
            resultado.Sondas.Add(new ResultadoSondaDisponibilidade
            {
                Servico = "NFeAutorizacao",
                Endpoint = "https://sefaz.test/ws",
                Fonte = FonteEvidenciaDisponibilidade.TelemetriaPassiva,
                DataHora = new DateTime(2026, 7, 20, 10, 0, 3),
                Status = StatusDisponibilidade.Operacional
            });

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.Operacional, resultado.Status);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ColecaoInteropMantemCountEGetItem()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(new ResultadoSondaDisponibilidade { Servico = "Autorizacao" });
            Assert.Equal(1, resultado.Sondas.Count);
            Assert.Equal("Autorizacao", resultado.Sondas.GetItem(0).Servico);
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [InlineData(StatusDisponibilidade.Operacional, OrigemProvavelIndisponibilidade.Nenhuma, "Os serviços da SEFAZ estão funcionando normalmente.")]
        [InlineData(StatusDisponibilidade.Degradado, OrigemProvavelIndisponibilidade.Nenhuma, "Os serviços da SEFAZ estão respondendo, mas apresentam lentidão ou instabilidade.")]
        [InlineData(StatusDisponibilidade.ParcialmenteIndisponivel, OrigemProvavelIndisponibilidade.Parcial, "Alguns serviços da SEFAZ estão indisponíveis, enquanto outros continuam funcionando.")]
        [InlineData(StatusDisponibilidade.Indisponivel, OrigemProvavelIndisponibilidade.AutoridadeFiscal, "Há indícios de indisponibilidade nos serviços da SEFAZ. Tente novamente mais tarde.")]
        [InlineData(StatusDisponibilidade.Inconclusivo, OrigemProvavelIndisponibilidade.Indeterminada, "Ainda não há informações suficientes para determinar se os serviços da SEFAZ estão disponíveis.")]
        [InlineData(StatusDisponibilidade.NaoAplicavel, OrigemProvavelIndisponibilidade.Indeterminada, "Este diagnóstico não se aplica ao documento, ambiente ou local configurado.")]
        [InlineData(StatusDisponibilidade.Degradado, OrigemProvavelIndisponibilidade.ConsumoIndevido, "A SEFAZ limitou temporariamente as consultas por excesso de consumo. Aguarde antes de tentar novamente.")]
        public void DescricaoTraduzResultadoParaUsuario(StatusDisponibilidade status, OrigemProvavelIndisponibilidade origem, string descricao)
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade
            {
                Status = status,
                OrigemProvavel = origem
            };

            Assert.Equal(descricao, resultado.Descricao);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void DescricaoOperacionalLimitaConclusaoQuandoSomenteStatusFoiObservado()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade
            {
                Status = StatusDisponibilidade.Operacional,
                OrigemProvavel = OrigemProvavelIndisponibilidade.Nenhuma
            };
            resultado.Sondas.Add(new ResultadoSondaDisponibilidade
            {
                Servico = "DNS",
                Fonte = FonteEvidenciaDisponibilidade.Infraestrutura,
                Status = StatusDisponibilidade.Operacional
            });
            resultado.Sondas.Add(new ResultadoSondaDisponibilidade
            {
                Servico = "StatusServico",
                Fonte = FonteEvidenciaDisponibilidade.StatusServico,
                Status = StatusDisponibilidade.Operacional,
                CStat = 107
            });

            Assert.Equal(
                "A consulta de status da SEFAZ está funcionando normalmente, mas a autorização e os demais serviços ainda não foram observados.",
                resultado.Descricao);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void AutorizacaoIndisponivelPrevaleceComoFalhaParcialMesmoComStatusServicoOperacional()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(new ResultadoSondaDisponibilidade
            {
                Servico = "StatusServico",
                Endpoint = "https://sefaz.test/status",
                Fonte = FonteEvidenciaDisponibilidade.StatusServico,
                Status = StatusDisponibilidade.Operacional,
                CStat = 107,
                Essencial = true
            });
            resultado.Sondas.Add(new ResultadoSondaDisponibilidade
            {
                Servico = "NFeAutorizacao",
                Endpoint = "https://sefaz.test/autorizacao",
                Fonte = FonteEvidenciaDisponibilidade.TelemetriaPassiva,
                Status = StatusDisponibilidade.Indisponivel,
                CStat = 108,
                Essencial = true
            });

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.ParcialmenteIndisponivel, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.Parcial, resultado.OrigemProvavel);
            Assert.Equal(
                "Alguns serviços da SEFAZ estão indisponíveis, enquanto outros continuam funcionando.",
                resultado.Descricao);
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [InlineData(TipoFalhaDisponibilidade.DNS, "Há indícios de um problema na conexão deste computador com a SEFAZ. Verifique a internet e a rede local.")]
        [InlineData(TipoFalhaDisponibilidade.Certificado, "Não foi possível acessar a SEFAZ porque o certificado digital não está disponível ou precisa ser revisado.")]
        [InlineData(TipoFalhaDisponibilidade.Configuracao, "Não foi possível verificar a SEFAZ porque a configuração do sistema precisa ser revisada.")]
        public void DescricaoDetalhaProblemaNoAmbienteLocal(TipoFalhaDisponibilidade falha, string descricao)
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade
            {
                Status = StatusDisponibilidade.Inconclusivo,
                OrigemProvavel = OrigemProvavelIndisponibilidade.AmbienteLocal
            };
            resultado.Sondas.Add(new ResultadoSondaDisponibilidade { TipoFalha = falha });

            Assert.Equal(descricao, resultado.Descricao);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void AgregadorIdentificaConfiguracaoComoProblemaLocal()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(new ResultadoSondaDisponibilidade
            {
                Servico = "Configuracao",
                Fonte = FonteEvidenciaDisponibilidade.Infraestrutura,
                Status = StatusDisponibilidade.Inconclusivo,
                TipoFalha = TipoFalhaDisponibilidade.Configuracao,
                Essencial = true
            });

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.Inconclusivo, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.AmbienteLocal, resultado.OrigemProvavel);
            Assert.Equal("Não foi possível verificar a SEFAZ porque a configuração do sistema precisa ser revisada.",
                resultado.Descricao);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void AgregadorMantemNaoAplicavelQuandoNenhumaSondaSeAplica()
        {
            var resultado = new ResultadoDiagnosticoDisponibilidade();
            resultado.Sondas.Add(new ResultadoSondaDisponibilidade
            {
                Servico = "StatusServico",
                Fonte = FonteEvidenciaDisponibilidade.Infraestrutura,
                Status = StatusDisponibilidade.NaoAplicavel,
                TipoFalha = TipoFalhaDisponibilidade.Nenhuma
            });
            resultado.Sondas.Add(new ResultadoSondaDisponibilidade
            {
                Servico = "Infraestrutura",
                Fonte = FonteEvidenciaDisponibilidade.Infraestrutura,
                Status = StatusDisponibilidade.NaoAplicavel,
                TipoFalha = TipoFalhaDisponibilidade.Nenhuma
            });

            AgregadorDisponibilidade.Agregar(resultado);

            Assert.Equal(StatusDisponibilidade.NaoAplicavel, resultado.Status);
            Assert.Equal(OrigemProvavelIndisponibilidade.Indeterminada, resultado.OrigemProvavel);
            Assert.Equal("Este diagnóstico não se aplica ao documento, ambiente ou local configurado.",
                resultado.Descricao);
        }

        [Theory(Skip = "Integração explícita: consulta somente StatusServico em homologação com certificado real.")]
        [Trait("Utility", "DisponibilidadeIntegracao")]
        [InlineData(TipoDFe.NFe, UFBrasil.PR, "4.00")]
        [InlineData(TipoDFe.NFCe, UFBrasil.PR, "4.00")]
        [InlineData(TipoDFe.CTe, UFBrasil.PR, "4.00")]
        [InlineData(TipoDFe.MDFe, UFBrasil.PR, "3.00")]
        public void SmokeTestStatusHomologacao(TipoDFe tipoDFe, UFBrasil uf, string versao)
        {
            var configuracao = ConfiguracaoBase();
            configuracao.TipoDFe = tipoDFe;
            configuracao.CodigoUF = (int)uf;
            configuracao.SchemaVersao = versao;
            configuracao.CertificadoDigital = PropConfig.CertificadoDigital;
            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao).ConsultarStatusServico();
            Assert.Contains(resultado.Sondas.Itens, x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico);
        }

        [Fact(Explicit = true, Timeout = 60000)]
        [Trait("Utility", "DisponibilidadeIntegracao")]
        public void ConsultaStatusNFCeSPProducao()
        {
            // Este teste é explícito porque acessa um serviço real. Ele não participa da suíte normal
            // e nunca envia NFCe, evento, inutilização ou qualquer outro XML com efeito fiscal.
            DiagnosticoDisponibilidadeDFe.LimparMemoriaDiagnostico();
            var configuracao = ConfiguracaoBase();
            configuracao.TipoDFe = TipoDFe.NFCe;
            configuracao.CodigoUF = (int)UFBrasil.SP;
            configuracao.TipoAmbiente = TipoAmbiente.Producao;
            configuracao.SchemaVersao = "4.00";
            configuracao.Servico = Servico.NFeStatusServico;
            configuracao.CertificadoDigital = PropConfig.CertificadoDigital;

            var opcoes = new ConfiguracaoDiagnosticoDisponibilidade
            {
                TimeoutMilissegundos = 10000,
                LimiteLentidaoMilissegundos = 3000
            };
            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao, opcoes).ConsultarStatusServico();

            var saida = TestContext.Current.TestOutputHelper;
            if (saida != null)
            {
                saida.WriteLine("Diagnóstico NFCe/SP em produção: {0} / {1}", resultado.Status,
                    resultado.OrigemProvavel);
                saida.WriteLine("Descrição: {0}", resultado.Descricao);
                foreach (var sonda in resultado.Sondas.Itens)
                {
                    saida.WriteLine(
                        "{0:O} | {1} | {2} | falha={3} | HTTP={4} | cStat={5} | {6} ms | cache={7} | endpoint={8} | motivo={9} | exceção={10}",
                        sonda.DataHora, sonda.Servico, sonda.Status, sonda.TipoFalha,
                        sonda.HttpStatusCode, sonda.CStat, sonda.DuracaoMilissegundos,
                        sonda.DoCache, sonda.Endpoint, sonda.XMotivo, sonda.Excecao);
                }
            }

            Assert.Equal(TipoDFe.NFCe, resultado.TipoDFe);
            Assert.Equal(UFBrasil.SP, resultado.UFBrasil);
            Assert.Equal(TipoAmbiente.Producao, resultado.TipoAmbiente);
            Assert.Contains(resultado.Sondas.Itens,
                x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico &&
                     x.Servico == "StatusServico");
        }

        [Fact(Explicit = true, Timeout = 60000)]
        [Trait("Utility", "DisponibilidadeIntegracao")]
        public void ConsultaStatusNFeMTProducao()
        {
            // Este teste é explícito porque acessa um serviço real. Ele não participa da suíte normal
            // e nunca envia NFe, evento, inutilização ou qualquer outro XML com efeito fiscal.
            DiagnosticoDisponibilidadeDFe.LimparMemoriaDiagnostico();
            var configuracao = ConfiguracaoBase();
            configuracao.TipoDFe = TipoDFe.NFe;
            configuracao.CodigoUF = (int)UFBrasil.MT;
            configuracao.TipoAmbiente = TipoAmbiente.Producao;
            configuracao.SchemaVersao = "4.00";
            configuracao.Servico = Servico.NFeStatusServico;
            configuracao.CertificadoDigital = PropConfig.CertificadoDigital;

            var opcoes = new ConfiguracaoDiagnosticoDisponibilidade
            {
                TimeoutMilissegundos = 10000,
                LimiteLentidaoMilissegundos = 3000
            };
            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao, opcoes).ConsultarStatusServico();

            var saida = TestContext.Current.TestOutputHelper;
            if (saida != null)
            {
                saida.WriteLine("Diagnóstico NFe/MT em produção: {0} / {1}", resultado.Status,
                    resultado.OrigemProvavel);
                saida.WriteLine("Descrição: {0}", resultado.Descricao);
                foreach (var sonda in resultado.Sondas.Itens)
                {
                    saida.WriteLine(
                        "{0:O} | {1} | {2} | falha={3} | HTTP={4} | cStat={5} | {6} ms | cache={7} | endpoint={8} | motivo={9} | exceção={10}",
                        sonda.DataHora, sonda.Servico, sonda.Status, sonda.TipoFalha,
                        sonda.HttpStatusCode, sonda.CStat, sonda.DuracaoMilissegundos,
                        sonda.DoCache, sonda.Endpoint, sonda.XMotivo, sonda.Excecao);
                }
            }

            Assert.Equal(TipoDFe.NFe, resultado.TipoDFe);
            Assert.Equal(UFBrasil.MT, resultado.UFBrasil);
            Assert.Equal(TipoAmbiente.Producao, resultado.TipoAmbiente);
            Assert.Contains(resultado.Sondas.Itens,
                x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico &&
                     x.Servico == "StatusServico");
        }

        [Fact(Explicit = true)]
        [Trait("Utility", "DisponibilidadeIntegracao")]
        public void ConsultaStatusNFeMTHomologacao()
        {
            // Este teste é explícito porque acessa um serviço real. Ele não participa da suíte normal
            // e nunca envia NFe, evento, inutilização ou qualquer outro XML com efeito fiscal.
            DiagnosticoDisponibilidadeDFe.LimparMemoriaDiagnostico();
            var configuracao = ConfiguracaoBase();
            configuracao.TipoDFe = TipoDFe.NFe;
            configuracao.CodigoUF = (int)UFBrasil.MT;
            configuracao.TipoAmbiente = TipoAmbiente.Homologacao;
            configuracao.SchemaVersao = "4.00";
            configuracao.Servico = Servico.NFeStatusServico;
            configuracao.CertificadoDigital = PropConfig.CertificadoDigital;

            var opcoes = new ConfiguracaoDiagnosticoDisponibilidade
            {
                TimeoutMilissegundos = 10000,
                LimiteLentidaoMilissegundos = 3000
            };
            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao, opcoes).ConsultarStatusServico();

            var saida = TestContext.Current.TestOutputHelper;
            if (saida != null)
            {
                saida.WriteLine("Diagnóstico NFe/MT em homologação: {0} / {1}", resultado.Status,
                    resultado.OrigemProvavel);
                saida.WriteLine("Descrição: {0}", resultado.Descricao);
                foreach (var sonda in resultado.Sondas.Itens)
                {
                    saida.WriteLine(
                        "{0:O} | {1} | {2} | falha={3} | HTTP={4} | cStat={5} | {6} ms | cache={7} | endpoint={8} | motivo={9} | exceção={10}",
                        sonda.DataHora, sonda.Servico, sonda.Status, sonda.TipoFalha,
                        sonda.HttpStatusCode, sonda.CStat, sonda.DuracaoMilissegundos,
                        sonda.DoCache, sonda.Endpoint, sonda.XMotivo, sonda.Excecao);
                }
            }

            Assert.Equal(TipoDFe.NFe, resultado.TipoDFe);
            Assert.Equal(UFBrasil.MT, resultado.UFBrasil);
            Assert.Equal(TipoAmbiente.Homologacao, resultado.TipoAmbiente);
            Assert.Contains(resultado.Sondas.Itens,
                x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico &&
                     x.Servico == "StatusServico");
        }

        [Fact(Explicit = true)]
        [Trait("Utility", "DisponibilidadeIntegracao")]
        public void ConsultaStatusNF3eMTProducao()
        {
            // Este teste acessa somente a consulta oficial de StatusServico da NF3e. Ele não envia
            // documento, evento ou qualquer outra mensagem capaz de produzir efeito fiscal.
            DiagnosticoDisponibilidadeDFe.LimparMemoriaDiagnostico();
            var configuracao = ConfiguracaoBase();
            configuracao.TipoDFe = TipoDFe.NF3e;
            configuracao.CodigoUF = (int)UFBrasil.MT;
            configuracao.TipoAmbiente = TipoAmbiente.Producao;
            configuracao.SchemaVersao = "1.00";
            configuracao.Servico = Servico.NF3eStatusServico;
            configuracao.CertificadoDigital = PropConfig.CertificadoDigital;

            var opcoes = new ConfiguracaoDiagnosticoDisponibilidade
            {
                TimeoutMilissegundos = 10000,
                LimiteLentidaoMilissegundos = 3000
            };
            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao, opcoes).ConsultarStatusServico();

            var saida = TestContext.Current.TestOutputHelper;
            if (saida != null)
            {
                saida.WriteLine("Diagnóstico NF3e/MT em produção: {0} / {1}", resultado.Status,
                    resultado.OrigemProvavel);
                saida.WriteLine("Descrição: {0}", resultado.Descricao);
                foreach (var sonda in resultado.Sondas.Itens)
                {
                    saida.WriteLine(
                        "{0:O} | {1} | {2} | falha={3} | HTTP={4} | cStat={5} | {6} ms | cache={7} | endpoint={8} | motivo={9} | exceção={10}",
                        sonda.DataHora, sonda.Servico, sonda.Status, sonda.TipoFalha,
                        sonda.HttpStatusCode, sonda.CStat, sonda.DuracaoMilissegundos,
                        sonda.DoCache, sonda.Endpoint, sonda.XMotivo, sonda.Excecao);
                }
            }

            Assert.Equal(TipoDFe.NF3e, resultado.TipoDFe);
            Assert.Equal(UFBrasil.MT, resultado.UFBrasil);
            Assert.Equal(TipoAmbiente.Producao, resultado.TipoAmbiente);
            Assert.Contains(resultado.Sondas.Itens,
                x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico &&
                     x.Servico == "StatusServico");
        }

        [Fact(Explicit = true)]
        [Trait("Utility", "DisponibilidadeIntegracao")]
        public void ConsultaStatusNFeAMProducao()
        {
            // Este teste é explícito porque acessa o serviço real da SEFAZ AM. A única mensagem
            // transmitida é a consulta oficial de StatusServico, sem efeito fiscal.
            var resultado = ConsultarStatusNFeAM(TipoAmbiente.Producao);

            Assert.Equal(TipoDFe.NFe, resultado.TipoDFe);
            Assert.Equal(UFBrasil.AM, resultado.UFBrasil);
            Assert.Equal(TipoAmbiente.Producao, resultado.TipoAmbiente);
            Assert.Contains(resultado.Sondas.Itens,
                x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico &&
                     x.Servico == "StatusServico");
        }

        [Fact(Explicit = true)]
        [Trait("Utility", "DisponibilidadeIntegracao")]
        public void ConsultaStatusNFeAMHomologacao()
        {
            // Produção e homologação possuem hosts próprios no Amazonas. Este teste mantém as
            // evidências separadas para mostrar se a indisponibilidade afeta um ou os dois ambientes.
            var resultado = ConsultarStatusNFeAM(TipoAmbiente.Homologacao);

            Assert.Equal(TipoDFe.NFe, resultado.TipoDFe);
            Assert.Equal(UFBrasil.AM, resultado.UFBrasil);
            Assert.Equal(TipoAmbiente.Homologacao, resultado.TipoAmbiente);
            Assert.Contains(resultado.Sondas.Itens,
                x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico &&
                     x.Servico == "StatusServico");
        }

        private static ResultadoDiagnosticoDisponibilidade ConsultarStatusNFeAM(TipoAmbiente ambiente)
        {
            DiagnosticoDisponibilidadeDFe.LimparMemoriaDiagnostico();
            var configuracao = ConfiguracaoBase();
            configuracao.TipoDFe = TipoDFe.NFe;
            configuracao.CodigoUF = (int)UFBrasil.AM;
            configuracao.TipoAmbiente = ambiente;
            configuracao.SchemaVersao = "4.00";
            configuracao.Servico = Servico.NFeStatusServico;
            configuracao.CertificadoDigital = PropConfig.CertificadoDigital;

            var opcoes = new ConfiguracaoDiagnosticoDisponibilidade
            {
                TimeoutMilissegundos = 10000,
                LimiteLentidaoMilissegundos = 3000
            };
            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao, opcoes).ConsultarStatusServico();

            var saida = TestContext.Current.TestOutputHelper;
            if (saida != null)
            {
                saida.WriteLine("Diagnóstico NFe/AM em {0}: {1} / {2}", ambiente, resultado.Status,
                    resultado.OrigemProvavel);
                saida.WriteLine("Descrição: {0}", resultado.Descricao);
                foreach (var sonda in resultado.Sondas.Itens)
                {
                    saida.WriteLine(
                        "{0:O} | {1} | {2} | falha={3} | HTTP={4} | cStat={5} | {6} ms | cache={7} | endpoint={8} | motivo={9} | exceção={10}",
                        sonda.DataHora, sonda.Servico, sonda.Status, sonda.TipoFalha,
                        sonda.HttpStatusCode, sonda.CStat, sonda.DuracaoMilissegundos,
                        sonda.DoCache, sonda.Endpoint, sonda.XMotivo, sonda.Excecao);
                }
            }

            return resultado;
        }

        private static Configuracao ConfiguracaoBase() => new Configuracao
        {
            TipoDFe = TipoDFe.NFe,
            TipoEmissao = TipoEmissao.Normal,
            CodigoUF = (int)UFBrasil.PR,
            TipoAmbiente = TipoAmbiente.Homologacao,
            SchemaVersao = "4.00",
            Servico = Servico.NFeAutorizacao
        };

        public static IEnumerable<object[]> RetornosFiscaisComCStat => new[]
        {
            new object[]
            {
                TipoDFe.NFe,
                Servico.NFeInutilizacao,
                "<retInutNFe xmlns='http://www.portalfiscal.inf.br/nfe'><infInut><cStat>213</cStat><xMotivo>Rejeição</xMotivo></infInut></retInutNFe>",
                213,
                StatusDisponibilidade.Operacional
            },
            new object[]
            {
                TipoDFe.NFCe,
                Servico.NFeInutilizacao,
                "<retInutNFe xmlns='http://www.portalfiscal.inf.br/nfe'><infInut><cStat>213</cStat><xMotivo>Rejeição</xMotivo></infInut></retInutNFe>",
                213,
                StatusDisponibilidade.Operacional
            },
            new object[]
            {
                TipoDFe.NFe,
                Servico.NFeConsultaCadastro,
                "<retConsCad xmlns='http://www.portalfiscal.inf.br/nfe'><infCons><cStat>111</cStat><xMotivo>Consulta processada</xMotivo></infCons></retConsCad>",
                111,
                StatusDisponibilidade.Operacional
            },
            new object[]
            {
                TipoDFe.CTe,
                Servico.NFeConsultaCadastro,
                "<retConsCad xmlns='http://www.portalfiscal.inf.br/nfe'><infCons><cStat>111</cStat><xMotivo>Consulta processada</xMotivo></infCons></retConsCad>",
                111,
                StatusDisponibilidade.Operacional
            },
            new object[]
            {
                TipoDFe.CTe,
                Servico.NFeRecepcaoEvento,
                "<retEventoCTe xmlns='http://www.portalfiscal.inf.br/cte'><infEvento><cStat>135</cStat><xMotivo>Evento registrado</xMotivo></infEvento></retEventoCTe>",
                135,
                StatusDisponibilidade.Operacional
            },
            new object[]
            {
                TipoDFe.MDFe,
                Servico.NFeRecepcaoEvento,
                "<retEventoMDFe xmlns='http://www.portalfiscal.inf.br/mdfe'><infEvento><cStat>999</cStat><xMotivo>Erro não catalogado</xMotivo></infEvento></retEventoMDFe>",
                999,
                StatusDisponibilidade.Indisponivel
            },
            new object[]
            {
                TipoDFe.NFe,
                Servico.NFeAutorizacao,
                "<retEnviNFe xmlns='http://www.portalfiscal.inf.br/nfe'><cStat>104</cStat><xMotivo>Lote processado</xMotivo><protNFe><infProt><cStat>999</cStat><xMotivo>Resultado do protocolo</xMotivo></infProt></protNFe></retEnviNFe>",
                104,
                StatusDisponibilidade.Operacional
            },
            new object[]
            {
                TipoDFe.NFCe,
                Servico.NFeAutorizacao,
                "<retEnviNFe xmlns='http://www.portalfiscal.inf.br/nfe'><cStat>104</cStat><xMotivo>Lote processado</xMotivo><protNFe><infProt><cStat>999</cStat><xMotivo>Resultado do protocolo</xMotivo></infProt></protNFe></retEnviNFe>",
                104,
                StatusDisponibilidade.Operacional
            },
            new object[]
            {
                TipoDFe.CTe,
                Servico.CTeAutorizacaoSinc,
                "<retCTe xmlns='http://www.portalfiscal.inf.br/cte'><cStat>104</cStat><xMotivo>Lote processado</xMotivo><protCTe><infProt><cStat>999</cStat><xMotivo>Resultado do protocolo</xMotivo></infProt></protCTe></retCTe>",
                104,
                StatusDisponibilidade.Operacional
            },
            new object[]
            {
                TipoDFe.MDFe,
                Servico.MDFeAutorizacaoSinc,
                "<retMDFe xmlns='http://www.portalfiscal.inf.br/mdfe'><cStat>104</cStat><xMotivo>Lote processado</xMotivo><protMDFe><infProt><cStat>999</cStat><xMotivo>Resultado do protocolo</xMotivo></infProt></protMDFe></retMDFe>",
                104,
                StatusDisponibilidade.Operacional
            },
            new object[]
            {
                TipoDFe.NF3e,
                Servico.NF3eStatusServico,
                "<retConsStatServNF3e xmlns='http://www.portalfiscal.inf.br/nf3e'><tpAmb>1</tpAmb><verAplic>1.00</verAplic><cStat>108</cStat><xMotivo>Servico Paralisado Momentaneamente (curto prazo)</xMotivo><cUF>51</cUF></retConsStatServNF3e>",
                108,
                StatusDisponibilidade.Indisponivel
            },
            new object[]
            {
                TipoDFe.BPe,
                Servico.BPeStatusServico,
                "<retConsStatServBPe xmlns='http://www.portalfiscal.inf.br/bpe'><cStat>107</cStat><xMotivo>Serviço em operação</xMotivo></retConsStatServBPe>",
                107,
                StatusDisponibilidade.Operacional
            },
            new object[]
            {
                TipoDFe.CTeOS,
                Servico.CTeStatusServico,
                "<retConsStatServCte xmlns='http://www.portalfiscal.inf.br/cte'><cStat>109</cStat><xMotivo>Serviço paralisado sem previsão</xMotivo></retConsStatServCte>",
                109,
                StatusDisponibilidade.Indisponivel
            },
            new object[]
            {
                TipoDFe.DCe,
                Servico.DCeStatusServico,
                "<retConsStatServDCe xmlns='http://www.portalfiscal.inf.br/dce'><cStat>107</cStat><xMotivo>Serviço em operação</xMotivo></retConsStatServDCe>",
                107,
                StatusDisponibilidade.Operacional
            },
            new object[]
            {
                TipoDFe.NFCom,
                Servico.NFComStatusServico,
                "<retConsStatServNFCom xmlns='http://www.portalfiscal.inf.br/nfcom'><cStat>999</cStat><xMotivo>Erro não catalogado</xMotivo></retConsStatServNFCom>",
                999,
                StatusDisponibilidade.Indisponivel
            },
            new object[]
            {
                TipoDFe.NFGas,
                Servico.NFGasStatusServico,
                "<retConsStatServNFGas xmlns='http://www.portalfiscal.inf.br/nfgas'><cStat>107</cStat><xMotivo>Serviço em operação</xMotivo></retConsStatServNFGas>",
                107,
                StatusDisponibilidade.Operacional
            }
        };

        public static IEnumerable<object[]> ServicosMonitorados => new[]
        {
            new object[] { TipoDFe.BPe, Servico.BPeStatusServico },
            new object[] { TipoDFe.BPe, Servico.BPeConsultaProtocolo },
            new object[] { TipoDFe.BPe, Servico.BPeRecepcaoEvento },
            new object[] { TipoDFe.BPe, Servico.BPeAutorizacao },
            new object[] { TipoDFe.BPe, Servico.BPeTMAutorizacao },
            new object[] { TipoDFe.BPe, Servico.BPeTAAutorizacao },
            new object[] { TipoDFe.CTe, Servico.CTeStatusServico },
            new object[] { TipoDFe.CTe, Servico.CTeConsultaProtocolo },
            new object[] { TipoDFe.CTe, Servico.NFeRecepcaoEvento },
            new object[] { TipoDFe.CTe, Servico.CTeDistribuicaoDFe },
            new object[] { TipoDFe.CTe, Servico.NFeConsultaCadastro },
            new object[] { TipoDFe.CTe, Servico.CTeAutorizacaoSinc },
            new object[] { TipoDFe.CTe, Servico.CTeAutorizacaoSimp },
            new object[] { TipoDFe.CTeOS, Servico.CTeStatusServico },
            new object[] { TipoDFe.CTeOS, Servico.CTeConsultaProtocolo },
            new object[] { TipoDFe.CTeOS, Servico.NFeRecepcaoEvento },
            new object[] { TipoDFe.CTeOS, Servico.NFeConsultaCadastro },
            new object[] { TipoDFe.CTeOS, Servico.CTeAutorizacaoOS },
            new object[] { TipoDFe.DCe, Servico.DCeStatusServico },
            new object[] { TipoDFe.DCe, Servico.DCeConsultaProtocolo },
            new object[] { TipoDFe.DCe, Servico.DCeRecepcaoEvento },
            new object[] { TipoDFe.DCe, Servico.DCeAutorizacaoSinc },
            new object[] { TipoDFe.MDFe, Servico.MDFeStatusServico },
            new object[] { TipoDFe.MDFe, Servico.MDFeConsultaProtocolo },
            new object[] { TipoDFe.MDFe, Servico.NFeRecepcaoEvento },
            new object[] { TipoDFe.MDFe, Servico.MDFeConsultaNaoEnc },
            new object[] { TipoDFe.MDFe, Servico.MDFeAutorizacaoSinc },
            new object[] { TipoDFe.NF3e, Servico.NF3eStatusServico },
            new object[] { TipoDFe.NF3e, Servico.NF3eConsultaProtocolo },
            new object[] { TipoDFe.NF3e, Servico.NF3eConsultaRecibo },
            new object[] { TipoDFe.NF3e, Servico.NF3eRecepcaoEvento },
            new object[] { TipoDFe.NF3e, Servico.NF3eAutorizacaoSinc },
            new object[] { TipoDFe.NFCe, Servico.NFeStatusServico },
            new object[] { TipoDFe.NFCe, Servico.NFeConsultaProtocolo },
            new object[] { TipoDFe.NFCe, Servico.NFeConsultaRecibo },
            new object[] { TipoDFe.NFCe, Servico.NFeInutilizacao },
            new object[] { TipoDFe.NFCe, Servico.NFeConsultaCadastro },
            new object[] { TipoDFe.NFCe, Servico.NFeRecepcaoEvento },
            new object[] { TipoDFe.NFCe, Servico.NFeAutorizacao },
            new object[] { TipoDFe.NFCe, Servico.NFCeDownloadXML },
            new object[] { TipoDFe.NFCe, Servico.NFCeConsultaChaves },
            new object[] { TipoDFe.NFCom, Servico.NFComStatusServico },
            new object[] { TipoDFe.NFCom, Servico.NFComConsultaProtocolo },
            new object[] { TipoDFe.NFCom, Servico.NFComRecepcaoEvento },
            new object[] { TipoDFe.NFCom, Servico.NFComAutorizacaoSinc },
            new object[] { TipoDFe.NFe, Servico.NFeStatusServico },
            new object[] { TipoDFe.NFe, Servico.NFeConsultaProtocolo },
            new object[] { TipoDFe.NFe, Servico.NFeConsultaRecibo },
            new object[] { TipoDFe.NFe, Servico.NFeInutilizacao },
            new object[] { TipoDFe.NFe, Servico.NFeConsultaCadastro },
            new object[] { TipoDFe.NFe, Servico.NFeRecepcaoEvento },
            new object[] { TipoDFe.NFe, Servico.NFeAutorizacao },
            new object[] { TipoDFe.NFe, Servico.NFeDistribuicaoDFe },
            new object[] { TipoDFe.NFGas, Servico.NFGasStatusServico },
            new object[] { TipoDFe.NFGas, Servico.NFGasConsultaProtocolo },
            new object[] { TipoDFe.NFGas, Servico.NFGasRecepcaoEvento },
            new object[] { TipoDFe.NFGas, Servico.NFGasAutorizacaoSinc }
        };

        private static async Task ComHistoricoBloqueado(Func<Task> executar)
        {
            var syncRoot = typeof(TelemetriaDisponibilidade)
                .GetField("SyncRoot", BindingFlags.Static | BindingFlags.NonPublic)
                .GetValue(null);
            var token = TestContext.Current.CancellationToken;
            var iniciou = new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously);
            using (var liberar = new ManualResetEventSlim())
            {
                var bloqueio = Task.Run(() =>
                {
                    var lockAdquirido = false;
                    try
                    {
                        Monitor.Enter(syncRoot, ref lockAdquirido);
                        iniciou.TrySetResult(true);
                        liberar.Wait(token);
                    }
                    finally
                    {
                        if (lockAdquirido)
                        {
                            Monitor.Exit(syncRoot);
                        }
                    }
                }, token);

                await iniciou.Task.WaitAsync(TimeSpan.FromSeconds(5), token);
                try
                {
                    await executar();
                }
                finally
                {
                    liberar.Set();
                    await bloqueio;
                }
            }
        }

        private static XmlDocument Retorno(int cStat)
        {
            var xml = new XmlDocument();
            xml.LoadXml("<retorno><cStat>" + cStat + "</cStat><xMotivo>conteúdo sigiloso 123456789</xMotivo></retorno>");
            return xml;
        }

        private static ResultadoSondaDisponibilidade Status(int cStat)
        {
            var resultado = new ResultadoSondaDisponibilidade
            {
                Servico = "StatusServico",
                Fonte = FonteEvidenciaDisponibilidade.StatusServico,
                DataHora = RelogioDisponibilidade.Agora(),
                CStat = cStat
            };
            ClassificadorDisponibilidade.ClassificarRespostaFiscal(resultado);
            return resultado;
        }

        private static X509Certificate2 CriarCertificadoInvalido(int tipoCertificado, DateTime agora)
        {
            if (tipoCertificado == 0)
            {
                return null;
            }

            if (tipoCertificado == 1)
            {
                using (var completo = CriarCertificadoValido(agora))
                {
                    return X509Certificate2.CreateFromPem(completo.ExportCertificatePem());
                }
            }

            using (var rsa = RSA.Create())
            {
                var requisicao = new CertificateRequest("CN=DiagnosticoDisponibilidadeTest", rsa,
                    HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                return requisicao.CreateSelfSigned(new DateTimeOffset(agora.AddDays(-2)),
                    new DateTimeOffset(agora.AddDays(-1)));
            }
        }

        private static X509Certificate2 CriarCertificadoValido(DateTime agora)
        {
            using (var rsa = RSA.Create())
            {
                var requisicao = new CertificateRequest("CN=DiagnosticoDisponibilidadeTest", rsa,
                    HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                return requisicao.CreateSelfSigned(new DateTimeOffset(agora.AddDays(-1)),
                    new DateTimeOffset(agora.AddDays(1)));
            }
        }

        private static ResultadoSondaDisponibilidade FalhaRemota(int segundo) => FalhaHttp(segundo, true);

        private static ResultadoSondaDisponibilidade FalhaHttp(int segundo, bool essencial) => new ResultadoSondaDisponibilidade
        {
            Servico = "NFeAutorizacao",
            Endpoint = "https://sefaz.test/ws",
            Fonte = FonteEvidenciaDisponibilidade.TelemetriaPassiva,
            DataHora = new DateTime(2026, 7, 20, 10, 0, segundo),
            Status = StatusDisponibilidade.Degradado,
            TipoFalha = TipoFalhaDisponibilidade.HTTP,
            HttpStatusCode = 503,
            Essencial = essencial
        };

        private static ResultadoSondaDisponibilidade FalhaTimeout(int segundo, bool essencial) => new ResultadoSondaDisponibilidade
        {
            Servico = "NFeAutorizacao",
            Endpoint = "https://sefaz.test/ws",
            Fonte = FonteEvidenciaDisponibilidade.TelemetriaPassiva,
            DataHora = new DateTime(2026, 7, 20, 10, 0, segundo),
            Status = StatusDisponibilidade.Degradado,
            TipoFalha = TipoFalhaDisponibilidade.Timeout,
            Essencial = essencial
        };

        private static ResultadoSondaDisponibilidade FalhaConexaoRecusada(int segundo, bool essencial) =>
            new ResultadoSondaDisponibilidade
            {
                Servico = "NFeAutorizacao",
                Endpoint = "https://sefaz.test/ws",
                Fonte = FonteEvidenciaDisponibilidade.TelemetriaPassiva,
                DataHora = new DateTime(2026, 7, 20, 10, 0, segundo),
                Status = StatusDisponibilidade.Degradado,
                TipoFalha = TipoFalhaDisponibilidade.ConexaoRecusada,
                Essencial = essencial
            };

        private static ResultadoSondaDisponibilidade Infraestrutura(TipoFalhaDisponibilidade falha,
            StatusDisponibilidade status) => new ResultadoSondaDisponibilidade
        {
            Servico = falha == TipoFalhaDisponibilidade.DNS ? "DNS" : "TCP",
            Fonte = FonteEvidenciaDisponibilidade.Infraestrutura,
            DataHora = new DateTime(2026, 7, 20, 10, 0, 3),
            Status = status,
            TipoFalha = falha,
            Essencial = true
        };

        private static ResultadoSondaDisponibilidade InfraestruturaEndpoint(string servico,
            TipoFalhaDisponibilidade falha, StatusDisponibilidade status, string endpoint) =>
            new ResultadoSondaDisponibilidade
            {
                Servico = servico,
                Endpoint = endpoint,
                Fonte = FonteEvidenciaDisponibilidade.Infraestrutura,
                DataHora = new DateTime(2026, 7, 20, 10, 0, 3),
                Status = status,
                TipoFalha = falha,
                Essencial = true
            };

        private sealed class ExecutorInfraestruturaFake : IExecutorInfraestruturaDisponibilidade
        {
            public int Execucoes { get; private set; }

            public IList<ResultadoSondaDisponibilidade> Executar(Configuracao configuracao, string endpoint, int timeoutMilissegundos)
            {
                Execucoes++;
                return new List<ResultadoSondaDisponibilidade>
                {
                    new ResultadoSondaDisponibilidade
                    {
                        Servico = "TCP",
                        Endpoint = endpoint,
                        Protocolo = "TCP",
                        Fonte = FonteEvidenciaDisponibilidade.Infraestrutura,
                        DataHora = RelogioDisponibilidade.Agora(),
                        Status = StatusDisponibilidade.Operacional,
                        TipoFalha = TipoFalhaDisponibilidade.Nenhuma
                    }
                };
            }
        }
    }
}
