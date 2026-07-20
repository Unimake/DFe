using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
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
        [InlineData(107, StatusDisponibilidade.Operacional, TipoFalhaDisponibilidade.Nenhuma)]
        [InlineData(108, StatusDisponibilidade.Indisponivel, TipoFalhaDisponibilidade.Protocolo)]
        [InlineData(109, StatusDisponibilidade.Indisponivel, TipoFalhaDisponibilidade.Protocolo)]
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
        public void QuatroDocumentosCarregamEndpointSemSondaFiscal(TipoDFe tipoDFe, string versao)
        {
            var configuracao = ConfiguracaoBase();
            configuracao.TipoDFe = tipoDFe;
            configuracao.SchemaVersao = versao;
            var executor = new ExecutorInfraestruturaFake();

            var resultado = new DiagnosticoDisponibilidadeDFe(configuracao, null, executor).Executar();

            Assert.Equal(1, executor.Execucoes);
            Assert.DoesNotContain(resultado.Sondas.Itens, x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico);
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

        private static Configuracao ConfiguracaoBase() => new Configuracao
        {
            TipoDFe = TipoDFe.NFe,
            TipoEmissao = TipoEmissao.Normal,
            CodigoUF = (int)UFBrasil.PR,
            TipoAmbiente = TipoAmbiente.Homologacao,
            SchemaVersao = "4.00",
            Servico = Servico.NFeAutorizacao
        };

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
