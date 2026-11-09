using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Security.Authentication;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Lookup;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.Utility.Rede
{
    [Collection("MemoriaDiagnosticoDisponibilidade")]
    public class CoberturaDisponibilidadeTest
    {
        public static IEnumerable<object[]> Respostas()
        {
            foreach (var codigo in new[] { 201, 401, 301 })
                yield return new object[] { TipoDFe.ESocial, Servico.ESocialEnviarLoteEventos,
                    "<eSocial xmlns='http://www.esocial.gov.br/schema/lote/eventos/envio/retornoEnvio/v1_1_0'><retornoEnvioLoteEventos><status><cdResposta>" + codigo + "</cdResposta><descResposta>CLIENTE TESTE segredo</descResposta></status><retornoEventos><status><cdResposta>301</cdResposta></status></retornoEventos></retornoEnvioLoteEventos></eSocial>",
                    codigo == 301 ? StatusDisponibilidade.Degradado : StatusDisponibilidade.Operacional, codigo, "eSocial.cdResposta" };
            foreach (var codigo in new[] { 1, 2, 3, 7, 8, 9, 99 })
                yield return new object[] { TipoDFe.EFDReinf, Servico.EFDReinfConsultaLoteAssincrono,
                    "<Reinf xmlns='http://www.reinf.esocial.gov.br/schemas/retornoLoteEventosAssincrono/v1_00_00'><retornoLoteEventosAssincrono><status><cdResposta>" + codigo + "</cdResposta></status></retornoLoteEventosAssincrono></Reinf>",
                    codigo == 99 ? StatusDisponibilidade.Degradado : StatusDisponibilidade.Operacional, codigo, "Reinf.cdResposta" };
            yield return new object[] { TipoDFe.EFDReinf, Servico.EFDReinfConsultaReciboEvento,
                "<Reinf xmlns='http://www.reinf.esocial.gov.br/schemas/retornoRecibosChaveEvento/v1_05_01'><ideStatus><cdRetorno>0</cdRetorno></ideStatus></Reinf>", StatusDisponibilidade.Operacional, 0, "Reinf.cdRetorno" };
            yield return new object[] { TipoDFe.GNRE, Servico.GNRELoteRecepcao,
                "<TRetLote_GNRE xmlns='http://www.gnre.pe.gov.br'><situacaoRecepcao><codigo>100</codigo></situacaoRecepcao></TRetLote_GNRE>", StatusDisponibilidade.Operacional, 100, "GNRE.codigo" };
            yield return new object[] { TipoDFe.PIX, Servico.PIXCobrancaCriar,
                "<PIXCobrancaCreateResponse><Status>0</Status><Motivo>segredo</Motivo></PIXCobrancaCreateResponse>", StatusDisponibilidade.Operacional, 0, "Unimake.Status" };
            yield return new object[] { TipoDFe.EBoleto, Servico.EBoletoCancelar,
                "<BoletoCancelarResponse><Status>999</Status><Motivo>segredo</Motivo></BoletoCancelarResponse>", StatusDisponibilidade.Degradado, 999, "Unimake.Status" };
            yield return new object[] { TipoDFe.DARE, Servico.DAREEnvio,
                "<DareRetorno xmlns='https://portal.fazenda.sp.gov.br/servicos/dare'><DARE><numeroControleDarePrincipal>123</numeroControleDarePrincipal></DARE></DareRetorno>", StatusDisponibilidade.Operacional, 0, "DARE.retorno" };
            yield return new object[] { TipoDFe.CIOT, Servico.CIOTCancelamentoOperacaoTransporte,
                new Unimake.Business.DFe.Xml.CIOT.RetCancelamentoOperacaoTransporte { Codigo = "110", Mensagem = "segredo" }.GerarXML().OuterXml,
                StatusDisponibilidade.Operacional, 110, "CIOT.Codigo" };
            yield return new object[] { TipoDFe.CIOT, Servico.CIOTConsultarCIOTGerado,
                new Unimake.Business.DFe.Xml.CIOT.RetConsultarCIOTGerado { Codigo = new List<string> { "EF123", "EF456" }, Mensagem = new List<string> { "segredo" } }.GerarXML().OuterXml,
                StatusDisponibilidade.Inconclusivo, 0, "CIOT.Codigo" };
            yield return new object[] { TipoDFe.UMessenger, Servico.UMessengerPublish,
                "<uMessengerResponse><Mensagem><Status>0</Status></Mensagem><Mensagem><Status>500</Status></Mensagem></uMessengerResponse>", StatusDisponibilidade.Degradado, 0, "uMessenger.Status" };
        }

        [Theory]
        [MemberData(nameof(Respostas))]
        [Trait("Utility", "Disponibilidade")]
        public void ConsumidorRealReconheceContratoSemRequestExtra(TipoDFe tipo, Servico servico, string xml,
            StatusDisponibilidade esperado, int codigo, string contrato)
        {
            var config = Config(tipo, servico);
            var item = ExecutarApi(config, xml);
            Assert.True(item.RespostaAplicacaoReconhecida);
            Assert.Equal(esperado, item.Status);
            Assert.Equal(0, item.CStat);
            Assert.Equal(codigo, item.CodigoRespostaAplicacao);
            Assert.Equal(contrato, item.ContratoResposta);
            Assert.DoesNotContain("segredo", string.Join("|", typeof(ObservacaoDisponibilidade).GetProperties().Select(p => p.GetValue(item))));
        }

        [Theory]
        [InlineData("<html>OK</html>")]
        [InlineData("<Resposta><Status>1</Status></Resposta>")]
        [InlineData("<PIXCobrancaCreateResponse><Status>abc</Status></PIXCobrancaCreateResponse>")]
        [InlineData("<PIXCobrancaCreateResponse><Status>-1</Status></PIXCobrancaCreateResponse>")]
        [Trait("Utility", "Disponibilidade")]
        public void Http200ComContratoDesconhecidoOuMalformadoEhInconclusivo(string xml)
        {
            var item = ExecutarApi(Config(TipoDFe.PIX, Servico.PIXCobrancaCriar), xml);
            Assert.False(item.RespostaAplicacaoReconhecida);
            Assert.Equal(StatusDisponibilidade.Inconclusivo, item.Status);
            Assert.Contains("não foi reconhecido", item.Limitacao);
        }

        [Theory]
        [InlineData(401, TipoFalhaDisponibilidade.Autenticacao)]
        [InlineData(403, TipoFalhaDisponibilidade.Autenticacao)]
        [InlineData(407, TipoFalhaDisponibilidade.Proxy)]
        [InlineData(429, TipoFalhaDisponibilidade.ConsumoIndevido)]
        [InlineData(503, TipoFalhaDisponibilidade.HTTP)]
        [Trait("Utility", "Disponibilidade")]
        public void HttpDeFalhaPrevaleceMesmoComContratoReconhecido(int http, TipoFalhaDisponibilidade esperado)
        {
            var item = ExecutarApi(Config(TipoDFe.PIX, Servico.PIXCobrancaCriar),
                "<PIXCobrancaCreateResponse><Status>0</Status></PIXCobrancaCreateResponse>", (HttpStatusCode)http);
            Assert.Equal(esperado, item.TipoFalha);
            Assert.NotEqual(StatusDisponibilidade.Operacional, item.Status);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void RejeicaoReinf404ComContratoValidoProvaRespostaDaConsulta()
        {
            var item = ExecutarApi(Config(TipoDFe.EFDReinf, Servico.EFDReinfConsultaLoteAssincrono),
                "<Reinf xmlns='http://www.reinf.esocial.gov.br/schemas/retornoLoteEventosAssincrono/v1_00_00'><retornoLoteEventosAssincrono><status><cdResposta>9</cdResposta></status></retornoLoteEventosAssincrono></Reinf>", HttpStatusCode.NotFound);
            Assert.Equal(StatusDisponibilidade.Operacional, item.Status);
            Assert.Equal(404, item.HttpStatusCode);
        }

        [Theory]
        [InlineData(PadraoNFSe.GINFES, "<ConsultarNfseResposta><ListaMensagemRetorno><MensagemRetorno><Codigo>E160</Codigo><Mensagem>segredo</Mensagem></MensagemRetorno></ListaMensagemRetorno></ConsultarNfseResposta>", 200)]
        [InlineData(PadraoNFSe.NACIONAL, "<NFSe xmlns='http://www.sped.fazenda.gov.br/nfse'><infNFSe Id='NFS123'><nNFSe>123</nNFSe></infNFSe></NFSe>", 200)]
        [InlineData(PadraoNFSe.NACIONAL, "<temp><tipoAmbiente>2</tipoAmbiente><erros><Codigo>E0014</Codigo><Descricao>segredo</Descricao></erros></temp>", 422)]
        [Trait("Utility", "Disponibilidade")]
        public void NfseReconheceContratoMunicipalENacionalSemCStat(PadraoNFSe padrao, string xml, int http)
        {
            var config = Config(TipoDFe.NFSe, Servico.NFSeConsultarNfse);
            config.PadraoNFSe = padrao;
            config.WebTagRetorno = "ConsultarNfseResult"; // wrapper SOAP difere da raiz de negócio
            var item = ExecutarApi(config, xml, (HttpStatusCode)http);
            Assert.Equal(StatusDisponibilidade.Operacional, item.Status);
            Assert.True(item.RespostaAplicacaoReconhecida);
            Assert.Equal(0, item.CStat);
            Assert.Contains("operação", item.Limitacao);
        }

        [Theory]
        [InlineData("<soap:Fault xmlns:soap='http://schemas.xmlsoap.org/soap/envelope/'><detail><cStat>107</cStat></detail></soap:Fault>", 200, StatusDisponibilidade.Inconclusivo)]
        [InlineData("<ret><cStat>107</cStat></ret>", 503, StatusDisponibilidade.Indisponivel)]
        [Trait("Utility", "Disponibilidade")]
        public void FaultOuHttp503NaoViramSucessoPorCodigoInterno(string xml, int http, StatusDisponibilidade esperado)
        {
            var item = ExecutarApi(Config(TipoDFe.NFe, Servico.NFeStatusServico), xml, (HttpStatusCode)http);
            Assert.Equal(esperado, item.Status);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void CapacidadeDistingueSessaoPassivaStatusEHistoricoSemCarregarCertificado()
        {
            foreach (TipoDFe tipo in Enum.GetValues(typeof(TipoDFe)))
            {
                var capacidade = new CapacidadesDiagnosticoDisponibilidade(tipo);
                Assert.Equal(DiagnosticoDisponibilidadeDFe.PossuiConsultaOficialStatus(tipo), capacidade.ConsultaOficialStatus);
                Assert.False(string.IsNullOrEmpty(capacidade.Limitacao));
            }
            var reinf = new CapacidadesDiagnosticoDisponibilidade(TipoDFe.EFDReinf);
            Assert.True(reinf.ObservacaoPassiva);
            Assert.False(reinf.ConsultaOficialStatus);
            Assert.False(reinf.HistoricoLegado);
            Assert.False(new CapacidadesDiagnosticoDisponibilidade(TipoDFe.SNCM).ObservacaoPassiva);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void HistoricoRemovePathECorpoSemMisturarEndpointsDoMesmoHost()
        {
            DiagnosticoDisponibilidadeDFe.LimparMemoriaDiagnostico();
            var config = Config(TipoDFe.NFe, Servico.NFeAutorizacao);
            var retorno = new XmlDocument(); retorno.LoadXml("<ret><cStat>108</cStat></ret>");
            TelemetriaDisponibilidade.Registrar(config, "https://sefaz.test/token-secreto/12345678901", "SOAP", 10, HttpStatusCode.OK, retorno, null);
            retorno.LoadXml("<ret><cStat>107</cStat></ret>");
            TelemetriaDisponibilidade.Registrar(config, "https://sefaz.test/outro-servico", "SOAP", 10, HttpStatusCode.OK, retorno, null);
            var resultado = new DiagnosticoDisponibilidadeDFe(config).ObterDiagnosticoPassivo();
            Assert.Equal(2, resultado.Sondas.Count);
            Assert.Equal(StatusDisponibilidade.ParcialmenteIndisponivel, resultado.Status);
            Assert.All(resultado.Sondas.Itens, s => Assert.Equal("https://sefaz.test", s.Endpoint));
            var falha = new ResultadoSondaDisponibilidade();
            ClassificadorDisponibilidade.PreencherFalha(new Exception("<xml>SEGREDO e token sem separador</xml>"), falha);
            Assert.DoesNotContain("SEGREDO", falha.Excecao);
            Assert.DoesNotContain("<xml>", falha.Excecao);
            Assert.NotEqual(CacheInfraestruturaDisponibilidade.CriarChaveCache(config, "https://sefaz.test/um", 1000),
                CacheInfraestruturaDisponibilidade.CriarChaveCache(config, "https://sefaz.test/dois", 1000));
        }

        [Theory]
        [InlineData(495)]
        [InlineData(496)]
        [Trait("Utility", "Disponibilidade")]
        public void ReinfIdentificaRecusaDeCertificadoSemAplicarRegraAOutrasApis(int http)
        {
            var reinf = ExecutarApi(Config(TipoDFe.EFDReinf, Servico.EFDReinfConsultaLoteAssincrono), "<erro>segredo</erro>", (HttpStatusCode)http);
            var pix = ExecutarApi(Config(TipoDFe.PIX, Servico.PIXCobrancaCriar), "<erro>segredo</erro>", (HttpStatusCode)http);
            Assert.Equal(TipoFalhaDisponibilidade.Certificado, reinf.TipoFalha);
            Assert.Equal(TipoFalhaDisponibilidade.HTTP, pix.TipoFalha);
            Assert.Equal(StatusDisponibilidade.Inconclusivo, reinf.Status);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void FalhaAuxiliarPreservaCausaMesmoQuandoLegadoSubstituiExcecao()
        {
            var canal = new CanalObservacoesDisponibilidade();
            using (var sessao = canal.IniciarOperacao(Config(TipoDFe.NFSe, Servico.NFSeGerarNfse), Guid.NewGuid(), Guid.NewGuid()))
            {
                SessaoObservacaoDisponibilidade.InformarTransporte("https://auth.test/token-secreto", "REST", "Autenticacao");
                SessaoObservacaoDisponibilidade.InformarFalhaAuxiliar(new WebException("XML privado", WebExceptionStatus.NameResolutionFailure));
                sessao.Falhar(new NullReferenceException("exceção legada"));
            }
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal(TipoFalhaDisponibilidade.DNS, item.TipoFalha);
            Assert.Equal("Autenticacao", item.Etapa);
            Assert.Equal(1, item.EntradasTransporte);
            Assert.Equal(StatusDisponibilidade.Inconclusivo, item.Status);
            Assert.False(canal.TentarLer(out _));
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ConsumidorSoapPreservaExcecaoTlsEContaUmaEntrada()
        {
            var falha = new HttpRequestException("segredo", new AuthenticationException("certificado privado"));
            var executor = new SoapFake(falha);
            var canal = new CanalObservacoesDisponibilidade();
            using (SoapTransportExecutorFactory.Override(() => executor))
            using (var sessao = canal.IniciarOperacao(Config(TipoDFe.NFe, Servico.NFeStatusServico), Guid.NewGuid(), Guid.NewGuid()))
            using (var consumidor = new ConsumirWS())
            {
                var xml = new XmlDocument(); xml.LoadXml("<consulta/>");
                var recebida = Assert.Throws<HttpRequestException>(() => consumidor.ExecutarServico(xml,
                    new WSSoap { EnderecoWeb = "https://soap.test/segredo", SoapString = "<Envelope>{xmlBody}</Envelope>",
                        TagRetorno = "ret", UsaCertificadoDigital = false, UseProxy = false }, null));
                Assert.Same(falha, recebida);
                sessao.Falhar(recebida);
            }
            Assert.Equal(1, executor.Chamadas);
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal(TipoFalhaDisponibilidade.TLS, item.TipoFalha);
            Assert.Equal(1, item.EntradasTransporte);
            Assert.Equal("https://soap.test", item.Destino);
        }

        [Theory]
        [InlineData(TipoDFe.NFCom, 107, 200, StatusDisponibilidade.Operacional)]
        [InlineData(TipoDFe.NFGas, 107, 200, StatusDisponibilidade.Operacional)]
        [InlineData(TipoDFe.BPe, 107, 200, StatusDisponibilidade.Operacional)]
        [InlineData(TipoDFe.DCe, 107, 200, StatusDisponibilidade.Operacional)]
        [InlineData(TipoDFe.CTeOS, 107, 200, StatusDisponibilidade.Operacional)]
        [InlineData(TipoDFe.NFCom, 107, 503, StatusDisponibilidade.Indisponivel)]
        [InlineData(TipoDFe.NFCom, 0, 200, StatusDisponibilidade.Inconclusivo)]
        [InlineData(TipoDFe.NFCom, -1, 200, StatusDisponibilidade.Inconclusivo)]
        [Trait("Utility", "Disponibilidade")]
        public void StatusOficialAtravessaExecutorRealComCacheSemRede(TipoDFe tipo, int codigo, int http, StatusDisponibilidade esperado)
        {
            DiagnosticoDisponibilidadeDFe.LimparMemoriaDiagnostico();
            using var rsa = RSA.Create(2048);
            var request = new CertificateRequest("CN=TESTE OFFLINE", rsa, System.Security.Cryptography.HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
            using var certificado = request.CreateSelfSigned(DateTimeOffset.Now.AddDays(-1), DateTimeOffset.Now.AddDays(1));
            var config = Config(tipo, Servico.NFeStatusServico);
            config.CertificadoDigital = certificado;
            config.TipoAmbiente = TipoAmbiente.Homologacao;
            var executor = new SoapFake(null, codigo, (HttpStatusCode)http);
            using (SoapTransportExecutorFactory.Override(() => executor))
            {
                var diagnostico = new DiagnosticoDisponibilidadeDFe(config, null, new InfraVazia(), null);
                var resultado = diagnostico.VerificarStatusServico();
                Assert.True(resultado.Sondas.Itens.Any(x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico && x.CStat == Math.Max(codigo, 0)),
                    string.Join(" | ", resultado.Sondas.Itens.Select(x => x.TipoFalha + ": " + x.XMotivo + " / " + x.Excecao)));
                Assert.Equal(esperado, Assert.Single(resultado.Sondas.Itens, x => x.Fonte == FonteEvidenciaDisponibilidade.StatusServico).Status);
                diagnostico.VerificarStatusServico();
                Assert.Equal(1, executor.Chamadas);
                Assert.NotNull(executor.Enviado);
                Assert.Contains("consStatServ", executor.Enviado);
            }
        }

        [Theory]
        [InlineData(1, "{\"status\":1,\"nome\":\"CLIENTE TESTE\"}", StatusDisponibilidade.Operacional)]
        [InlineData(0, "{\"status\":0,\"erro\":\"segredo\"}", StatusDisponibilidade.Operacional)]
        [InlineData(0, "{\"status\":\"error\",\"code\":\"INPUT\",\"message\":\"segredo\"}", StatusDisponibilidade.Operacional)]
        [InlineData(0, "<html>segredo</html>", StatusDisponibilidade.Inconclusivo)]
        [Trait("Utility", "Disponibilidade")]
        public async Task LookupPublicoObservaSemExporDocumentoTokenOuCorpo(int codigo, string corpo, StatusDisponibilidade esperado)
        {
            var handler = new LookupHandler(corpo);
            using var client = new HttpClient(handler);
            using var lookup = new CpfCnpjComBrLookup("token-secreto", client);
            var canal = new CanalObservacoesDisponibilidade();
            using (var sessao = canal.IniciarOperacao(Config(TipoDFe.Desconhecido, (Servico)(-1)), Guid.NewGuid(), Guid.NewGuid()))
            {
                try
                {
                    await lookup.ConsultarCpfAsync("12345678901", false, TestContext.Current.CancellationToken);
                    sessao.Concluir(null, 0);
                }
                catch (Exception ex) { sessao.Falhar(ex); }
            }
            Assert.Equal(1, handler.Chamadas);
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal("CpfCnpjComBr", item.IntegracaoRemota);
            Assert.Equal(TipoDFe.Desconhecido, item.TipoDFe);
            Assert.Equal(esperado, item.Status);
            Assert.Equal(codigo, item.CodigoRespostaAplicacao);
            Assert.DoesNotContain("token", item.Destino);
            Assert.DoesNotContain("12345678901", item.Destino);
        }

        private static Configuracao Config(TipoDFe tipo, Servico servico) => new Configuracao
        {
            TipoDFe = tipo, Servico = servico, CodigoUF = 35, TipoAmbiente = TipoAmbiente.Homologacao,
            ColetarTelemetriaDisponibilidade = true
        };

        private static ObservacaoDisponibilidade ExecutarApi(Configuracao config, string xml, HttpStatusCode http = HttpStatusCode.OK)
        {
            var executor = new ApiFake(xml, http);
            var canal = new CanalObservacoesDisponibilidade();
            using (ApiTransportExecutorFactory.Override(() => executor))
            using (var sessao = canal.IniciarOperacao(config, Guid.NewGuid(), Guid.NewGuid()))
            using (var consumidor = new ConsumirAPI())
            {
                try
                {
                    consumidor.ExecutarServico(new APIConfig { RequestURI = "https://api.test/segredo?token=secreto", MetodoAPI = "get",
                        TagRetorno = "prop:innertext", UsaCertificadoDigital = false, UseProxy = false }, null);
                    sessao.Concluir(consumidor.RetornoServicoXML, consumidor.HttpStatusCode);
                }
                catch (Exception ex) { sessao.Falhar(ex); }
            }
            Assert.Equal(1, executor.Chamadas);
            Assert.True(canal.TentarLer(out var item));
            Assert.False(canal.TentarLer(out _));
            Assert.Equal(1, item.EntradasTransporte);
            return item;
        }

        private sealed class ApiFake(string xml, HttpStatusCode http) : IApiTransportExecutor
        {
            internal int Chamadas;
            public TransportResponse Execute(TransportRequest request)
            {
                Chamadas++;
                return new TransportResponse { StatusCode = http, HttpResponseMessage = new HttpResponseMessage(http)
                    { Content = new StringContent(xml, Encoding.UTF8, "application/xml") } };
            }
        }

        private sealed class SoapFake(Exception falha = null, int codigo = 107, HttpStatusCode http = HttpStatusCode.OK) : ISoapTransportExecutor
        {
            internal int Chamadas;
            internal string Enviado;
            public TransportResponse Execute(TransportRequest request)
            {
                Chamadas++;
                Enviado = request.Body;
                if (falha != null) throw falha;
                // A tag configurada é obtida do request no teste de cada família.
                var entrada = new XmlDocument(); entrada.LoadXml(request.Body);
                var consulta = entrada.SelectSingleNode("//*[starts-with(local-name(),'consStatServ')]");
                var retorno = consulta.LocalName.Replace("cons", "retCons");
                var wrapper = consulta.NamespaceURI.EndsWith("/cte") ? "cteStatusServicoCTResult" :
                    consulta.NamespaceURI.EndsWith("/nfcom") ? "nfcomResultMsg" :
                    consulta.NamespaceURI.EndsWith("/nfgas") ? "nfgasResultMsg" :
                    consulta.NamespaceURI.EndsWith("/bpe") ? "bpeResultMsg" : "dceResultMsg";
                return new TransportResponse { StatusCode = http,
                    Content = codigo == -1 ? "<html>segredo</html>" : "<soap:Envelope xmlns:soap='http://schemas.xmlsoap.org/soap/envelope/'><soap:Body><" + wrapper + "><" + retorno + " xmlns='" + consulta.NamespaceURI + "'><cStat>" + codigo + "</cStat></" + retorno + "></" + wrapper + "></soap:Body></soap:Envelope>" };
            }
        }

        private sealed class InfraVazia : IExecutorInfraestruturaDisponibilidade
        {
            public IList<ResultadoSondaDisponibilidade> Executar(Configuracao configuracao, string endpoint, int timeoutMilissegundos)
                => new List<ResultadoSondaDisponibilidade>();
        }

        private sealed class LookupHandler(string corpo) : HttpMessageHandler
        {
            internal int Chamadas;
            protected override Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
            {
                Chamadas++;
                return Task.FromResult(new HttpResponseMessage(HttpStatusCode.OK) { Content = new StringContent(corpo) });
            }
        }
    }
}
