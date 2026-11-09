using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text;
using Unimake.Business.DFe;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.Utility.Rede
{
    [Collection("MemoriaDiagnosticoDisponibilidade")]
    public class InterpretacaoContratosDisponibilidadeTest
    {
        public static IEnumerable<object[]> Gnre()
        {
            foreach (var codigo in new[] { 100, 103, 153, 155, 156, 157, 199, 301, 302, 303 })
                yield return new object[] { Servico.GNRELoteRecepcao, "TRetLote_GNRE", "situacaoRecepcao", codigo, StatusDisponibilidade.Operacional, TipoFalhaDisponibilidade.Nenhuma };
            foreach (var codigo in new[] { 400, 401, 402, 403, 501, 502, 503, 504, 601, 602 })
                yield return new object[] { Servico.GNREConsultaResultadoLote, "TResultLote_GNRE", "situacaoProcess", codigo, StatusDisponibilidade.Operacional, TipoFalhaDisponibilidade.Nenhuma };
            foreach (var codigo in new[] { 450, 451, 452, 453, 454, 455 })
                yield return new object[] { Servico.GNREConsultaConfigUF, "TConfigUf", "situacaoConsulta", codigo, StatusDisponibilidade.Operacional, TipoFalhaDisponibilidade.Nenhuma };
            yield return new object[] { Servico.GNREConsultaResultadoLote, "TRetLote_GNRE", "situacaoRecepcao", 100, StatusDisponibilidade.Operacional, TipoFalhaDisponibilidade.Nenhuma };
            foreach (var codigo in new[] { 197, 404, 604 })
                yield return new object[] { Servico.GNREConsultaResultadoLote, "TResultLote_GNRE", "situacaoProcess", codigo, StatusDisponibilidade.Degradado, TipoFalhaDisponibilidade.Protocolo };
            foreach (var codigo in new[] { 101, 102, 154, 198, 999 })
                yield return new object[] { Servico.GNRELoteRecepcao, "TRetLote_GNRE", "situacaoRecepcao", codigo, StatusDisponibilidade.Inconclusivo,
                    codigo == 101 ? TipoFalhaDisponibilidade.Certificado : codigo == 102 || codigo == 154 ? TipoFalhaDisponibilidade.Autenticacao : codigo == 198 ? TipoFalhaDisponibilidade.Configuracao : TipoFalhaDisponibilidade.Protocolo };
            yield return new object[] { Servico.GNREConsultaResultadoLote, "TResultLote_GNRE", "situacaoProcess", 603, StatusDisponibilidade.Inconclusivo, TipoFalhaDisponibilidade.Autenticacao };
        }

        [Theory, MemberData(nameof(Gnre)), Trait("Utility", "Disponibilidade")]
        public void GnreDistingueProcessamentoRejeicaoFalhaTecnicaEAcesso(Servico servico, string raiz, string grupo,
            int codigo, StatusDisponibilidade status, TipoFalhaDisponibilidade falha)
        {
            var xml = "<" + raiz + " xmlns='http://www.gnre.pe.gov.br'><" + grupo + "><codigo>" + codigo + "</codigo><descricao>SEGREDO</descricao></" + grupo + "><resultado><codigo>703</codigo></resultado></" + raiz + ">";
            var item = Executar(Config(TipoDFe.GNRE, servico), xml);
            Assert.True(item.RespostaAplicacaoReconhecida);
            Assert.Equal(codigo, item.CodigoRespostaAplicacao);
            Assert.Equal(status, item.Status);
            Assert.Equal(falha, item.TipoFalha);
        }

        [Theory]
        [InlineData(Servico.GNRELoteRecepcao, "<TResultLote_GNRE xmlns='http://www.gnre.pe.gov.br'><situacaoProcess><codigo>400</codigo></situacaoProcess></TResultLote_GNRE>")]
        [InlineData(Servico.GNRELoteRecepcao, "<TRetLote_GNRE xmlns='urn:estranho'><situacaoRecepcao><codigo>100</codigo></situacaoRecepcao></TRetLote_GNRE>")]
        [InlineData(Servico.GNRELoteRecepcao, "<TRetLote_GNRE xmlns='http://www.gnre.pe.gov.br'><situacaoRecepcao><codigo>abc</codigo></situacaoRecepcao><cStat>107</cStat></TRetLote_GNRE>")]
        [Trait("Utility", "Disponibilidade")]
        public void GnreNaoEmprestaContratoDeOutroServico(Servico servico, string xml)
        {
            Assert.Equal(StatusDisponibilidade.Inconclusivo, Executar(Config(TipoDFe.GNRE, servico), xml).Status);
        }

        public static IEnumerable<object[]> NfseMunicipal()
        {
            foreach (var padrao in new[] { PadraoNFSe.ABASE, PadraoNFSe.AVMB, PadraoNFSe.WEBISS, PadraoNFSe.PUBLICA })
                yield return new object[] { padrao, Servico.NFSeRecepcionarLoteRps, "<EnviarLoteRpsResposta xmlns='" + (padrao == PadraoNFSe.ABASE ? "http://nfse.abase.com.br/nfse.xsd" : padrao == PadraoNFSe.PUBLICA ? "http://www.publica.inf.br" : "http://www.abrasf.org.br/nfse.xsd") + "'><NumeroLote>42</NumeroLote><DataRecebimento>2026-11-09T10:00:00</DataRecebimento><Protocolo>PROTO-TESTE</Protocolo></EnviarLoteRpsResposta>", StatusDisponibilidade.Operacional };
            yield return new object[] { PadraoNFSe.GINFES, Servico.NFSeRecepcionarLoteRps, "<EnviarLoteRpsResposta xmlns='http://www.ginfes.com.br/servico_enviar_lote_rps_resposta_v03.xsd'><NumeroLote>42</NumeroLote><DataRecebimento>2026-11-09T10:00:00</DataRecebimento><Protocolo>PROTO-TESTE</Protocolo></EnviarLoteRpsResposta>", StatusDisponibilidade.Operacional };
            foreach (var codigo in new[] { "E1", "E2", "E3", "E6", "E8", "E9", "E999" })
                yield return new object[] { PadraoNFSe.PUBLICA, Servico.NFSeConsultarNfse, MunicipalErro(codigo), codigo == "E999" ? StatusDisponibilidade.Inconclusivo : StatusDisponibilidade.Operacional };
            yield return new object[] { PadraoNFSe.GINFES, Servico.NFSeConsultarNfse, MunicipalErro("E160"), StatusDisponibilidade.Operacional };
            yield return new object[] { PadraoNFSe.GINFES, Servico.NFSeConsultarNfse, MunicipalErro("E1"), StatusDisponibilidade.Inconclusivo };
            yield return new object[] { PadraoNFSe.WEBISS, Servico.NFSeConsultarNfseServicoPrestado, "<ConsultarNfseServicoPrestadoResposta xmlns='http://www.abrasf.org.br/nfse.xsd'><ListaNfse/></ConsultarNfseServicoPrestadoResposta>", StatusDisponibilidade.Operacional };
            foreach (var situacao in new[] { "1", "2", "3", "4", "99", "abc" })
                yield return new object[] { PadraoNFSe.WEBISS, Servico.NFSeConsultarLoteRps, "<ConsultarLoteRpsResposta xmlns='http://www.abrasf.org.br/nfse.xsd'><Situacao>" + situacao + "</Situacao></ConsultarLoteRpsResposta>", situacao == "99" || situacao == "abc" ? StatusDisponibilidade.Inconclusivo : StatusDisponibilidade.Operacional };
            foreach (var sucesso in new[] { "true", "false", "inválido" })
                yield return new object[] { PadraoNFSe.PAULISTANA, Servico.NFSeConsultarNfse, "<RetornoConsulta xmlns='http://www.prefeitura.sp.gov.br/nfe'><Cabecalho><Sucesso>" + sucesso + "</Sucesso></Cabecalho></RetornoConsulta>", sucesso == "true" ? StatusDisponibilidade.Operacional : StatusDisponibilidade.Inconclusivo };
        }

        private static string MunicipalErro(string codigo) => "<ConsultarNfseResposta><ListaMensagemRetorno><MensagemRetorno><Codigo>" + codigo + "</Codigo><Mensagem>SEGREDO</Mensagem></MensagemRetorno></ListaMensagemRetorno></ConsultarNfseResposta>";

        [Theory, MemberData(nameof(NfseMunicipal)), Trait("Utility", "Disponibilidade")]
        public void NfseRespeitaPadraoServicoEContrato(PadraoNFSe padrao, Servico servico, string xml, StatusDisponibilidade status)
        {
            var config = Config(TipoDFe.NFSe, servico); config.PadraoNFSe = padrao;
            Assert.Equal(status, Executar(config, xml).Status);
        }

        [Theory]
        [InlineData("<EnviarLoteRpsResposta xmlns='http://www.abrasf.org.br/nfse.xsd'><NumeroLote>42</NumeroLote></EnviarLoteRpsResposta>")]
        [InlineData("<EnviarLoteRpsResposta xmlns='urn:falso'><Protocolo>SEGREDO</Protocolo></EnviarLoteRpsResposta>")]
        [InlineData("<ConsultarNfseResposta><Protocolo>SEGREDO</Protocolo></ConsultarNfseResposta>")]
        [InlineData("<EnviarLoteRpsResposta><cStat>107</cStat></EnviarLoteRpsResposta>")]
        [Trait("Utility", "Disponibilidade")]
        public void NfseRespostaIncompletaOuDeOutroServicoNaoConfirmaRecepcao(string xml)
        {
            var config = Config(TipoDFe.NFSe, Servico.NFSeRecepcionarLoteRps); config.PadraoNFSe = PadraoNFSe.WEBISS;
            Assert.Equal(StatusDisponibilidade.Inconclusivo, Executar(config, xml).Status);
        }

        [Fact, Trait("Utility", "Disponibilidade")]
        public void AliasConfiguradoAindaExigeContratoDoPadrao()
        {
            var config = Config(TipoDFe.NFSe, Servico.NFSeEnvioLoteRps); config.PadraoNFSe = PadraoNFSe.WEBISS;
            config.WebTagRetorno = "EnviarLoteRpsResposta";
            Assert.Equal(StatusDisponibilidade.Operacional, Executar(config, "<EnviarLoteRpsResposta xmlns='http://www.abrasf.org.br/nfse.xsd'><Protocolo>PROTO-TESTE</Protocolo></EnviarLoteRpsResposta>").Status);
            Assert.Equal(StatusDisponibilidade.Inconclusivo, Executar(config, "<EnviarLoteRpsResposta xmlns='urn:estranho'><Protocolo>PROTO-TESTE</Protocolo></EnviarLoteRpsResposta>").Status);
        }

        [Theory]
        [InlineData("E0014", StatusDisponibilidade.Operacional, TipoFalhaDisponibilidade.Nenhuma)]
        [InlineData("E0138", StatusDisponibilidade.Operacional, TipoFalhaDisponibilidade.Nenhuma)]
        [InlineData("E1203", StatusDisponibilidade.Inconclusivo, TipoFalhaDisponibilidade.Certificado)]
        [InlineData("E1207", StatusDisponibilidade.Inconclusivo, TipoFalhaDisponibilidade.Certificado)]
        [InlineData("E999", StatusDisponibilidade.Inconclusivo, TipoFalhaDisponibilidade.Protocolo)]
        [Trait("Utility", "Disponibilidade")]
        public void NacionalNaoConfundeRejeicaoDeDpsComIndisponibilidade(string codigo, StatusDisponibilidade status, TipoFalhaDisponibilidade falha)
        {
            var config = Config(TipoDFe.NFSe, Servico.NFSeGerarNfse); config.PadraoNFSe = PadraoNFSe.NACIONAL;
            var item = Executar(config, "<temp><tipoAmbiente>2</tipoAmbiente><erros><Codigo>" + codigo + "</Codigo><Descricao>SEGREDO</Descricao></erros></temp>", HttpStatusCode.UnprocessableEntity);
            Assert.True(item.RespostaAplicacaoReconhecida);
            Assert.Equal(status, item.Status);
            Assert.Equal(falha, item.TipoFalha);
        }

        [Theory]
        [InlineData(50, StatusDisponibilidade.Operacional)]
        [InlineData(44, StatusDisponibilidade.Inconclusivo)]
        [Trait("Utility", "Disponibilidade")]
        public void NacionalConsultaDpsExigeChaveDoContrato(int tamanho, StatusDisponibilidade status)
        {
            var config = Config(TipoDFe.NFSe, Servico.NFSeConsultarNfsePorRps); config.PadraoNFSe = PadraoNFSe.NACIONAL;
            Assert.Equal(status, Executar(config, "<temp><chaveAcesso>" + new string('1', tamanho) + "</chaveAcesso></temp>").Status);
        }

        [Theory]
        [InlineData("DOCUMENTOS_LOCALIZADOS", StatusDisponibilidade.Operacional)]
        [InlineData("NENHUM_DOCUMENTO_LOCALIZADO", StatusDisponibilidade.Operacional)]
        [InlineData("REJEICAO", StatusDisponibilidade.Inconclusivo)]
        [InlineData("OK", StatusDisponibilidade.Inconclusivo)]
        [Trait("Utility", "Disponibilidade")]
        public void AdnReconheceConsultaInclusiveSemDocumentos(string codigo, StatusDisponibilidade status)
        {
            var config = Config(TipoDFe.NFSe, Servico.NFSeConsultarDistribuicaoNFSeNSU); config.PadraoNFSe = PadraoNFSe.NACIONAL;
            Assert.Equal(status, Executar(config, "<temp><StatusProcessamento>" + codigo + "</StatusProcessamento></temp>").Status);
        }

        [Theory]
        [InlineData("<temp><tipoAmbiente>2</tipoAmbiente><erros><Codigo>E0014</Codigo></erros><erros><Codigo>E999</Codigo></erros></temp>")]
        [InlineData("<temp><erros><Codigo>E0014</Codigo></erros></temp>")]
        [InlineData("<NFSe xmlns='http://www.sped.fazenda.gov.br/nfse'><infNFSe><campoDesconhecido>SEGREDO</campoDesconhecido></infNFSe></NFSe>")]
        [Trait("Utility", "Disponibilidade")]
        public void NacionalCodigoConhecidoNaoEncobreErroDesconhecidoOuEnvelopeInvalido(string xml)
        {
            var config = Config(TipoDFe.NFSe, Servico.NFSeGerarNfse); config.PadraoNFSe = PadraoNFSe.NACIONAL;
            Assert.Equal(StatusDisponibilidade.Inconclusivo, Executar(config, xml).Status);
        }

        [Theory]
        [InlineData(Servico.NFSeRecepcionarEventosDiversos)]
        [InlineData(Servico.NFSeCancelarNfse)]
        [Trait("Utility", "Disponibilidade")]
        public void NacionalReconheceProcessamentoDeEventoSemCStat(Servico servico)
        {
            var config = Config(TipoDFe.NFSe, servico); config.PadraoNFSe = PadraoNFSe.NACIONAL;
            var item = Executar(config, "<evento xmlns='http://www.sped.fazenda.gov.br/nfse'><infEvento><verAplic>TESTE</verAplic><dhProc>2026-11-09T10:00:00-03:00</dhProc></infEvento></evento>");
            Assert.Equal(StatusDisponibilidade.Operacional, item.Status);
            Assert.Equal(0, item.CStat);
        }

        [Fact, Trait("Utility", "Disponibilidade")]
        public void PdfBase64ComBordasCorretasEConteudoMalformadoNaoViraDisponivel()
        {
            var valor = Convert.ToBase64String(Encoding.ASCII.GetBytes("%PDF-1.4\n" + new string('x', 100) + "\n%%EOF\n"));
            valor = valor.Substring(0, 40) + "!" + valor.Substring(41);
            var xml = "<RetObterOperacaoTransportePdf xmlns='" + Unimake.Business.DFe.Xml.CIOT.CIOTNamespace.PortalANTT + "'><Sucesso>true</Sucesso><Pdf>" + valor + "</Pdf></RetObterOperacaoTransportePdf>";
            Assert.Equal(StatusDisponibilidade.Inconclusivo, Executar(Config(TipoDFe.CIOT, Servico.CIOTObterOperacaoTransportePdf), xml).Status);
        }

        [Fact, Trait("Utility", "Disponibilidade")]
        public void PdfCodigoDeSucessoSemArquivoNaoConfirmaEntrega()
        {
            var xml = "<RetObterOperacaoTransportePdf xmlns='" + Unimake.Business.DFe.Xml.CIOT.CIOTNamespace.PortalANTT + "'><Codigo>110</Codigo></RetObterOperacaoTransportePdf>";
            Assert.Equal(StatusDisponibilidade.Inconclusivo, Executar(Config(TipoDFe.CIOT, Servico.CIOTObterOperacaoTransportePdf), xml).Status);
        }

        [Theory]
        [InlineData("<Codigo>110</Codigo>", StatusDisponibilidade.Operacional, TipoFalhaDisponibilidade.Nenhuma)]
        [InlineData("<Codigo>EF123</Codigo>", StatusDisponibilidade.Inconclusivo, TipoFalhaDisponibilidade.Protocolo)]
        [InlineData("<Codigo>110</Codigo><Sucesso>false</Sucesso>", StatusDisponibilidade.Inconclusivo, TipoFalhaDisponibilidade.Protocolo)]
        [InlineData("<Codigo>110</Codigo><Sucesso>abc</Sucesso>", StatusDisponibilidade.Inconclusivo, TipoFalhaDisponibilidade.Protocolo)]
        [InlineData("<temp><error>USUARIO_NAO_AUTORIZADO</error></temp>", StatusDisponibilidade.Inconclusivo, TipoFalhaDisponibilidade.Autenticacao)]
        [InlineData("<temp><error>ERRO_NOVO_SEGREDO</error></temp>", StatusDisponibilidade.Inconclusivo, TipoFalhaDisponibilidade.Protocolo)]
        [Trait("Utility", "Disponibilidade")]
        public void CiotNaoTransformaCodigoDesconhecidoEmSucesso(string corpo, StatusDisponibilidade status, TipoFalhaDisponibilidade falha)
        {
            var xml = "<RetCancelamentoOperacaoTransporte xmlns='" + Unimake.Business.DFe.Xml.CIOT.CIOTNamespace.PortalANTT + "'>" + corpo + "</RetCancelamentoOperacaoTransporte>";
            var item = Executar(Config(TipoDFe.CIOT, Servico.CIOTCancelamentoOperacaoTransporte), xml);
            Assert.Equal(status, item.Status);
            Assert.Equal(falha, item.TipoFalha);
        }

        [Theory]
        [InlineData("%PDF-1.4\nconteudo sintético\n%%EOF\n", true)]
        [InlineData("%PDF-1.4\nconteudo truncado", false)]
        [InlineData("<html>SEGREDO</html>", false)]
        [InlineData("%PDF-SEGREDO\nconteudo\n%%EOF", false)]
        [Trait("Utility", "Disponibilidade")]
        public void DownloadPdfNaoConsomeNemModificaStreamOuRepeteRequest(string corpo, bool completo)
        {
            var bytes = Encoding.UTF8.GetBytes(corpo);
            var config = Config(TipoDFe.NFSe, Servico.NFSeConsultarNfsePDF); config.PadraoNFSe = PadraoNFSe.NACIONAL;
            var item = Executar(config, corpo, conferir: c =>
            {
                Assert.NotNull(c.RetornoServicoStream);
                Assert.Equal(0, c.RetornoServicoStream.Position);
                using var copia = new MemoryStream(); c.RetornoServicoStream.CopyTo(copia);
                Assert.Equal(bytes, copia.ToArray());
            }, mediaType: "application/pdf");
            Assert.Equal(completo ? StatusDisponibilidade.Operacional : StatusDisponibilidade.Inconclusivo, item.Status);
            Assert.Contains("PDF", item.Limitacao);
        }

        [Theory]
        [InlineData(Servico.NFSeConsultarNfse, "application/pdf")]
        [InlineData(Servico.NFSeConsultarNfsePDF, "application/octet-stream")]
        [Trait("Utility", "Disponibilidade")]
        public void PdfNaoConfirmaServicoDiferenteNemFormatoNaoDeclarado(Servico servico, string mediaType)
        {
            var config = Config(TipoDFe.NFSe, servico); config.PadraoNFSe = PadraoNFSe.NACIONAL;
            Assert.Equal(StatusDisponibilidade.Inconclusivo, Executar(config, "%PDF-1.4\nconteudo\n%%EOF", mediaType: mediaType).Status);
        }

        [Theory]
        [InlineData(true, false)]
        [InlineData(true, true)]
        [InlineData(false, false)]
        [Trait("Utility", "Disponibilidade")]
        public void CiotPdfExecutarPreservaResultMesmoQuandoDiagnosticoNaoConfirmaArquivo(bool completo, bool quebras)
        {
            var pdf = Convert.ToBase64String(Encoding.ASCII.GetBytes(completo ? "%PDF-1.4\nconteudo sintético\n%%EOF\n" : "arquivo truncado"));
            if (quebras) pdf = pdf.Insert(12, "\r\n");
            var executor = new ApiFake(Newtonsoft.Json.JsonConvert.SerializeObject(new { Sucesso = true, Pdf = pdf }), HttpStatusCode.OK, "application/json");
            var config = Config(TipoDFe.CIOT, Servico.CIOTObterOperacaoTransportePdf);
            config.ProvedorCIOT = ProvedorCIOT.EFrete; config.EFreteToken = "TOKEN-TESTE"; config.EFreteIntegrador = "INTEGRADOR-TESTE";
            var canal = new CanalObservacoesDisponibilidade();
            using (ApiTransportExecutorFactory.Override(() => executor))
            using (var sessao = canal.IniciarOperacao(config, Guid.NewGuid(), Guid.NewGuid()))
            {
                var servico = new Unimake.Business.DFe.Servicos.CIOT.ObterOperacaoTransportePdf(
                    new Unimake.Business.DFe.Xml.CIOT.ObterOperacaoTransportePdf { CodigoIdentificacaoOperacao = "992000000126/4321", ProvedorCIOT = ProvedorCIOT.EFrete }, config);
                servico.Executar();
                // A leitura XML existente normaliza CRLF para LF; os bytes do arquivo são idênticos.
                Assert.Equal(Convert.FromBase64String(pdf), Convert.FromBase64String(servico.Result.Pdf));
                Assert.True(servico.Result.Sucesso);
                var pdfOriginal = servico.Result.Pdf;
                var retornoOriginal = servico.RetornoWSString;
                sessao.Concluir(servico.RetornoWSXML, servico.HttpStatusCode);
                Assert.Equal(pdfOriginal, servico.Result.Pdf);
                Assert.Equal(retornoOriginal, servico.RetornoWSString);
            }
            Assert.Equal(1, executor.Chamadas);
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal(completo ? StatusDisponibilidade.Operacional : StatusDisponibilidade.Inconclusivo, item.Status);
            Assert.DoesNotContain(pdf, item.Limitacao);
        }

        private static Configuracao Config(TipoDFe tipo, Servico servico) => new Configuracao
        {
            TipoDFe = tipo, Servico = servico, CodigoUF = 35, TipoAmbiente = TipoAmbiente.Homologacao, ColetarTelemetriaDisponibilidade = true
        };

        private static ObservacaoDisponibilidade Executar(Configuracao config, string corpo, HttpStatusCode http = HttpStatusCode.OK,
            Action<ConsumirAPI> conferir = null, string mediaType = "application/xml")
        {
            var executor = new ApiFake(corpo, http, mediaType);
            var canal = new CanalObservacoesDisponibilidade();
            using (ApiTransportExecutorFactory.Override(() => executor))
            using (var sessao = canal.IniciarOperacao(config, Guid.NewGuid(), Guid.NewGuid()))
            using (var consumidor = new ConsumirAPI())
            {
                var tag = "prop:innertext";
                if (mediaType == "application/xml")
                {
                    var documento = new System.Xml.XmlDocument(); documento.LoadXml(corpo);
                    tag = documento.DocumentElement.Name;
                }
                consumidor.ExecutarServico(new APIConfig { RequestURI = "https://api.test/SEGREDO?token=SEGREDO", MetodoAPI = "get", TagRetorno = tag, UsaCertificadoDigital = false, UseProxy = false }, null);
                var retornoOriginal = consumidor.RetornoServicoXML.OuterXml;
                sessao.Concluir(consumidor.RetornoServicoXML, consumidor.HttpStatusCode);
                Assert.Equal(retornoOriginal, consumidor.RetornoServicoXML.OuterXml);
                conferir?.Invoke(consumidor);
            }
            Assert.Equal(1, executor.Chamadas);
            Assert.True(canal.TentarLer(out var item));
            Assert.False(canal.TentarLer(out _));
            Assert.Equal(1, item.EntradasTransporte);
            Assert.DoesNotContain("SEGREDO", string.Join("|", typeof(ObservacaoDisponibilidade).GetProperties().Select(p => p.GetValue(item))));
            return item;
        }

        private sealed class ApiFake(string corpo, HttpStatusCode status, string mediaType) : IApiTransportExecutor
        {
            internal int Chamadas;
            public TransportResponse Execute(TransportRequest request)
            {
                Chamadas++;
                return new TransportResponse { StatusCode = status, HttpResponseMessage = new HttpResponseMessage(status) { Content = new StringContent(corpo, Encoding.UTF8, mediaType) } };
            }
        }
    }
}
