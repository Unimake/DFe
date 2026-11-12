using System;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.EFDReinf;
using Xunit;

namespace Unimake.DFe.Test.EFDReinf.BugFixes
{
    [Trait("DFe", "EFDReinf")]
    public class RetornoPeriodoVazioTest
    {
        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("   ")]
        public void RetornoComOcorrencias_DeveLerPeriodoVazioSemFabricarData(string periodo)
        {
            var campo = periodo == null ? string.Empty : "<perApur>" + periodo + "</perApur>";
            var xml = "<Reinf xmlns=\"http://www.reinf.esocial.gov.br/schemas/retornoLoteEventosAssincrono/v1_00_00\"><retornoLoteEventosAssincrono><status><cdResposta>3</cdResposta><descResposta>Lote processado com ocorrencias</descResposta></status><retornoEventos><evento Id=\"ID1999999990001912026081500000000001\"><retornoEvento><Reinf xmlns=\"http://www.reinf.esocial.gov.br/schemas/evtTotal/v2_01_02\"><evtTotal id=\"ID900100000000000000000001\"><ideEvento>" + campo + "</ideEvento><ideContri><tpInsc>1</tpInsc><nrInsc>99999999</nrInsc></ideContri><ideRecRetorno><ideStatus><cdRetorno>1</cdRetorno><descRetorno>ERRO</descRetorno><regOcorrs><tpOcorr>1</tpOcorr><localErroAviso>/Reinf</localErroAviso><codResp>MS1000</codResp><dscResp>Ocorrencia sintetica</dscResp></regOcorrs></ideStatus></ideRecRetorno></evtTotal></Reinf></retornoEvento></evento></retornoEventos></retornoLoteEventosAssincrono></Reinf>";
            var retorno = XMLUtility.Deserializar<ReinfRetornoLoteAssincrono>(xml);
            Assert.Equal(3, retorno.RetornoLoteEventosAssincrono.Status.CdResposta);
            var total = retorno.RetornoLoteEventosAssincrono.RetornoEventos.Evento[0].RetornoEvento.Reinf9001;
            Assert.Equal(default(DateTimeOffset), total.EvtTotal.IdeEvento.PerApur);
            Assert.Equal(string.Empty, total.EvtTotal.IdeEvento.PerApurField);
            Assert.Equal(1, (int)total.EvtTotal.IdeRecRetorno.IdeStatus.CdRetorno);
            var gerado = total.GerarXML();
            Assert.Equal(string.Empty, gerado.SelectSingleNode("//*[local-name()='perApur']").InnerText);
        }

        [Fact]
        public void PeriodoValido_DevePreservarAnoEMes()
        {
            var evento = XMLUtility.Deserializar<Reinf9001>("<Reinf xmlns=\"http://www.reinf.esocial.gov.br/schemas/evtTotal/v2_01_02\"><evtTotal><ideEvento><perApur>2026-08</perApur></ideEvento></evtTotal></Reinf>");
            Assert.Equal(2026, evento.EvtTotal.IdeEvento.PerApur.Year);
            Assert.Equal(8, evento.EvtTotal.IdeEvento.PerApur.Month);
            Assert.Equal("2026-08", evento.GerarXML().SelectSingleNode("//*[local-name()='perApur']").InnerText);
        }

        [Theory]
        [InlineData("invalido")]
        [InlineData("2026-13")]
        [InlineData("2026-02-30")]
        public void PeriodoPreenchidoInvalido_DeveContinuarFalhando(string periodo)
        {
            var evento = new IdeEvento9001();
            Assert.Throws<FormatException>(() => evento.PerApurField = periodo);
        }

        [Fact]
        public void LimparPeriodo_DeveRemoverValorAnterior()
        {
            var evento = new IdeEvento9001 { PerApurField = "2026-08" };
            evento.PerApurField = string.Empty;
            Assert.Equal(default(DateTimeOffset), evento.PerApur);
            Assert.Equal(string.Empty, evento.PerApurField);
        }
    }
}
