using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Exceptions;
using DFeNFeABI = Unimake.Business.DFe.Xml.NFeABI;
using Xunit;

namespace Unimake.DFe.Test.NFeABI.Serializacao
{
    public class EventosNFeABITest
    {
        private const string NamespaceNFeABI = "http://www.portalfiscal.inf.br/nfeabi";

        [Theory]
        [InlineData("eventoNFeABI-cancelamento-valido.xml", "NFeABI.evCancNFeABI_v1.00.xsd")]
        [InlineData("eventoNFeABI-pagamento-parcela-valido.xml", "NFeABI.evPagParcelaNFeABI_v1.00.xsd")]
        [InlineData("eventoNFeABI-pagamento-complementar-valido.xml", "NFeABI.evPagParcelaNFeABI_v1.00.xsd")]
        [InlineData("eventoNFeABI-apropriacao-credito-valido.xml", "NFeABI.evApropCredIndNFeABI_v1.00.xsd")]
        [Trait("DFe", "NFeABI")]
        public void DesserializaValidaERoundTripDosTresEventos(string arquivo, string schemaDetalhe)
        {
            var original = new XmlDocument();
            original.Load(Path.Combine(@"..\..\..\NFeABI\Resources", arquivo));

            var evento = XMLUtility.Deserializar<DFeNFeABI.EventoNFeABI>(original.OuterXml);
            evento.Validar();
            var roundTrip = evento.GerarXML();

            Assert.Equal(original.InnerText, roundTrip.InnerText);
            Assert.Equal("ID" + ((int)evento.InfEvento.TpEvento).ToString("000000") + evento.InfEvento.ChNFeABI + "1", evento.InfEvento.Id);
            AssertSchema(roundTrip, "NFeABI.eventoNFeABI_v1.00.xsd");

            XmlDocument detalhe;
            if (evento.InfEvento.DetEvento.EvCancNFeABI != null) detalhe = evento.InfEvento.DetEvento.EvCancNFeABI.GerarXML();
            else if (evento.InfEvento.DetEvento.EvPagParcelaNFeABI != null) detalhe = evento.InfEvento.DetEvento.EvPagParcelaNFeABI.GerarXML();
            else detalhe = evento.InfEvento.DetEvento.EvApropCredIndNFeABI.GerarXML();
            AssertSchema(detalhe, schemaDetalhe);
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void SerializaRetornoEEventoProcessadoComAtributosOpcionais()
        {
            var evento = CarregarEvento("eventoNFeABI-cancelamento-valido.xml");
            var retorno = new DFeNFeABI.RetEventoNFeABI
            {
                Versao = "1.00",
                InfEvento = new DFeNFeABI.InfRetEventoNFeABI
                {
                    TpAmb = TipoAmbiente.Homologacao,
                    VerAplic = "TESTE ABI-004",
                    COrgao = UFBrasil.PR,
                    CStat = 135,
                    XMotivo = "Evento registrado e vinculado",
                    ChNFeABI = evento.InfEvento.ChNFeABI,
                    TpEvento = DFeNFeABI.TipoEventoNFeABI.Cancelamento,
                    XEvento = "Cancelamento homologado",
                    NSeqEvento = 1,
                    DhRegEventoField = "2026-09-14T12:01:00-03:00",
                    NProt = "141260000000001"
                }
            };
            var processado = new DFeNFeABI.ProcEventoNFeABI
            {
                Versao = "1.00",
                IPTransmissor = "127.0.0.1",
                NPortaCon = 0,
                DhConexaoField = "2026-09-14T12:00:30-03:00",
                EventoNFeABI = evento,
                RetEventoNFeABI = retorno
            };

            var xmlRetorno = retorno.GerarXML();
            var xmlProcessado = processado.GerarXML();
            AssertSchema(xmlRetorno, "NFeABI.retEventoNFeABI_v1.00.xsd");
            AssertSchema(xmlProcessado, "NFeABI.procEventoNFeABI_v1.00.xsd");
            Assert.Contains("nPortaCon=\"0\"", xmlProcessado.OuterXml);
            Assert.Equal(evento.InfEvento.ChNFeABI + "_110111_1-procEventoNFeABI.xml", processado.NomeArquivoDistribuicao);
            Assert.Equal(xmlProcessado.InnerText, XMLUtility.Deserializar<DFeNFeABI.ProcEventoNFeABI>(xmlProcessado.OuterXml).GerarXML().InnerText);
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void RejeitaTipoDeEventoIncompativelESequenciaDiferenteDeUm()
        {
            var evento = CarregarEvento("eventoNFeABI-cancelamento-valido.xml");
            evento.InfEvento.TpEvento = DFeNFeABI.TipoEventoNFeABI.PagamentoParcela;
            Assert.Throws<ValidatorDFeException>(() => evento.Validar());

            evento.InfEvento.TpEvento = DFeNFeABI.TipoEventoNFeABI.Cancelamento;
            evento.InfEvento.NSeqEvento = 2;
            Assert.Throws<ValidatorDFeException>(() => evento.Validar());
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void RejeitaCamposCondicionaisInvalidosDoPagamento()
        {
            var pagamento01 = CarregarEvento("eventoNFeABI-pagamento-parcela-valido.xml").InfEvento.DetEvento.EvPagParcelaNFeABI;
            pagamento01.VOrigParcela = 10;
            Assert.Throws<ValidatorDFeException>(() => pagamento01.Validar());

            var pagamento02 = new DFeNFeABI.EvPagParcelaNFeABI
            {
                CdEventoPag = DFeNFeABI.CodigoEventoPagamentoNFeABI.PagamentoComplementar,
                VOrigParcela = 100,
                GIBSCBS = new DFeNFeABI.GIBSCBSEventoPagParcela()
            };
            Assert.Throws<ValidatorDFeException>(() => pagamento02.Validar());

            pagamento02 = CarregarEvento("eventoNFeABI-pagamento-complementar-valido.xml").InfEvento.DetEvento.EvPagParcelaNFeABI;
            pagamento02.GIBSCBS.VBC = 5000.01;
            Assert.Throws<ValidatorDFeException>(() => pagamento02.Validar());
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void PreservaPrecisaoMonetariaEPercentualDoEvento()
        {
            var pagamento = CarregarEvento("eventoNFeABI-pagamento-parcela-valido.xml").InfEvento.DetEvento.EvPagParcelaNFeABI;
            var xml = pagamento.GerarXML().OuterXml;
            Assert.Contains("<vParcela>100000.00</vParcela>", xml);
            Assert.Contains("<pIBSUF>0.1000</pIBSUF>", xml);

            var apropriacao = CarregarEvento("eventoNFeABI-apropriacao-credito-valido.xml").InfEvento.DetEvento.EvApropCredIndNFeABI;
            Assert.Contains("<pParticip>60.0000</pParticip>", apropriacao.GerarXML().OuterXml);
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void RejeitaEscolhasIdentificadorasEParticipacoesInvalidas()
        {
            var evento = CarregarEvento("eventoNFeABI-cancelamento-valido.xml");
            evento.InfEvento.CPF = "12345678901";
            Assert.Throws<ValidatorDFeException>(() => evento.Validar());

            var apropriacao = CarregarEvento("eventoNFeABI-apropriacao-credito-valido.xml");
            apropriacao.InfEvento.DetEvento.EvApropCredIndNFeABI.Adquirente[1].NAdquir = 3;
            Assert.Throws<ValidatorDFeException>(() => apropriacao.Validar());

            apropriacao = CarregarEvento("eventoNFeABI-apropriacao-credito-valido.xml");
            apropriacao.InfEvento.DetEvento.EvApropCredIndNFeABI.Adquirente[1].PParticip = 30;
            Assert.Throws<ValidatorDFeException>(() => apropriacao.Validar());

            apropriacao = CarregarEvento("eventoNFeABI-apropriacao-credito-valido.xml");
            apropriacao.InfEvento.DetEvento.EvApropCredIndNFeABI.Adquirente[0].CPF = "12345678901";
            Assert.Throws<ValidatorDFeException>(() => apropriacao.Validar());
        }

        private static DFeNFeABI.EventoNFeABI CarregarEvento(string arquivo)
        {
            var original = new XmlDocument();
            original.Load(Path.Combine(@"..\..\..\NFeABI\Resources", arquivo));
            return XMLUtility.Deserializar<DFeNFeABI.EventoNFeABI>(original.OuterXml);
        }

        private static void AssertSchema(XmlDocument documento, string schema)
        {
            var validador = new ValidarSchema();
            validador.Validar(documento, schema, NamespaceNFeABI);
            Assert.True(validador.Success, validador.ErrorMessage);
        }
    }
}
