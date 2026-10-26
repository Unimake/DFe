using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Serializacao
{
    [Trait("DFe", "NFe")]
    public class AtualizacaoSchemaPagamentoTest
    {
        private const string Ns = "http://www.portalfiscal.inf.br/nfe";
        private const string EventoRecurso = @"..\..\..\NFe\Resources\EventoVinculacaoPagamento.xml";

        [Fact]
        public void Evento110300PreservaGrupoAtributoTextosEOrdem()
        {
            var original = new XmlDocument();
            original.Load(EventoRecurso);
            var evento = XMLUtility.Deserializar<EnvEvento>(original);
            var detalhe = Assert.IsType<DetEventoVinculacaoPagamento>(evento.Evento[0].InfEvento.DetEvento);
            Assert.Equal(TipoEventoNFe.VinculacaoPagamento, evento.Evento[0].InfEvento.TpEvento);
            Assert.Equal(TipoAutor.EmpresaEmitente, detalhe.TpAutor);
            Assert.Equal("01", detalhe.GPgto.TpMeioPgto);
            Assert.Equal("TESTE&TRANSACAO", detalhe.GPgto.IdTransacao);
            Assert.Equal("00000000", detalhe.GPgto.CNPJBasePSP);
            var gerado = evento.GerarXML();
            Assert.Equal(original.InnerText, gerado.InnerText);
            var det = gerado.GetElementsByTagName("detEvento")[0];
            Assert.Equal(new[] { "descEvento", "cOrgaoAutor", "tpAutor", "verAplic", "nProt", "gPgto" },
                det.ChildNodes.Cast<XmlNode>().Select(n => n.LocalName));
            Assert.Null(det.LastChild.Attributes["nPag"]);
            Validar(Documento(det), "e110300_v1.00.xsd");
            var roundTrip = XMLUtility.Deserializar<EnvEvento>(gerado);
            Assert.Equal("TESTE&TRANSACAO", Assert.IsType<DetEventoVinculacaoPagamento>(roundTrip.Evento[0].InfEvento.DetEvento).GPgto.IdTransacao);
        }

        [Fact]
        public void NFePreservaValoresLiquidosICMSPrevistoEListaDePagamentos()
        {
            var nfe = CriarNFe();
            var inf = nfe.InfNFeField;
            var prod = inf.Det[0].Prod;
            prod.VUnComLiq = 1.1234567890m;
            prod.VProdLiq = 0;
            inf.Total.VProdLiqTot = 0;
            inf.Det[0].Imposto.ICMS = new ICMS
            {
                ICMSPrevistoPagtoAntecip = new ICMSPrevistoPagtoAntecip { VICMSPrevisto = 12.34 }
            };
            inf.GPgtoVinc = new GPgtoVinc
            {
                GPgto = new List<GPgto>
                {
                    new GPgto { NPag = 1, IdTransacao = "TESTE&01", TpMeioPgto = "01", CNPJReceb = "00000000000191", CNPJBasePSP = "00000000" },
                    new GPgto { NPag = 990, IdTransacao = "TESTE02", TpMeioPgto = "99", CNPJReceb = "00000000000191", CNPJBasePSP = "00000000" }
                }
            };
            var xml = XMLUtility.Serializar(nfe);
            Assert.Equal("0.00", xml.GetElementsByTagName("vProdLiq")[0].InnerText);
            var filhos = xml.GetElementsByTagName("prod")[0].ChildNodes.Cast<XmlNode>().Select(n => n.LocalName).ToList();
            Assert.Equal(new[] { "vProd", "vUnComLiq", "vProdLiq", "cEANTrib" }, filhos.Skip(filhos.IndexOf("vProd")).Take(4));
            Assert.Equal("vProdLiqTot", xml.GetElementsByTagName("total")[0].FirstChild.LocalName);
            var elementos = xml.GetElementsByTagName("infNFe")[0].ChildNodes.Cast<XmlNode>().Select(n => n.LocalName).ToList();
            Assert.True(elementos.IndexOf("gPgtoVinc") > elementos.IndexOf("pag"));
            Assert.True(elementos.IndexOf("gPgtoVinc") < elementos.IndexOf("infAdic"));
            Validar(xml, "nfe_v4.00.xsd");
            var roundTrip = XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(xml);
            Assert.Equal(1.1234567890m, roundTrip.InfNFeField.Det[0].Prod.VUnComLiq);
            Assert.Equal(12.34, roundTrip.InfNFeField.Det[0].Imposto.ICMS.ICMSPrevistoPagtoAntecip.VICMSPrevisto);
            Assert.Equal(2, roundTrip.InfNFeField.GPgtoVinc.GPgto.Count);
            Assert.Equal(990, roundTrip.InfNFeField.GPgtoVinc.GPgto[1].NPag);
            Assert.Equal("TESTE&01", roundTrip.InfNFeField.GPgtoVinc.GPgto[0].IdTransacao);
            var lote = new EnviNFe { Versao = "4.00", IdLote = "1", IndSinc = SimNao.Sim, NFe = new List<Business.DFe.Xml.NFe.NFe> { roundTrip } };
            Validar(lote.GerarXML(), "enviNFe_v4.00.xsd");
        }

        [Fact]
        public void CamposOpcionaisAusentesNaoGeramValoresDefault()
        {
            var nfe = CriarNFe();
            var xml = XMLUtility.Serializar(nfe);
            foreach (var tag in new[] { "vUnComLiq", "vProdLiq", "vProdLiqTot", "gPgtoVinc", "ICMSPrevistoPagtoAntecip" })
            {
                Assert.Empty(xml.GetElementsByTagName(tag).Cast<XmlNode>());
            }
            Validar(xml, "nfe_v4.00.xsd");
            nfe.InfNFeField.Det[0].Prod.VUnComLiq = 0;
            nfe.InfNFeField.Det[0].Prod.VProdLiq = 0;
            nfe.InfNFeField.Total.VProdLiqTot = 0;
            var zero = XMLUtility.Serializar(XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(XMLUtility.Serializar(nfe)));
            Assert.Single(zero.GetElementsByTagName("vUnComLiq").Cast<XmlNode>());
            Assert.Equal("0.00", zero.GetElementsByTagName("vProdLiqTot")[0].InnerText);
        }

        [Theory]
        [InlineData(110300)]
        [InlineData(211120)]
        [InlineData(412140)]
        [InlineData(420110)]
        [InlineData(420120)]
        [InlineData(420130)]
        [InlineData(420140)]
        [InlineData(420150)]
        [InlineData(420160)]
        [InlineData(420170)]
        [InlineData(112110)]
        public void CancelamentoAceitaNovosCodigosEPreservaLegado(int codigo)
        {
            var original = XMLUtility.Deserializar<EnvEvento>(File.ReadAllText(EventoRecurso));
            original.Evento[0].InfEvento.TpEvento = TipoEventoNFe.Desconhecido;
            original.Evento[0].InfEvento.DetEvento = new DetEventoCancelamentoDeEvento
            {
                Versao = "1.00", COrgaoAutor = UFBrasil.PR, VerAplic = "TESTE", TpEventoAut = (TipoEventoNFe)codigo,
                NProtEvento = "141260000000001"
            };
            original.Evento[0].InfEvento.TpEvento = TipoEventoNFe.CancelamentoDeEvento;
            var xml = original.GerarXML();
            var det = Documento(xml.GetElementsByTagName("detEvento")[0]);
            Validar(det, "e110001_v1.00.xsd");
            var roundTrip = XMLUtility.Deserializar<EnvEvento>(xml);
            Assert.Equal(codigo, (int)Assert.IsType<DetEventoCancelamentoDeEvento>(roundTrip.Evento[0].InfEvento.DetEvento).TpEventoAut);
            det.DocumentElement.RemoveAttribute("versao");
            var validar = new ValidarSchema();
            validar.Validar(det, "NFe.e110001_v1.00.xsd", Ns);
            Assert.False(validar.Success);
            Assert.Contains("versao", validar.ErrorMessage);
        }

        [Fact]
        public void SchemaExigeParDeValoresLiquidosEGrupoDePagamentoNaoVazio()
        {
            var nfe = CriarNFe();
            nfe.InfNFeField.Det[0].Prod.VUnComLiq = 10;
            var validar = new ValidarSchema();
            validar.Validar(XMLUtility.Serializar(nfe), "NFe.nfe_v4.00.xsd", Ns);
            Assert.False(validar.Success);
            Assert.Contains("vProdLiq", validar.ErrorMessage);
            nfe.InfNFeField.Det[0].Prod.VProdLiq = 10;
            nfe.InfNFeField.GPgtoVinc = new GPgtoVinc { GPgto = new List<GPgto>() };
            validar.Validar(XMLUtility.Serializar(nfe), "NFe.nfe_v4.00.xsd", Ns);
            Assert.False(validar.Success);
            Assert.Contains("gPgto", validar.ErrorMessage);
        }

        [Fact]
        public void Evento110300RespeitaUmaOcorrenciaEAutorEmitente()
        {
            var xml = new XmlDocument();
            xml.Load(EventoRecurso);
            var det = Documento(xml.GetElementsByTagName("detEvento")[0]);
            det.GetElementsByTagName("descEvento")[0].InnerText = "Vinculacao Pagamento";
            Validar(det, "e110300_v1.00.xsd");
            var pgto = det.GetElementsByTagName("gPgto")[0];
            det.DocumentElement.AppendChild(pgto.CloneNode(true));
            var validar = new ValidarSchema();
            validar.Validar(det, "NFe.e110300_v1.00.xsd", Ns);
            Assert.False(validar.Success);
            Assert.Contains("gPgto", validar.ErrorMessage);
            det.DocumentElement.RemoveChild(det.DocumentElement.LastChild);
            det.GetElementsByTagName("tpAutor")[0].InnerText = "2";
            validar.Validar(det, "NFe.e110300_v1.00.xsd", Ns);
            Assert.False(validar.Success);
            Assert.Contains("tpAutor", validar.ErrorMessage);
        }

        [Fact]
        public void Distribuicao135PreservaTiposRemovidosNaAtualizacao()
        {
            var consulta = new DistDFeInt
            {
                Versao = "1.35", TpAmb = TipoAmbiente.Homologacao, CNPJ = "00000000000191",
                CUFAutor = UFBrasil.PR, DistNSU = new DistNSU { UltNSU = "000000000000000" }
            };
            var xml = consulta.GerarXML();
            Validar(xml, "distDFeInt_v1.35.xsd");
            Assert.Equal("000000000000000", XMLUtility.Deserializar<DistDFeInt>(xml).DistNSU.UltNSU);
            var retorno = new RetDistDFeInt
            {
                Versao = "1.35", TpAmb = TipoAmbiente.Homologacao, VerAplic = "TESTE", CStat = 137,
                XMotivo = "Nenhum documento localizado", UltNSU = "000000000000000", MaxNSU = "000000000000000",
                DhRespField = "2026-10-26T10:00:00-03:00"
            };
            Validar(retorno.GerarXML(), "retDistDFeInt_v1.35.xsd");
        }

        [Theory]
        [InlineData("nPag", false)]
        [InlineData("idTransacao", false)]
        [InlineData("nPag", true)]
        [InlineData("idTransacao", true)]
        public void SchemaRejeitaAtributoDuplicadoNosPagamentosVinculados(string atributo, bool validarLote)
        {
            var nfe = CriarNFe();
            nfe.InfNFeField.GPgtoVinc = CriarPagamentosVinculados();
            var pagamentos = nfe.InfNFeField.GPgtoVinc.GPgto;
            if (atributo == "nPag")
            {
                pagamentos[1].NPag = pagamentos[0].NPag;
            }
            else
            {
                pagamentos[1].IdTransacao = pagamentos[0].IdTransacao;
            }

            var validar = new ValidarSchema();
            validar.Validar(GerarDocumentoPagamentos(nfe, validarLote),
                validarLote ? "NFe.enviNFe_v4.00.xsd" : "NFe.nfe_v4.00.xsd", Ns);

            Assert.False(validar.Success, "O schema aceitou o atributo duplicado: " + atributo);
            Assert.Contains("pk_gPgto_" + atributo, validar.ErrorMessage);
        }

        [Theory]
        [InlineData(false)]
        [InlineData(true)]
        public void SchemaAceitaPagamentosVinculadosComAtributosDistintos(bool validarLote)
        {
            var nfe = CriarNFe();
            nfe.InfNFeField.GPgtoVinc = CriarPagamentosVinculados();

            Validar(GerarDocumentoPagamentos(nfe, validarLote),
                validarLote ? "enviNFe_v4.00.xsd" : "nfe_v4.00.xsd");
        }

        private static GPgtoVinc CriarPagamentosVinculados() => new GPgtoVinc
        {
            GPgto = new List<GPgto>
            {
                new GPgto { NPag = 1, IdTransacao = "TRANSACAO-01", TpMeioPgto = "01", CNPJReceb = "00000000000191", CNPJBasePSP = "00000000" },
                new GPgto { NPag = 2, IdTransacao = "TRANSACAO-02", TpMeioPgto = "99", CNPJReceb = "00000000000191", CNPJBasePSP = "00000000" }
            }
        };

        private static XmlDocument GerarDocumentoPagamentos(Business.DFe.Xml.NFe.NFe nfe, bool validarLote) =>
            validarLote
                ? new EnviNFe
                {
                    Versao = "4.00", IdLote = "1", IndSinc = SimNao.Sim,
                    NFe = new List<Business.DFe.Xml.NFe.NFe> { nfe }
                }.GerarXML()
                : XMLUtility.Serializar(nfe);

        private static Business.DFe.Xml.NFe.NFe CriarNFe()
        {
            var doc = new XmlDocument();
            doc.Load(@"..\..\..\NFe\Resources\99999999999999999999999999999999999999999999-procNFe.xml");
            var nfe = XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(doc.GetElementsByTagName("NFe")[0].OuterXml);
            nfe.InfNFeSupl.QrCode = "https://www.exemplo.gov.br/qrcode?p=41250899999999999900000000000000009000000000|3|2|01|0.00|1|12345678901|YWJjZA==";
            return nfe;
        }

        private static XmlDocument Documento(XmlNode node)
        {
            var doc = new XmlDocument();
            doc.AppendChild(doc.ImportNode(node, true));
            return doc;
        }

        private static void Validar(XmlDocument xml, string schema)
        {
            var validar = new ValidarSchema();
            validar.Validar(xml, "NFe." + schema, Ns);
            Assert.True(validar.Success, validar.ErrorMessage);
        }
    }
}
