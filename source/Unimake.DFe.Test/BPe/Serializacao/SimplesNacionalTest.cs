using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.BPe;
using BPeTMXml = Unimake.Business.DFe.Xml.BPeTM;
using BPeTAXml = Unimake.Business.DFe.Xml.BPeTA;
using Xunit;

namespace Unimake.DFe.Test.BPe.Serializacao
{
    public class SimplesNacionalTest
    {
        private const string NS = "http://www.portalfiscal.inf.br/bpe";

        private static void Validar(XmlDocument xml, string schema)
        {
            var validador = new ValidarSchema();
            validador.Validar(xml, "BPe." + schema + "_v1.00.xsd", NS);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        private static XmlElement Campo(XmlNode pai, string nome, string valor = null)
        {
            var campo = pai.OwnerDocument.CreateElement(nome, NS);
            if (valor != null)
            {
                campo.InnerText = valor;
            }
            pai.AppendChild(campo);
            return campo;
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData("BPe", false)]
        [InlineData("BPeTA", false)]
        [InlineData("BPeTM", false)]
        [InlineData("BPe", true)]
        [InlineData("BPeTA", true)]
        [InlineData("BPeTM", true)]
        public void NovosGruposPreservamCaminhoOrdemEOpcionalidade(string documento, bool informado)
        {
            var original = new XmlDocument();
            original.Load(Path.Combine(@"..\..\..\BPe\Resources", documento + "_AtualizacaoSchemas.xml"));
            var inf = original.DocumentElement["infBPe"];
            if (informado)
            {
                if (documento == "BPeTM")
                {
                    Campo(inf["ide"], "indFornecimento", "1");
                }
                Campo(Campo(inf["ide"], "gMultaJuros"), "chBPeRef", "35260712345678000195630010000000011234567890");
                Campo(inf["emit"], "tpRegimeIBSCBS", "4");
                var imposto = documento == "BPeTM" ? inf["detBPeTM"]["det"]["imp"] : inf["imp"];
                if (imposto["IBSCBS"] == null)
                {
                    var ibscbs = Campo(imposto, "IBSCBS");
                    Campo(ibscbs, "CST", "000");
                    Campo(ibscbs, "cClassTrib", "000001");
                    if (imposto["vTotDFe"] != null)
                    {
                        imposto.InsertBefore(ibscbs, imposto["vTotDFe"]);
                    }
                }
                var trib = Campo(imposto["IBSCBS"], "gTribSN");
                // O grupo deve anteceder o estorno quando ambos são informados.
                var estorno = imposto["IBSCBS"]["gEstornoCred"];
                if (estorno != null)
                {
                    imposto["IBSCBS"].InsertBefore(trib, estorno);
                }
                var ibs = Campo(trib, "gIBSSN");
                Campo(ibs, "pIBSSN", "1.2345");
                Campo(ibs, "vIBSSN", "0.00");
                var cbs = Campo(trib, "gCBSSN");
                Campo(cbs, "pCBSSN", "2.3456");
                Campo(cbs, "vCBSSN", "2.35");
                if (documento == "BPeTM")
                {
                    var totais = Campo(inf["total"]["IBSCBSTot"], "gTribSN");
                    Campo(totais, "vIBSSN", "0.00");
                    Campo(totais, "vCBSSN", "2.35");
                }
            }

            XmlDocument gerado;
            BPeTMXml.GMultaJuros multa;
            BPeTMXml.Emit emit;
            BPeTMXml.IBSCBS impostos;
            switch (documento)
            {
                case "BPe":
                    var bpe = XMLUtility.Deserializar<Business.DFe.Xml.BPe.BPe>(original);
                    multa = bpe.InfBPe.Ide.GMultaJuros;
                    emit = bpe.InfBPe.Emit;
                    impostos = bpe.InfBPe.Imp.IBSCBS;
                    gerado = bpe.GerarXML();
                    break;
                case "BPeTA":
                    var aereo = XMLUtility.Deserializar<BPeTAXml.BPeTA>(original);
                    multa = aereo.InfBPe.Ide.GMultaJuros;
                    emit = aereo.InfBPe.Emit;
                    impostos = aereo.InfBPe.Imp.IBSCBS;
                    gerado = aereo.GerarXML();
                    break;
                default:
                    var metro = XMLUtility.Deserializar<BPeTMXml.BPeTM>(original);
                    multa = metro.InfBPe.Ide.GMultaJuros;
                    emit = metro.InfBPe.Emit;
                    impostos = metro.InfBPe.DetBPeTM[0].Det[0].Imp.IBSCBS;
                    Assert.Equal(informado, metro.InfBPe.Ide.ShouldSerializeIndFornecimento());
                    Assert.Equal(informado, metro.InfBPe.Total.IBSCBSTot.GTribSN != null);
                    gerado = metro.GerarXML();
                    break;
            }
            Assert.Equal(informado, multa != null);
            Assert.Equal(informado, emit.ShouldSerializeTpRegimeIBSCBS());
            Assert.Equal(informado, impostos?.GTribSN != null);
            if (informado)
            {
                Assert.Equal(TipoRegimeIBSCBS.NormalOpcaoPendenteSimplesNacionalRegimeRegular, emit.TpRegimeIBSCBS);
                Assert.Equal(1.2345, impostos.GTribSN.GIBSSN.PIBSSN);
                Assert.Equal(0, impostos.GTribSN.GIBSSN.VIBSSN);
                Assert.Equal(2.3456, impostos.GTribSN.GCBSSN.PCBSSN);
            }
            Assert.Equal(original.InnerText, gerado.InnerText);
            Assert.All(gerado.GetElementsByTagName("gTribSN").Cast<XmlNode>(), n => Assert.Equal(NS, n.NamespaceURI));
            var schema = documento == "BPe" ? "bpe" : documento == "BPeTA" ? "bpeTA" : "bpeTM";
            Validar(original, schema);
            Validar(gerado, schema);
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(false)]
        [InlineData(true)]
        public void EventoTributacaoPreservaListaZerosOpcionaisEPrecisao(bool informado)
        {
            var evento = XMLUtility.Deserializar<EventoBPe>(File.ReadAllText(@"..\..\..\BPe\Resources\eventoBPe-110301-canc-vinc-pgto.xml"));
            evento.InfEvento.TpEvento = TipoEventoBPe.TributacaoSimplesNacional;
            evento.InfEvento.DetEvento = new DetEventoBPe
            {
                VersaoEvento = "1.00",
                EvTributacaoSN = new EvTributacaoSN
                {
                    NProt = "135260000000001",
                    GTribItemSN = new List<GTribItemSN>
                    {
                        new GTribItemSN { NItem = "1", VRBSNItem = 100, TpRBSN = TipoReceitaBrutaSimplesNacional.ReceitaBrutaInterna },
                        new GTribItemSN { NItem = "2", VRBSNItem = 0, TpRBSN = TipoReceitaBrutaSimplesNacional.NaoReceitaBruta }
                    },
                    GTotalSN = new GTotalSN { VRBSNTot = 100 }
                }
            };
            var detalhe = evento.InfEvento.DetEvento.EvTributacaoSN;
            if (informado)
            {
                detalhe.GTribItemSN[0].PIBSSN = 1.2345;
                detalhe.GTribItemSN[0].VIBSSN = 0;
                detalhe.GTribItemSN[0].PCBSSN = 2.3456;
                detalhe.GTribItemSN[0].VCBSSN = 2.35;
                detalhe.GTribItemSN[0].VIBSPendSusp = 0;
                detalhe.GTribItemSN[0].VCBSPendSusp = 0;
                detalhe.GTotalSN.VIBSSNTot = 0;
                detalhe.GTotalSN.VIBSSNtotPendSusp = 0;
                detalhe.GTotalSN.VCBSSNTot = 2.35;
                detalhe.GTotalSN.VCBSSNtotPendSusp = 0;
            }
            var xml = evento.GerarXML();
            var payload = xml.GetElementsByTagName("evTributacaoSN", NS)[0];
            Assert.Equal(new[] { "descEvento", "nProt", "gTribItemSN", "gTribItemSN", "gTotalSN" }, payload.ChildNodes.Cast<XmlNode>().Select(n => n.LocalName));
            Assert.Equal(informado ? "1.2345" : null, payload["gTribItemSN"]["pIBSSN"]?.InnerText);
            Assert.Equal(informado ? "0.00" : null, payload["gTribItemSN"]["vIBSSN"]?.InnerText);
            var lido = XMLUtility.Deserializar<EventoBPe>(xml);
            Assert.Equal(2, lido.InfEvento.DetEvento.EvTributacaoSN.GTribItemSN.Count);
            Assert.Equal(detalhe.GTribItemSN[0].PIBSSN, lido.InfEvento.DetEvento.EvTributacaoSN.GTribItemSN[0].PIBSSN);
            Assert.Equal(xml.InnerText, lido.GerarXML().InnerText);
            Assert.StartsWith("ID412140", lido.InfEvento.Id);
            var isolado = new XmlDocument();
            isolado.AppendChild(isolado.ImportNode(payload, true));
            Validar(isolado, "evTributacaoSN");
            // O envelope exige assinatura; usa-se a assinatura sintética da massa estrutural.
            var assinado = new XmlDocument();
            assinado.Load(@"..\..\..\BPe\Resources\BPe_AtualizacaoSchemas.xml");
            xml.DocumentElement.AppendChild(xml.ImportNode(assinado.DocumentElement.LastChild, true));
            Validar(xml, "eventoBPe");
        }

        [Fact]
        [Trait("DFe", "BPe")]
        public void CancelamentoTributacaoPreservaProtocoloENomeExato()
        {
            var evento = new EvCancTribSN { NProt = "135260000000001", NProtEvTribSN = "135260000000002" };
            var xml = evento.GerarXML();
            Assert.Equal("evCancTribSN", xml.DocumentElement.LocalName);
            Assert.Equal(new[] { "descEvento", "nProt", "nProtEvTribSN" }, xml.DocumentElement.ChildNodes.Cast<XmlNode>().Select(n => n.LocalName));
            Assert.Equal(evento.NProtEvTribSN, XMLUtility.Deserializar<EvCancTribSN>(xml).NProtEvTribSN);
            Validar(xml, "evCancTributacaoSN");
            var envelope = new EventoBPe
            {
                InfEvento = new InfEventoBPe
                {
                    TpEvento = TipoEventoBPe.CancelamentoTributacaoSimplesNacional,
                    TpAmb = TipoAmbiente.Homologacao,
                    DetEvento = new DetEventoBPe { EvCancTribSN = evento }
                }
            };
            var lido = XMLUtility.Deserializar<EventoBPe>(envelope.GerarXML());
            Assert.StartsWith("ID412141", lido.InfEvento.Id);
            Assert.Equal(evento.NProtEvTribSN, lido.InfEvento.DetEvento.EvCancTribSN.NProtEvTribSN);
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(false, false)]
        [InlineData(true, false)]
        [InlineData(false, true)]
        [InlineData(true, true)]
        public void ExcessoBagagemReutilizaTributacaoComSubgruposIndependentes(bool comIBS, bool comCBS)
        {
            var evento = new EvExcessoBagagem
            {
                NProt = "135260000000001",
                QBagagem = "2",
                VTotBag = 100,
                IBSCBS = new BPeTMXml.IBSCBS
                {
                    CST = "000",
                    CClassTrib = "000001",
                    GTribSN = new BPeTMXml.GTribSN
                    {
                        GIBSSN = comIBS ? new BPeTMXml.GIBSSN { PIBSSN = 1.2345, VIBSSN = 0 } : null,
                        GCBSSN = comCBS ? new BPeTMXml.GCBSSN { PCBSSN = 2.3456, VCBSSN = 2.35 } : null
                    },
                    GEstornoCred = new BPeTMXml.GEstornoCred { VIBSEstCred = 0, VCBSEstCred = 0 }
                }
            };
            var xml = evento.GerarXML();
            var trib = xml.DocumentElement["IBSCBS"]["gTribSN"];
            Assert.Equal("gEstornoCred", trib.NextSibling.LocalName);
            Assert.Equal(comIBS, trib["gIBSSN"] != null);
            Assert.Equal(comCBS, trib["gCBSSN"] != null);
            var lido = XMLUtility.Deserializar<EvExcessoBagagem>(xml);
            Assert.Equal(comIBS, lido.IBSCBS.GTribSN.GIBSSN != null);
            Assert.Equal(comCBS, lido.IBSCBS.GTribSN.GCBSSN != null);
            Assert.Equal(xml.InnerText, lido.GerarXML().InnerText);
            Validar(xml, "evExcessoBagagem");
        }
    }
}
