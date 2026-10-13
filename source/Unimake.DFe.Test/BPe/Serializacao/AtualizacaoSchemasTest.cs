using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using BPeXml = Unimake.Business.DFe.Xml.BPe;
using BPeTAXml = Unimake.Business.DFe.Xml.BPeTA;
using BPeTMXml = Unimake.Business.DFe.Xml.BPeTM;
using Xunit;

namespace Unimake.DFe.Test.BPe.Serializacao
{
    public class AtualizacaoSchemasTest
    {
        private const string NamespaceBPe = "http://www.portalfiscal.inf.br/bpe";

        private static XmlDocument LerModelo(string documento)
        {
            var xml = new XmlDocument();
            xml.Load(Path.Combine(@"..\..\..\BPe\Resources", documento + "_AtualizacaoSchemas.xml"));
            return xml;
        }

        private static void Validar(XmlDocument xml, string schema, bool valido = true, string tagInvalida = null)
        {
            var validador = new ValidarSchema();
            validador.Validar(xml, "BPe." + schema + "_v1.00.xsd", NamespaceBPe);
            Assert.True(validador.Success == valido, validador.ErrorMessage);
            if (!valido)
            {
                Assert.Contains(tagInvalida, validador.ErrorMessage);
            }
        }

        private static void ConferirValor(XmlNode grupo, double? valor, string proximaTag)
        {
            var campo = grupo["vBPLiq", NamespaceBPe];
            if (!valor.HasValue)
            {
                Assert.Null(campo);
                return;
            }

            Assert.NotNull(campo);
            Assert.Equal(NamespaceBPe, campo.NamespaceURI);
            Assert.Equal("vBP", campo.PreviousSibling.LocalName);
            Assert.Equal(proximaTag, campo.NextSibling.LocalName);
            Assert.Equal(valor.Value.ToString("F2", CultureInfo.InvariantCulture), campo.InnerText);
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData("BPe", 81.82d)]
        [InlineData("BPeTA", 99.70d)]
        [InlineData("BPeTM", 9.90d)]
        public void DesserializacaoPreservaValoresLiquidosDoXML(string documento, double esperado)
        {
            var original = LerModelo(documento);
            XmlDocument gerado;
            switch (documento)
            {
                case "BPe":
                    var bpe = XMLUtility.Deserializar<BPeXml.BPe>(original);
                    Assert.Equal(esperado, bpe.InfBPe.InfValorBPe.VBPLiq);
                    gerado = bpe.GerarXML();
                    break;
                case "BPeTA":
                    var bpeTA = XMLUtility.Deserializar<BPeTAXml.BPeTA>(original);
                    Assert.Equal(esperado, bpeTA.InfBPe.InfValorBPe.VBPLiq);
                    gerado = bpeTA.GerarXML();
                    break;
                default:
                    var bpeTM = XMLUtility.Deserializar<BPeTMXml.BPeTM>(original);
                    Assert.Equal(esperado, bpeTM.InfBPe.DetBPeTM[0].Det[0].VBPLiq);
                    Assert.Equal(esperado, bpeTM.InfBPe.Total.VBPLiq);
                    gerado = bpeTM.GerarXML();
                    break;
            }

            Assert.Equal(original.InnerText, gerado.InnerText);
            var schema = documento == "BPe" ? "bpe" : documento == "BPeTA" ? "bpeTA" : "bpeTM";
            Validar(original, schema);
            Validar(gerado, schema);
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(null)]
        [InlineData(0d)]
        [InlineData(81.82d)]
        public void ValorLiquidoBPePreservaAusenciaZeroEOrdem(double? valor)
        {
            var modelo = XMLUtility.Deserializar<BPeXml.BPe>(LerModelo("BPe"));
            modelo.InfBPe.InfValorBPe.VBPLiq = valor;
            var xml = modelo.GerarXML();

            ConferirValor(xml.DocumentElement["infBPe"]["infValorBPe"], valor, "vDesconto");
            Assert.Equal(valor.HasValue ? 1 : 0, xml.GetElementsByTagName("vBPLiq", NamespaceBPe).Count);
            Assert.Equal(valor, XMLUtility.Deserializar<BPeXml.BPe>(xml).InfBPe.InfValorBPe.VBPLiq);
            Validar(xml, "bpe");
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(null)]
        [InlineData(0d)]
        [InlineData(99.70d)]
        public void ValorLiquidoBPeTAPreservaAusenciaZeroEOrdem(double? valor)
        {
            var modelo = XMLUtility.Deserializar<BPeTAXml.BPeTA>(LerModelo("BPeTA"));
            modelo.InfBPe.InfValorBPe.VBPLiq = valor;
            var xml = modelo.GerarXML();

            ConferirValor(xml.DocumentElement["infBPe"]["infValorBPe"], valor, "vDesconto");
            Assert.Equal(valor.HasValue ? 1 : 0, xml.GetElementsByTagName("vBPLiq", NamespaceBPe).Count);
            Assert.Equal(valor, XMLUtility.Deserializar<BPeTAXml.BPeTA>(xml).InfBPe.InfValorBPe.VBPLiq);
            Validar(xml, "bpeTA");
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(null)]
        [InlineData(0d)]
        [InlineData(9.90d)]
        public void ValorLiquidoBPeTMPreservaAusenciaZeroEOrdem(double? valor)
        {
            var modelo = XMLUtility.Deserializar<BPeTMXml.BPeTM>(LerModelo("BPeTM"));
            modelo.InfBPe.DetBPeTM[0].Det[0].VBPLiq = valor;
            modelo.InfBPe.Total.VBPLiq = valor;
            var xml = modelo.GerarXML();
            var infBPe = xml.DocumentElement["infBPe"];

            ConferirValor(infBPe["detBPeTM"]["det"], valor, "imp");
            ConferirValor(infBPe["total"], valor, "ICMSTot");
            Assert.Equal(valor.HasValue ? 2 : 0, xml.GetElementsByTagName("vBPLiq", NamespaceBPe).Count);
            var lido = XMLUtility.Deserializar<BPeTMXml.BPeTM>(xml);
            Assert.Equal(valor, lido.InfBPe.DetBPeTM[0].Det[0].VBPLiq);
            Assert.Equal(valor, lido.InfBPe.Total.VBPLiq);
            Validar(xml, "bpeTM");
        }

        [Fact]
        [Trait("DFe", "BPe")]
        public void DetalhesBPeTMPreservamValoresLiquidosIndependentes()
        {
            var modelo = XMLUtility.Deserializar<BPeTMXml.BPeTM>(LerModelo("BPeTM"));
            var primeiro = modelo.InfBPe.DetBPeTM[0].Det[0];
            primeiro.VBPLiq = 0;
            modelo.InfBPe.DetBPeTM[0].Det.Add(new BPeTMXml.Det
            {
                NViagem = 2,
                CMunIni = primeiro.CMunIni,
                QPass = primeiro.QPass,
                VBP = primeiro.VBP,
                VBPLiq = 9.90,
                Imp = primeiro.Imp,
                Comp = primeiro.Comp
            });
            modelo.InfBPe.Total.QPass = 2;
            modelo.InfBPe.Total.VBP = 20;
            var xml = modelo.GerarXML();
            var lido = XMLUtility.Deserializar<BPeTMXml.BPeTM>(xml);

            Assert.Equal(2, lido.InfBPe.DetBPeTM[0].Det.Count);
            Assert.Equal(0d, lido.InfBPe.DetBPeTM[0].Det[0].VBPLiq);
            Assert.Equal(9.90d, lido.InfBPe.DetBPeTM[0].Det[1].VBPLiq);
            Assert.Equal(9.90d, lido.InfBPe.Total.VBPLiq);
            Validar(xml, "bpeTM");
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData("BPe")]
        [InlineData("BPeTM")]
        public void DocumentosProcessadosPreservamValoresLiquidos(string documento)
        {
            var retorno = XMLUtility.Deserializar<BPeXml.RetBPe>(File.ReadAllText(@"..\..\..\BPe\Resources\retBPe.xml"));
            XmlDocument xml;
            if (documento == "BPe")
            {
                var processado = new BPeXml.BPeProc
                {
                    Versao = "1.00",
                    BPe = XMLUtility.Deserializar<BPeXml.BPe>(LerModelo(documento)),
                    ProtBPe = retorno.ProtBPe
                };
                xml = processado.GerarXML();
                var lido = XMLUtility.Deserializar<BPeXml.BPeProc>(xml);
                Assert.Equal(81.82d, lido.BPe.InfBPe.InfValorBPe.VBPLiq);
                Assert.Equal(xml.InnerText, lido.GerarXML().InnerText);
                Validar(xml, "procBPe");
            }
            else
            {
                var processado = new BPeTMXml.BPeTMProc
                {
                    Versao = "1.00",
                    BPeTM = XMLUtility.Deserializar<BPeTMXml.BPeTM>(LerModelo(documento)),
                    ProtBPe = retorno.ProtBPe
                };
                xml = processado.GerarXML();
                var lido = XMLUtility.Deserializar<BPeTMXml.BPeTMProc>(xml);
                Assert.Equal(9.90d, lido.BPeTM.InfBPe.DetBPeTM[0].Det[0].VBPLiq);
                Assert.Equal(9.90d, lido.BPeTM.InfBPe.Total.VBPLiq);
                Assert.Equal(xml.InnerText, lido.GerarXML().InnerText);
                Validar(xml, "procBPeTM");
            }
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData("CNPJ", false)]
        [InlineData("CNPJ", true)]
        [InlineData("CPF", false)]
        [InlineData("CPF", true)]
        [InlineData("idEstrangeiro", false)]
        [InlineData("idEstrangeiro", true)]
        public void CompradorBPeTAAceitaEnderecoOpcional(string identificacao, bool comEndereco)
        {
            var modelo = XMLUtility.Deserializar<BPeTAXml.BPeTA>(LerModelo("BPeTA"));
            modelo.InfBPe.Comp = new BPeTAXml.Comp
            {
                XNome = "COMPRADOR TESTE",
                CNPJ = identificacao == "CNPJ" ? "00000000000000" : null,
                CPF = identificacao == "CPF" ? "00000000000" : null,
                IdEstrangeiro = identificacao == "idEstrangeiro" ? "EXT123456" : null,
                EnderComp = comEndereco ? new BPeTAXml.EnderComp
                {
                    XLgr = "RUA EXEMPLO",
                    Nro = "100",
                    XBairro = "CENTRO",
                    CMun = "3550308",
                    XMun = "SAO PAULO",
                    UF = UFBrasil.SP
                } : null
            };
            var xml = modelo.GerarXML();
            var comp = xml.DocumentElement["infBPe"]["comp"];
            Assert.Equal(identificacao, comp["xNome"].NextSibling.LocalName);
            Assert.Equal(1, comp.SelectNodes("*[local-name()='CNPJ' or local-name()='CPF' or local-name()='idEstrangeiro']").Count);
            Assert.Equal(comEndereco, comp["enderComp"] != null);
            var lido = XMLUtility.Deserializar<BPeTAXml.BPeTA>(xml);
            Assert.Equal(modelo.InfBPe.Comp.CNPJ, lido.InfBPe.Comp.CNPJ);
            Assert.Equal(modelo.InfBPe.Comp.CPF, lido.InfBPe.Comp.CPF);
            Assert.Equal(modelo.InfBPe.Comp.IdEstrangeiro, lido.InfBPe.Comp.IdEstrangeiro);
            Assert.Equal(comEndereco, lido.InfBPe.Comp.EnderComp != null);
            if (comEndereco)
            {
                Assert.Equal("RUA EXEMPLO", lido.InfBPe.Comp.EnderComp.XLgr);
            }
            Validar(xml, "bpeTA");
        }

        [Fact]
        [Trait("DFe", "BPe")]
        public void CompradorBPeContinuaExigindoEndereco()
        {
            var modelo = XMLUtility.Deserializar<BPeXml.BPe>(LerModelo("BPe"));
            modelo.InfBPe.Comp = new BPeXml.Comp { CPF = "00000000000", XNome = "COMPRADOR TESTE" };
            Validar(modelo.GerarXML(), "bpe", false, "enderComp");
        }

        public static IEnumerable<object[]> CodigosBeneficioFiscal()
        {
            foreach (var documento in new[] { "BPe", "BPeTM" })
            {
                foreach (var grupo in new[] { "ICMS20", "ICMS45", "ICMS90" })
                {
                    foreach (var codigo in new[] { "SP123456", "SP12345678", "SP12&456", "SP12345ÿ", "SP12345", "SP1234567", "SP123456789", "SP12 456" })
                    {
                        var valido = codigo == "SP123456" || codigo == "SP12345678" || codigo == "SP12&456" || codigo == "SP12345ÿ";
                        yield return new object[] { documento, grupo, codigo, valido };
                    }
                }
            }
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [MemberData(nameof(CodigosBeneficioFiscal))]
        public void BeneficioFiscalRespeitaNovoPatternNasRaizesConsumidoras(string documento, string grupo, string codigo, bool valido)
        {
            var imposto = new BPeTMXml.ICMS();
            switch (grupo)
            {
                case "ICMS20":
                    imposto.ICMS20 = new BPeTMXml.ICMS20 { CST = CSTICMSBPe.TributacaoComReducaoBC, PRedBC = 10, VBC = 100, PICMS = 12, VICMS = 12, VICMSDeson = 1, CBenef = codigo };
                    break;
                case "ICMS45":
                    imposto.ICMS45 = new BPeTMXml.ICMS45 { CST = CSTICMSBPe.Isencao, VICMSDeson = 1, CBenef = codigo };
                    break;
                default:
                    imposto.ICMS90 = new BPeTMXml.ICMS90 { CST = CSTICMSBPe.Outros, VBC = 100, PICMS = 12, VICMS = 12, VICMSDeson = 1, CBenef = codigo };
                    break;
            }

            XmlDocument xml;
            BPeTMXml.ICMS lido;
            if (documento == "BPe")
            {
                var modelo = XMLUtility.Deserializar<BPeXml.BPe>(LerModelo(documento));
                modelo.InfBPe.Imp.ICMS = imposto;
                xml = modelo.GerarXML();
                lido = XMLUtility.Deserializar<BPeXml.BPe>(xml).InfBPe.Imp.ICMS;
            }
            else
            {
                var modelo = XMLUtility.Deserializar<BPeTMXml.BPeTM>(LerModelo(documento));
                modelo.InfBPe.DetBPeTM[0].Det[0].Imp.ICMS = imposto;
                xml = modelo.GerarXML();
                lido = XMLUtility.Deserializar<BPeTMXml.BPeTM>(xml).InfBPe.DetBPeTM[0].Det[0].Imp.ICMS;
            }

            var campo = xml.GetElementsByTagName("cBenef", NamespaceBPe)[0];
            Assert.Equal(grupo, campo.ParentNode.LocalName);
            Assert.Equal(codigo, campo.InnerText);
            var codigoLido = grupo == "ICMS20" ? lido.ICMS20.CBenef : grupo == "ICMS45" ? lido.ICMS45.CBenef : lido.ICMS90.CBenef;
            Assert.Equal(codigo, codigoLido);
            Validar(xml, documento == "BPe" ? "bpe" : "bpeTM", valido, "cBenef");
        }
    }
}
