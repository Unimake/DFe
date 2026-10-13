using System.IO;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFGas;
using Xunit;
using NFGasXml = Unimake.Business.DFe.Xml.NFGas.NFGas;

namespace Unimake.DFe.Test.NFGas.Serializacao
{
    public class AtualizacaoSchemaTest
    {
        private const string NamespaceNFGas = "http://www.portalfiscal.inf.br/nfgas";
        private const string CaminhoXML = @"..\..\..\NFGas\Resources\nfgas-atualizacao-schema.xml";

        [Fact]
        [Trait("DFe", "NFGas")]
        public void DevePreservarNovosCamposEValidarSchemaDaNFGas()
        {
            var original = new XmlDocument();
            original.Load(CaminhoXML);
            var modelo = XMLUtility.Deserializar<NFGasXml>(original);

            ConferirNovosCampos(modelo);

            var gerado = modelo.GerarXML();
            Assert.Equal(original.InnerText, gerado.InnerText);
            ConferirOrdemENamespace(gerado);
            Validar(gerado, "nfgas_v1.00.xsd");
            ConferirNovosCampos(XMLUtility.Deserializar<NFGasXml>(gerado));
        }

        [Fact]
        [Trait("DFe", "NFGas")]
        public void DevePreservarNovosCamposNoDocumentoProcessado()
        {
            var modelo = LerModelo();
            var processo = XMLUtility.Deserializar<NFGasProc>(File.ReadAllText(@"..\..\..\NFGas\Resources\nfgasProc.xml"));
            processo.NFGas = modelo;
            processo.ProtNFGas.InfProt.ChNFGas = modelo.InfNFGas.Chave;
            processo.ProtNFGas.InfProt.NProt = "1352600000000001";

            var gerado = processo.GerarXML();
            var relido = XMLUtility.Deserializar<NFGasProc>(gerado);

            ConferirNovosCampos(relido.NFGas);
            Assert.Equal(gerado.InnerText, relido.GerarXML().InnerText);
            Validar(gerado, "procNFGas_v1.00.xsd");
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("0.00")]
        public void DeveDistinguirAusenciaEZeroNosValoresLiquidos(string valor)
        {
            var modelo = LerModelo();
            var normal = modelo.InfNFGas.Det[0].GNormal;
            normal.Prod.VItemLiq = valor;
            normal.Prod.VProdLiq = valor;
            modelo.InfNFGas.Total.VProdLiq = valor;
            normal.GProcRef.GIBSCBS = null;
            normal.Imposto.GICMSPrevistoPagtoAntecip = null;
            normal.Imposto.Orig = (OrigemMercadoria)(-1);
            normal.Imposto.IndSemCST = "1";

            var gerado = modelo.GerarXML();
            var presente = !string.IsNullOrEmpty(valor);
            Assert.Equal(presente ? 1 : 0, gerado.GetElementsByTagName("vItemLiq", NamespaceNFGas).Count);
            Assert.Equal(presente ? 2 : 0, gerado.GetElementsByTagName("vProdLiq", NamespaceNFGas).Count);
            Assert.Empty(gerado.GetElementsByTagName("gICMSPrevistoPagtoAntecip", NamespaceNFGas).Cast<XmlNode>());
            Assert.Empty(gerado.GetElementsByTagName("gProcRef", NamespaceNFGas)[0].ChildNodes.Cast<XmlNode>().Where(x => x.LocalName == "gIBSCBS"));

            var relido = XMLUtility.Deserializar<NFGasXml>(gerado);
            Assert.Equal(presente ? valor : null, relido.InfNFGas.Det[0].GNormal.Prod.VItemLiq);
            Assert.Equal(presente ? valor : null, relido.InfNFGas.Total.VProdLiq);
            Validar(gerado, "nfgas_v1.00.xsd");
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(0)]
        [InlineData(6)]
        [InlineData(7)]
        [InlineData(20)]
        [InlineData(21)]
        public void DeveAplicarNovoLimiteDeTarifas(int quantidade)
        {
            var modelo = LerModelo();
            var normal = modelo.InfNFGas.Det[0].GNormal;
            var tarifas = normal.GTarif;
            normal.GTarif = Enumerable.Range(0, quantidade).Select(x => tarifas[x % tarifas.Count]).ToList();

            var gerado = modelo.GerarXML();
            Assert.Equal(quantidade, gerado.GetElementsByTagName("gTarif", NamespaceNFGas).Count);
            var relido = XMLUtility.Deserializar<NFGasXml>(gerado);
            Assert.Equal(quantidade, relido.InfNFGas.Det[0].GNormal.GTarif?.Count ?? 0);

            var validador = CriarValidador(gerado, "nfgas_v1.00.xsd");
            Assert.True(validador.Success == (quantidade <= 20), validador.ErrorMessage);
            if (quantidade > 20)
            {
                Assert.Contains("gTarif", validador.ErrorMessage);
            }
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(1)]
        [InlineData(2)]
        [InlineData(3)]
        [InlineData(4)]
        [InlineData(5)]
        [InlineData(6)]
        [InlineData(7)]
        [InlineData(8)]
        public void DevePreservarDominiosAnteriorENovoDaOrigemQuantidade(int codigo)
        {
            var modelo = LerModelo();
            modelo.InfNFGas.Det[0].GNormal.Prod.IndOrigemQtd = (OrigemQuantidadeFaturadaNFGas)codigo;
            var gerado = modelo.GerarXML();

            Assert.Equal(codigo.ToString(), gerado.GetElementsByTagName("indOrigemQtd", NamespaceNFGas)[0].InnerText);
            Assert.Equal((OrigemQuantidadeFaturadaNFGas)codigo, XMLUtility.Deserializar<NFGasXml>(gerado).InfNFGas.Det[0].GNormal.Prod.IndOrigemQtd);
            Validar(gerado, "nfgas_v1.00.xsd");
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData("PR123456", true)]
        [InlineData("PR12345678", true)]
        [InlineData("PR12345", false)]
        [InlineData("PR1234567", false)]
        [InlineData("PR123456789", false)]
        [InlineData("PR 23456", false)]
        public void DeveValidarNovoFormatoDoBeneficioNosCincoGruposICMS(string codigo, bool valido)
        {
            foreach (var grupo in new[] { "ICMS20", "ICMS40", "ICMS51", "ICMS60", "ICMS90" })
            {
                var xml = new XmlDocument();
                xml.Load(CaminhoXML);
                var imposto = (XmlElement)xml.GetElementsByTagName("imposto", NamespaceNFGas)[0];
                imposto.RemoveChild(imposto.GetElementsByTagName("gICMSPrevistoPagtoAntecip", NamespaceNFGas)[0]);
                var icms = xml.CreateElement(grupo, NamespaceNFGas);
                imposto.InsertBefore(icms, imposto.GetElementsByTagName("IBSCBS", NamespaceNFGas)[0]);
                Adicionar(icms, "CST", grupo.Substring(4));

                if (grupo == "ICMS20")
                {
                    Adicionar(icms, "pRedBC", "0.00");
                }

                if (grupo == "ICMS20" || grupo == "ICMS51" || grupo == "ICMS90")
                {
                    Adicionar(icms, "vBC", "100.00");
                    Adicionar(icms, "pICMS", "18.00");
                    Adicionar(icms, "vICMS", "18.00");
                }

                if (grupo == "ICMS51")
                {
                    Adicionar(icms, "pDif", "0.00");
                    Adicionar(icms, "vICMSOp", "18.00");
                    Adicionar(icms, "vICMSDif", "0.00");
                }
                else
                {
                    if (grupo == "ICMS60")
                    {
                        Adicionar(icms, "pRedBCEfet", "0.00");
                        Adicionar(icms, "vBCEfet", "100.00");
                        Adicionar(icms, "pICMSEfet", "18.00");
                        Adicionar(icms, "vICMSEfet", "18.00");
                    }

                    Adicionar(icms, "vICMSDeson", "1.00");
                }

                Adicionar(icms, "cBenef", codigo);
                var validador = CriarValidador(xml, "nfgas_v1.00.xsd");
                Assert.True(validador.Success == valido, grupo + ": " + validador.ErrorMessage);
                if (!valido)
                {
                    Assert.Contains("cBenef", validador.ErrorMessage);
                }
            }
        }

        private static NFGasXml LerModelo() => XMLUtility.Deserializar<NFGasXml>(File.ReadAllText(CaminhoXML));

        private static void ConferirNovosCampos(NFGasXml modelo)
        {
            var normal = modelo.InfNFGas.Det[0].GNormal;
            Assert.Equal(OrigemQuantidadeFaturadaNFGas.Minimo, normal.Prod.IndOrigemQtd);
            Assert.Equal("8.1234567890", normal.Prod.VItemLiq);
            Assert.Equal("81.2345678900", normal.Prod.VProdLiq);
            Assert.Equal("81.23", modelo.InfNFGas.Total.VProdLiq);
            Assert.Equal("18.25", normal.Imposto.GICMSPrevistoPagtoAntecip.VICMSPrevisto);
            Assert.Equal("100.12", normal.GProcRef.GIBSCBS.VBC);
            Assert.Equal("1.23", normal.GProcRef.GIBSCBS.VIBSUF);
            Assert.Equal("0.45", normal.GProcRef.GIBSCBS.VIBSMun);
            Assert.Equal("8.76", normal.GProcRef.GIBSCBS.VCBS);
            Assert.Equal(20, normal.GTarif.Count);
        }

        private static void ConferirOrdemENamespace(XmlDocument xml)
        {
            var ns = new XmlNamespaceManager(xml.NameTable);
            ns.AddNamespace("n", NamespaceNFGas);
            var prod = xml.SelectSingleNode("/n:NFGas/n:infNFGas/n:det/n:gNormal/n:prod", ns);
            Assert.Equal("vItemLiq", prod["vProd"].NextSibling.LocalName);
            Assert.Equal("vProdLiq", prod["vItemLiq"].NextSibling.LocalName);
            Assert.Equal("indDevolucao", prod["vProdLiq"].NextSibling.LocalName);
            var processo = xml.SelectSingleNode("/n:NFGas/n:infNFGas/n:det/n:gNormal/n:gProcRef", ns);
            Assert.Equal("gIBSCBS", processo["indDevolucao"].NextSibling.LocalName);
            Assert.Equal("gProc", processo["gIBSCBS"].NextSibling.LocalName);
            Assert.Equal(new[] { "vBC", "vIBSUF", "vIBSMun", "vCBS" }, processo["gIBSCBS"].ChildNodes.Cast<XmlNode>().Select(x => x.LocalName));
            var imposto = xml.SelectSingleNode("/n:NFGas/n:infNFGas/n:det/n:gNormal/n:imposto", ns);
            Assert.Equal("gICMSPrevistoPagtoAntecip", imposto["orig"].NextSibling.LocalName);
            Assert.Equal("IBSCBS", imposto["gICMSPrevistoPagtoAntecip"].NextSibling.LocalName);
            var total = xml.SelectSingleNode("/n:NFGas/n:infNFGas/n:total", ns);
            Assert.Equal("vProdLiq", total["vProd"].NextSibling.LocalName);
            Assert.Equal("ICMSTot", total["vProdLiq"].NextSibling.LocalName);
            Assert.All(processo["gIBSCBS"].ChildNodes.Cast<XmlNode>(), x => Assert.Equal(NamespaceNFGas, x.NamespaceURI));
        }

        private static void Adicionar(XmlElement pai, string nome, string valor)
        {
            var filho = pai.OwnerDocument.CreateElement(nome, NamespaceNFGas);
            filho.InnerText = valor;
            pai.AppendChild(filho);
        }

        private static ValidarSchema CriarValidador(XmlDocument xml, string schema)
        {
            var validador = new ValidarSchema();
            validador.Validar(xml, "NFGas." + schema, NamespaceNFGas);
            return validador;
        }

        private static void Validar(XmlDocument xml, string schema)
        {
            var validador = CriarValidador(xml, schema);
            Assert.True(validador.Success, validador.ErrorMessage);
        }
    }
}
