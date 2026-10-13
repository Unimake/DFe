using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NF3e;
using Xunit;
using ModeloNF3e = Unimake.Business.DFe.Xml.NF3e.NF3e;

namespace Unimake.DFe.Test.NF3e.Serializacao
{
    public class AtualizacaoSchemasTest
    {
        private const string NamespaceNF3e = "http://www.portalfiscal.inf.br/nf3e";
        private const string ArquivoModelo = @"..\..\..\NF3e\Resources\NF3e_AtualizacaoSchemas.xml";
        private const string CaminhoItem = "/n:NF3e/n:infNF3e/n:NFdet/n:det/n:detItem";

        private static ModeloNF3e LerModelo() => XMLUtility.Deserializar<ModeloNF3e>(File.ReadAllText(ArquivoModelo));

        private static DetItem Item(ModeloNF3e modelo) => modelo.InfNF3e.NFdet[0].Det[1].DetItem;

        private static XmlNode Selecionar(XmlDocument xml, string caminho)
        {
            var namespaces = new XmlNamespaceManager(xml.NameTable);
            namespaces.AddNamespace("n", NamespaceNF3e);
            return xml.SelectSingleNode(caminho, namespaces);
        }

        private static void Validar(XmlDocument xml, string schema = "NF3e.nf3e_v1.00.xsd")
        {
            var validador = new ValidarSchema();
            validador.Validar(xml, schema, NamespaceNF3e);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        [Fact]
        [Trait("DFe", "NF3e")]
        public void NovoLeiautePreservaConteudoNoRoundTrip()
        {
            var original = new XmlDocument();
            original.Load(ArquivoModelo);
            Validar(original);
            var modelo = XMLUtility.Deserializar<ModeloNF3e>(original);
            Assert.Equal(80.1234567891m, Item(modelo).Prod.VItemLiq);
            Assert.Equal(80.1234567891m, Item(modelo).Prod.VProdLiq);
            Assert.Equal(80.12d, modelo.InfNF3e.Total.VProdLiq);
            Assert.Equal(18.25d, Item(modelo).Imposto.GICMSPrevistoPagtoAntecip.VICMSPrevisto);
            Assert.Equal(100.12d, Item(modelo).GProcRef.GIBSCBS.VBC);
            Assert.Equal(0.10d, Item(modelo).GProcRef.GIBSCBS.VIBSUF);
            Assert.Equal(0d, Item(modelo).GProcRef.GIBSCBS.VIBSMun);
            Assert.Equal(0.90d, Item(modelo).GProcRef.GIBSCBS.VCBS);
            Assert.Equal(2, Item(modelo).GProcRef.GProc.Count);
            Assert.NotNull(Item(modelo).Imposto.IBSCBS.GIBSCBS.GIBSUF);
            var gerado = modelo.GerarXML();
            Assert.Equal(original.InnerText, gerado.InnerText);
            Assert.Equal(gerado.InnerText, XMLUtility.Deserializar<ModeloNF3e>(gerado).GerarXML().InnerText);
            Assert.Null(Selecionar(gerado, "//n:detItemAnt/n:vItemLiq"));
            Assert.Null(Selecionar(gerado, "//n:detItemAnt/n:vProdLiq"));
            Validar(gerado);
        }

        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(null)]
        [InlineData("0.00")]
        [InlineData("1234567890123.1234567891")]
        public void ValoresLiquidosDoItemPreservamAusenciaZeroPrecisaoEOrdem(string valor)
        {
            var modelo = LerModelo();
            decimal? numero = valor == null ? (decimal?)null : decimal.Parse(valor, CultureInfo.InvariantCulture);
            Item(modelo).Prod.VItemLiq = numero;
            Item(modelo).Prod.VProdLiq = numero;
            var xml = modelo.GerarXML();
            var unitario = Selecionar(xml, CaminhoItem + "/n:prod/n:vItemLiq");
            var total = Selecionar(xml, CaminhoItem + "/n:prod/n:vProdLiq");
            Assert.Equal(numero.HasValue, unitario != null);
            Assert.Equal(numero.HasValue, total != null);
            if (numero.HasValue)
            {
                Assert.Equal(valor, unitario.InnerText);
                Assert.Equal(valor, total.InnerText);
                Assert.Equal("vProd", unitario.PreviousSibling.LocalName);
                Assert.Same(total, unitario.NextSibling);
                Assert.Equal("indDevolucao", total.NextSibling.LocalName);
            }
            var lido = XMLUtility.Deserializar<ModeloNF3e>(xml);
            Assert.Equal(numero, Item(lido).Prod.VItemLiq);
            Assert.Equal(numero, Item(lido).Prod.VProdLiq);
            Validar(xml);
        }

        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(null)]
        [InlineData(0d)]
        [InlineData(100.25d)]
        public void TotalLiquidoPreservaAusenciaZeroEOrdem(double? valor)
        {
            var modelo = LerModelo();
            modelo.InfNF3e.Total.VProdLiq = valor;
            var xml = modelo.GerarXML();
            var campo = Selecionar(xml, "/n:NF3e/n:infNF3e/n:total/n:vProdLiq");
            Assert.Equal(valor.HasValue, campo != null);
            if (campo != null)
            {
                Assert.Equal(valor.Value.ToString("F2", CultureInfo.InvariantCulture), campo.InnerText);
                Assert.Equal("vProd", campo.PreviousSibling.LocalName);
                Assert.Equal("ICMSTot", campo.NextSibling.LocalName);
            }
            Assert.Equal(valor, XMLUtility.Deserializar<ModeloNF3e>(xml).InfNF3e.Total.VProdLiq);
            Validar(xml);
        }

        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(0d)]
        [InlineData(125.25d)]
        public void ICMSPrevistoSerializaAlternativaComZero(double valor)
        {
            var modelo = LerModelo();
            Item(modelo).Imposto.GICMSPrevistoPagtoAntecip.VICMSPrevisto = valor;
            var xml = modelo.GerarXML();
            var grupo = Selecionar(xml, CaminhoItem + "/n:imposto/n:gICMSPrevistoPagtoAntecip");
            Assert.Equal(valor.ToString("F2", CultureInfo.InvariantCulture), grupo.FirstChild.InnerText);
            Assert.Null(Selecionar(xml, CaminhoItem + "/n:imposto/n:ICMS00"));
            Assert.Equal("PIS", grupo.NextSibling.LocalName);
            Assert.Equal(valor, Item(XMLUtility.Deserializar<ModeloNF3e>(xml)).Imposto.GICMSPrevistoPagtoAntecip.VICMSPrevisto);
            Validar(xml);
        }

        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(false)]
        [InlineData(true)]
        public void IBSCBSDoProcessoPreservaAusenciaZeroEOrdem(bool informar)
        {
            var modelo = LerModelo();
            Item(modelo).GProcRef.GIBSCBS = informar ? new GIBSCBSProcRef() : null;
            var xml = modelo.GerarXML();
            var grupo = Selecionar(xml, CaminhoItem + "/n:gProcRef/n:gIBSCBS");
            Assert.Equal(informar, grupo != null);
            if (grupo != null)
            {
                Assert.Equal("vCOFINSEfet", grupo.PreviousSibling.LocalName);
                Assert.Equal("gProc", grupo.NextSibling.LocalName);
                Assert.Equal(new[] { "vBC", "vIBSUF", "vIBSMun", "vCBS" }, grupo.ChildNodes.Cast<XmlNode>().Select(n => n.LocalName));
                Assert.All(grupo.ChildNodes.Cast<XmlNode>(), n => Assert.Equal("0.00", n.InnerText));
                Assert.Equal(0d, Item(XMLUtility.Deserializar<ModeloNF3e>(xml)).GProcRef.GIBSCBS.VCBS);
            }
            else
            {
                Assert.Null(Item(XMLUtility.Deserializar<ModeloNF3e>(xml)).GProcRef.GIBSCBS);
            }
            Assert.Equal(2, Item(XMLUtility.Deserializar<ModeloNF3e>(xml)).GProcRef.GProc.Count);
            Validar(xml);
        }

        public static IEnumerable<object[]> CodigosBeneficio()
        {
            foreach (var cst in new[] { "20", "40", "51", "60", "90" })
            {
                foreach (var codigo in new[] { "PR123456", "PR12345678", "PR1234567", "PR 23456", "PR12345", "PR€23456" })
                {
                    yield return new object[] { cst, codigo, codigo == "PR123456" || codigo == "PR12345678" };
                }
            }
        }

        [Theory]
        [Trait("DFe", "NF3e")]
        [MemberData(nameof(CodigosBeneficio))]
        public void SchemaRestringeBeneficioNosCincoGruposICMS(string cst, string codigo, bool valido)
        {
            var xml = LerModelo().GerarXML();
            var imposto = Selecionar(xml, CaminhoItem + "/n:imposto");
            var grupo = xml.CreateElement("ICMS" + cst, NamespaceNF3e);
            Action<string, string> adicionar = (nome, valor) =>
            {
                var campo = xml.CreateElement(nome, NamespaceNF3e);
                campo.InnerText = valor;
                grupo.AppendChild(campo);
            };
            adicionar("CST", cst);
            if (cst == "20")
            {
                adicionar("pRedBC", "0.00");
                adicionar("vBC", "100.00");
                adicionar("pICMS", "18.00");
                adicionar("vICMS", "18.00");
            }
            else if (cst == "60")
            {
                adicionar("vBCSTRet", "100.00");
                adicionar("pICMSSTRet", "18.0000");
                adicionar("vICMSSTRet", "18.00");
            }
            adicionar("vICMSDeson", "1.00");
            adicionar("cBenef", codigo);
            imposto.ReplaceChild(grupo, Selecionar(xml, CaminhoItem + "/n:imposto/n:gICMSPrevistoPagtoAntecip"));
            var validador = new ValidarSchema();
            validador.Validar(xml, "NF3e.nf3e_v1.00.xsd", NamespaceNF3e);
            Assert.True(validador.Success == valido, validador.ErrorMessage);
            if (!valido)
            {
                Assert.Contains("cBenef", validador.ErrorMessage);
            }
            var lido = Item(XMLUtility.Deserializar<ModeloNF3e>(xml)).Imposto;
            var beneficio = cst == "20" ? lido.ICMS20.CBenef : cst == "40" ? lido.ICMS40.CBenef :
                cst == "51" ? lido.ICMS51.CBenef : cst == "60" ? lido.ICMS60.CBenef : lido.ICMS90.CBenef;
            Assert.Equal(codigo, beneficio);
        }

        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(false)]
        [InlineData(true)]
        public void SchemaRejeitaParLiquidoIncompleto(bool informarUnitario)
        {
            var modelo = LerModelo();
            Item(modelo).Prod.VItemLiq = informarUnitario ? (decimal?)0 : null;
            Item(modelo).Prod.VProdLiq = informarUnitario ? null : (decimal?)0;
            var validador = new ValidarSchema();
            validador.Validar(modelo.GerarXML(), "NF3e.nf3e_v1.00.xsd", NamespaceNF3e);
            Assert.False(validador.Success);
            Assert.Contains("vProdLiq", validador.ErrorMessage);
        }

        [Fact]
        [Trait("DFe", "NF3e")]
        public void SchemaRejeitaICMSPrevistoJuntoComCST()
        {
            var modelo = LerModelo();
            Item(modelo).Imposto.ICMS40 = new ICMS40 { CST = "40" };
            var validador = new ValidarSchema();
            validador.Validar(modelo.GerarXML(), "NF3e.nf3e_v1.00.xsd", NamespaceNF3e);
            Assert.False(validador.Success);
            Assert.Contains("gICMSPrevistoPagtoAntecip", validador.ErrorMessage);
        }

        [Fact]
        [Trait("DFe", "NF3e")]
        public void DocumentoProcessadoPreservaNovosCamposEValidaSchemaRaiz()
        {
            var nf3e = LerModelo();
            var processado = new NF3eProc
            {
                Versao = "1.00",
                DhConexao = new DateTimeOffset(2024, 11, 28, 15, 49, 0, TimeSpan.FromHours(-3)),
                NF3e = nf3e,
                ProtNF3e = new ProtNF3e
                {
                    Versao = "1.00",
                    InfProt = new InfProt
                    {
                        TpAmb = TipoAmbiente.Homologacao,
                        VerAplic = "TESTE",
                        ChNF3e = nf3e.InfNF3e.Chave,
                        DhRecbto = new DateTimeOffset(2024, 11, 28, 15, 49, 0, TimeSpan.FromHours(-3)),
                        CStat = 100,
                        XMotivo = "Autorizado o uso da NF3e"
                    }
                }
            };
            var xml = processado.GerarXML();
            Validar(xml, "NF3e.procNF3e_v1.00.xsd");
            var lido = XMLUtility.Deserializar<NF3eProc>(xml);
            Assert.Equal(80.1234567891m, Item(lido.NF3e).Prod.VItemLiq);
            Assert.Equal(18.25d, Item(lido.NF3e).Imposto.GICMSPrevistoPagtoAntecip.VICMSPrevisto);
            Assert.Equal(0.90d, Item(lido.NF3e).GProcRef.GIBSCBS.VCBS);
            Assert.Equal(xml.InnerText, lido.GerarXML().InnerText);
        }
    }
}
