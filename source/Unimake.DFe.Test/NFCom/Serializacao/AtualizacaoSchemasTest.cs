using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFCom;
using Xunit;
using ModeloNFCom = Unimake.Business.DFe.Xml.NFCom.NFCom;

namespace Unimake.DFe.Test.NFCom.Serializacao
{
    public class AtualizacaoSchemasTest
    {
        private const string NamespaceNFCom = "http://www.portalfiscal.inf.br/nfcom";
        private const string ArquivoModelo = @"..\..\..\NFCom\Resources\NFCom_AtualizacaoSchemas.xml";

        private static ModeloNFCom LerModelo() => XMLUtility.Deserializar<ModeloNFCom>(File.ReadAllText(ArquivoModelo));

        private static XmlNode Selecionar(XmlDocument xml, string caminho)
        {
            var namespaces = new XmlNamespaceManager(xml.NameTable);
            namespaces.AddNamespace("n", NamespaceNFCom);
            return xml.SelectSingleNode(caminho, namespaces);
        }

        private static void Validar(XmlDocument xml, string schema = "NFCom.nfcom_v1.00.xsd")
        {
            var validador = new ValidarSchema();
            validador.Validar(xml, schema, NamespaceNFCom);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        [Fact]
        [Trait("DFe", "NFCom")]
        public void NovoLeiautePreservaConteudoNoRoundTrip()
        {
            var original = new XmlDocument();
            original.Load(ArquivoModelo);
            Validar(original);

            var modelo = XMLUtility.Deserializar<ModeloNFCom>(original);
            Assert.Equal(4106902, modelo.InfNFCom.Assinante.CMunPrinc);
            Assert.Equal(80.12345678m, modelo.InfNFCom.Det[0].Prod.VItemLiq);
            Assert.Equal(80.12345678m, modelo.InfNFCom.Det[0].Prod.VProdLiq);
            Assert.Equal(80.12d, modelo.InfNFCom.Total.VProdLiq);
            Assert.Equal(18.25d, modelo.InfNFCom.Det[0].Imposto.GICMSPrevistoPagtoAntecip.VICMSPrevisto);
            Assert.Equal(100.12d, modelo.InfNFCom.Det[0].GProcRef.GIBSCBS.VBC);
            Assert.Equal(0.10d, modelo.InfNFCom.Det[0].GProcRef.GIBSCBS.VIBSUF);
            Assert.Equal(0d, modelo.InfNFCom.Det[0].GProcRef.GIBSCBS.VIBSMun);
            Assert.Equal(0.90d, modelo.InfNFCom.Det[0].GProcRef.GIBSCBS.VCBS);
            Assert.Equal(2, modelo.InfNFCom.Assinante.TerminaisAdicionais.Count);
            Assert.Equal(2, modelo.InfNFCom.Det[0].GProcRef.GProc.Count);

            var gerado = modelo.GerarXML();
            Assert.Equal(original.InnerText, gerado.InnerText);
            Assert.Equal(gerado.InnerText, XMLUtility.Deserializar<ModeloNFCom>(gerado).GerarXML().InnerText);
            Validar(gerado);
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(0)]
        [InlineData(4106902)]
        public void MunicipioPrincipalPreservaAusenciaOrdemETerminaisAdicionais(int municipio)
        {
            var modelo = LerModelo();
            modelo.InfNFCom.Assinante.CMunPrinc = municipio;
            var xml = modelo.GerarXML();
            var campo = Selecionar(xml, "/n:NFCom/n:infNFCom/n:assinante/n:cMunPrinc");
            Assert.Equal(municipio > 0, campo != null);
            if (campo != null)
            {
                Assert.Equal("cUFPrinc", campo.PreviousSibling.LocalName);
                Assert.Equal("NroTermAdic", campo.NextSibling.LocalName);
            }
            var lido = XMLUtility.Deserializar<ModeloNFCom>(xml);
            Assert.Equal(municipio, lido.InfNFCom.Assinante.CMunPrinc);
            Assert.Equal(2, lido.InfNFCom.Assinante.TerminaisAdicionais.Count);
            Validar(xml);
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(null)]
        [InlineData("0.00")]
        [InlineData("1234567890123.12345678")]
        public void ValoresLiquidosDoItemPreservamAusenciaZeroPrecisaoEOrdem(string valor)
        {
            var modelo = LerModelo();
            decimal? numero = valor == null ? (decimal?)null : decimal.Parse(valor, CultureInfo.InvariantCulture);
            modelo.InfNFCom.Det[0].Prod.VItemLiq = numero;
            modelo.InfNFCom.Det[0].Prod.VProdLiq = numero;
            var xml = modelo.GerarXML();
            var item = Selecionar(xml, "/n:NFCom/n:infNFCom/n:det/n:prod/n:vItemLiq");
            var produto = Selecionar(xml, "/n:NFCom/n:infNFCom/n:det/n:prod/n:vProdLiq");
            Assert.Equal(numero.HasValue, item != null);
            Assert.Equal(numero.HasValue, produto != null);
            if (numero.HasValue)
            {
                Assert.Equal(valor, item.InnerText);
                Assert.Equal(valor, produto.InnerText);
                Assert.Equal("vProd", item.PreviousSibling.LocalName);
                Assert.Same(produto, item.NextSibling);
                Assert.Equal("dExpiracao", produto.NextSibling.LocalName);
            }
            var lido = XMLUtility.Deserializar<ModeloNFCom>(xml);
            Assert.Equal(numero, lido.InfNFCom.Det[0].Prod.VItemLiq);
            Assert.Equal(numero, lido.InfNFCom.Det[0].Prod.VProdLiq);
            Validar(xml);
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(null)]
        [InlineData(0d)]
        [InlineData(100.25d)]
        public void TotalLiquidoPreservaAusenciaZeroEOrdem(double? valor)
        {
            var modelo = LerModelo();
            modelo.InfNFCom.Total.VProdLiq = valor;
            var xml = modelo.GerarXML();
            var campo = Selecionar(xml, "/n:NFCom/n:infNFCom/n:total/n:vProdLiq");
            Assert.Equal(valor.HasValue, campo != null);
            if (campo != null)
            {
                Assert.Equal(valor.Value.ToString("F2", CultureInfo.InvariantCulture), campo.InnerText);
                Assert.Equal("vProd", campo.PreviousSibling.LocalName);
                Assert.Equal("ICMSTot", campo.NextSibling.LocalName);
            }
            Assert.Equal(valor, XMLUtility.Deserializar<ModeloNFCom>(xml).InfNFCom.Total.VProdLiq);
            Validar(xml);
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(0d)]
        [InlineData(125.25d)]
        public void ICMSPrevistoSerializaAlternativaComZero(double valor)
        {
            var modelo = LerModelo();
            modelo.InfNFCom.Det[0].Imposto.GICMSPrevistoPagtoAntecip.VICMSPrevisto = valor;
            var xml = modelo.GerarXML();
            var grupo = Selecionar(xml, "/n:NFCom/n:infNFCom/n:det/n:imposto/n:gICMSPrevistoPagtoAntecip");
            Assert.NotNull(grupo);
            Assert.Equal(valor.ToString("F2", CultureInfo.InvariantCulture), grupo.FirstChild.InnerText);
            Assert.Null(Selecionar(xml, "/n:NFCom/n:infNFCom/n:det/n:imposto/n:ICMS00"));
            Assert.Equal(valor, XMLUtility.Deserializar<ModeloNFCom>(xml).InfNFCom.Det[0].Imposto.GICMSPrevistoPagtoAntecip.VICMSPrevisto);
            Validar(xml);
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(false)]
        [InlineData(true)]
        public void IBSCBSDoProcessoPreservaAusenciaZeroEOrdem(bool informar)
        {
            var modelo = LerModelo();
            modelo.InfNFCom.Det[0].GProcRef.GIBSCBS = informar ? new GIBSCBSProcRef() : null;
            var xml = modelo.GerarXML();
            var grupo = Selecionar(xml, "/n:NFCom/n:infNFCom/n:det/n:gProcRef/n:gIBSCBS");
            Assert.Equal(informar, grupo != null);
            if (grupo != null)
            {
                Assert.Equal("vProd", grupo.PreviousSibling.LocalName);
                Assert.Equal("gProc", grupo.NextSibling.LocalName);
                Assert.Equal(4, grupo.ChildNodes.Count);
                Assert.Equal("vBC", grupo.ChildNodes[0].LocalName);
                Assert.Equal("vIBSUF", grupo.ChildNodes[1].LocalName);
                Assert.Equal("vIBSMun", grupo.ChildNodes[2].LocalName);
                Assert.Equal("vCBS", grupo.ChildNodes[3].LocalName);
                foreach (XmlNode filho in grupo.ChildNodes)
                {
                    Assert.Equal("0.00", filho.InnerText);
                    Assert.Equal(NamespaceNFCom, filho.NamespaceURI);
                }
            }
            Assert.Equal(informar, XMLUtility.Deserializar<ModeloNFCom>(xml).InfNFCom.Det[0].GProcRef.GIBSCBS != null);
            Assert.Null(Selecionar(xml, "/n:NFCom/n:infNFCom/n:det/n:imposto/n:IBSCBS/n:gIBSCBS"));
            Validar(xml);
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData("ICMS20")]
        [InlineData("ICMS40")]
        [InlineData("ICMS51")]
        [InlineData("ICMS90")]
        [InlineData("ICMSUFDest")]
        public void BeneficiosFiscaisRespeitamComprimentosOitoEDez(string grupo)
        {
            foreach (var codigo in new[] { "PR123456", "PR12345678", "PR1234567", "PR12345", "PR123456789", "PR12 456" })
            {
                var modelo = LerModelo();
                var imposto = new Imposto();
                switch (grupo)
                {
                    case "ICMS20":
                        imposto.ICMS20 = new ICMS20 { CST = "20", VICMSDeson = 1, CBenef = codigo };
                        break;
                    case "ICMS40":
                        imposto.ICMS40 = new ICMS40 { CST = "40", VICMSDeson = 1, CBenef = codigo };
                        break;
                    case "ICMS51":
                        imposto.ICMS51 = new ICMS51 { CST = "51", VICMSDeson = 1, CBenef = codigo };
                        break;
                    case "ICMS90":
                        imposto.ICMS90 = new ICMS90 { CST = "90", VICMSDeson = 1, CBenef = codigo };
                        break;
                    case "ICMSUFDest":
                        imposto.ICMS00 = new ICMS00 { CST = "00" };
                        imposto.ICMSUFDest = new List<ICMSUFDest> { new ICMSUFDest { CUFDest = UFBrasil.PR, CBenefUFDest = codigo } };
                        break;
                }
                modelo.InfNFCom.Det[0].Imposto = imposto;
                var xml = modelo.GerarXML();
                var validador = new ValidarSchema();
                validador.Validar(xml, "NFCom.nfcom_v1.00.xsd", NamespaceNFCom);
                var valido = codigo == "PR123456" || codigo == "PR12345678";
                Assert.True(validador.Success == valido, validador.ErrorMessage);
                if (!valido)
                {
                    Assert.Contains(grupo == "ICMSUFDest" ? "cBenefUFDest" : "cBenef", validador.ErrorMessage);
                }
                var lido = XMLUtility.Deserializar<ModeloNFCom>(xml).GerarXML();
                Assert.Equal(codigo, Selecionar(lido, "/n:NFCom/n:infNFCom/n:det/n:imposto/n:" + grupo + "/n:" + (grupo == "ICMSUFDest" ? "cBenefUFDest" : "cBenef")).InnerText);
            }
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(true)]
        [InlineData(false)]
        public void SchemaRejeitaSequenciaLiquidaIncompleta(bool informarUnitario)
        {
            var modelo = LerModelo();
            modelo.InfNFCom.Det[0].Prod.VItemLiq = informarUnitario ? (decimal?)0 : null;
            modelo.InfNFCom.Det[0].Prod.VProdLiq = informarUnitario ? null : (decimal?)0;
            var validador = new ValidarSchema();
            validador.Validar(modelo.GerarXML(), "NFCom.nfcom_v1.00.xsd", NamespaceNFCom);
            Assert.False(validador.Success);
            Assert.Contains(informarUnitario ? "vProdLiq" : "vItemLiq", validador.ErrorMessage);
        }

        [Fact]
        [Trait("DFe", "NFCom")]
        public void SchemaRejeitaICMSPrevistoComOutraAlternativaDeICMS()
        {
            var modelo = LerModelo();
            modelo.InfNFCom.Det[0].Imposto.ICMS00 = new ICMS00 { CST = "00" };
            var validador = new ValidarSchema();
            validador.Validar(modelo.GerarXML(), "NFCom.nfcom_v1.00.xsd", NamespaceNFCom);
            Assert.False(validador.Success);
            Assert.Contains("gICMSPrevistoPagtoAntecip", validador.ErrorMessage);
        }

        [Fact]
        [Trait("DFe", "NFCom")]
        public void NFComProcessadaTambemPreservaNovosGrupos()
        {
            var nfcom = LerModelo();
            var processada = new NFComProc
            {
                Versao = "1.00",
                NFCom = nfcom,
                ProtNFCom = new ProtNFCom
                {
                    Versao = "1.00",
                    InfProt = new InfProt
                    {
                        TpAmb = TipoAmbiente.Homologacao,
                        VerAplic = "TESTE",
                        ChNFCom = nfcom.InfNFCom.Chave,
                        DhRecbto = new DateTimeOffset(2026, 10, 10, 10, 0, 1, TimeSpan.FromHours(-3)),
                        NProt = "1412600000000001",
                        DigVal = "AA==",
                        CStat = 100,
                        XMotivo = "Autorizado o uso da NFCom"
                    }
                }
            };
            var xml = processada.GerarXML();
            var lido = XMLUtility.Deserializar<NFComProc>(xml);
            Assert.Equal(80.12345678m, lido.NFCom.InfNFCom.Det[0].Prod.VItemLiq);
            Assert.NotNull(lido.NFCom.InfNFCom.Det[0].Imposto.GICMSPrevistoPagtoAntecip);
            Assert.Equal(100.12d, lido.NFCom.InfNFCom.Det[0].GProcRef.GIBSCBS.VBC);
            Assert.Equal(xml.InnerText, lido.GerarXML().InnerText);
            Validar(xml, "NFCom.procNFCom_v1.00.xsd");
        }
    }
}
