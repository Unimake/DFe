using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.CTe.Serializacao
{
    public class AtualizacaoSchemasTest
    {
        private const string NamespaceCTe = "http://www.portalfiscal.inf.br/cte";

        private static Unimake.Business.DFe.Xml.CTe.CTe LerModelo() => XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTe.CTe>(File.ReadAllText(@"..\..\..\CTe\Resources\CTe_AtualizacaoSchemas.xml"));

        private static void Validar(XmlDocument xml)
        {
            var validador = new ValidarSchema();
            validador.Validar(xml, "CTe.cte_v4.00.xsd", NamespaceCTe);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(null)]
        [InlineData(0d)]
        [InlineData(100.25d)]
        public void ValorLiquidoPreservaAusenciaZeroEOrdem(double? valor)
        {
            var modelo = LerModelo();
            modelo.InfCTe.VPrest.VTPrestLiq = valor;
            var xml = modelo.GerarXML();
            var campos = xml.GetElementsByTagName("vTPrestLiq", NamespaceCTe);
            Assert.Equal(valor.HasValue ? 1 : 0, campos.Count);
            if (valor.HasValue)
            {
                Assert.Equal("vTPrest", campos[0].PreviousSibling.LocalName);
                Assert.Equal("vRec", campos[0].NextSibling.LocalName);
                Assert.Equal(valor.Value.ToString("F2", System.Globalization.CultureInfo.InvariantCulture), campos[0].InnerText);
            }

            Assert.Equal(valor, XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTe.CTe>(xml).InfCTe.VPrest.VTPrestLiq);
            Validar(xml);
        }

        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(0d)]
        [InlineData(125.25d)]
        public void ICMSPrevistoSerializaAlternativaSemICMSNormal(double valor)
        {
            var modelo = LerModelo();
            modelo.InfCTe.Imp.ICMS.GICMSPrevistoPagtoAntecip.VICMSPrevisto = valor;
            var xml = modelo.GerarXML();
            Assert.Equal(0, xml.GetElementsByTagName("ICMS00").Count);
            var grupo = xml.GetElementsByTagName("gICMSPrevistoPagtoAntecip", NamespaceCTe)[0];
            Assert.Equal("ICMS", grupo.ParentNode.LocalName);
            Assert.Equal(valor.ToString("F2", System.Globalization.CultureInfo.InvariantCulture), grupo.FirstChild.InnerText);
            Assert.Equal(valor, XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTe.CTe>(xml).InfCTe.Imp.ICMS.GICMSPrevistoPagtoAntecip.VICMSPrevisto);
            Validar(xml);
        }

        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(TipoEmissao.ContingenciaOfflineCTe)]
        [InlineData(TipoEmissao.RegimeEspecialNFF)]
        public void TipoEmissaoNovoDominioSerializaEDesserializa(TipoEmissao tipo)
        {
            var modelo = LerModelo();
            modelo.InfCTe.Ide.TpEmis = tipo;
            var xml = modelo.GerarXML();
            Assert.Equal(((int)tipo).ToString(), xml.GetElementsByTagName("tpEmis")[0].InnerText);
            Assert.Equal(tipo, XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTe.CTe>(xml).InfCTe.Ide.TpEmis);
            Validar(xml);
        }

        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(TipoEmissao.ContingenciaEPEC)]
        [InlineData(TipoEmissao.ContingenciaFSDA)]
        public void EmissaoLegadaPermaneceLegivelMasSchemaAtualRejeita(TipoEmissao tipo)
        {
            var modelo = LerModelo();
            modelo.InfCTe.Ide.TpEmis = tipo;
            var xml = modelo.GerarXML();
            Assert.Equal(tipo, XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTe.CTe>(xml).InfCTe.Ide.TpEmis);
            var validador = new ValidarSchema();
            validador.Validar(xml, "CTe.cte_v4.00.xsd", NamespaceCTe);
            Assert.False(validador.Success);
            Assert.Contains("tpEmis", validador.ErrorMessage);
        }

        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData("PR123456", true)]
        [InlineData("PR12345678", true)]
        [InlineData("PR12345", false)]
        [InlineData("PR1234567", false)]
        public void BeneficioFiscalRespeitaNovoComprimento(string codigo, bool valido)
        {
            var modelo = LerModelo();
            modelo.InfCTe.Imp.ICMS = new Unimake.Business.DFe.Xml.CTe.ICMS
            {
                ICMS20 = new Unimake.Business.DFe.Xml.CTe.ICMS20 { PRedBC = 10, VBC = 100, PICMS = 12, VICMS = 12, VICMSDeson = 10, CBenef = codigo }
            };
            var xml = modelo.GerarXML();
            var validador = new ValidarSchema();
            validador.Validar(xml, "CTe.cte_v4.00.xsd", NamespaceCTe);
            Assert.True(validador.Success == valido, validador.ErrorMessage);
            if (!valido)
            {
                Assert.Contains("cBenef", validador.ErrorMessage);
            }
            Assert.Equal(codigo, XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTe.CTe>(xml).InfCTe.Imp.ICMS.ICMS20.CBenef);
        }
    }
}
