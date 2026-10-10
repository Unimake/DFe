using System.IO;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;
using ModeloCTeSimp = Unimake.Business.DFe.Xml.CTeSimp.CTeSimp;

namespace Unimake.DFe.Test.CTeSimp.Serializacao
{
    public class AtualizacaoSchemasTest
    {
        private const string NamespaceCTe = "http://www.portalfiscal.inf.br/cte";

        private static ModeloCTeSimp LerModelo() => XMLUtility.Deserializar<ModeloCTeSimp>(File.ReadAllText(@"..\..\..\CTeSimp\Resources\CTeSimp_AtualizacaoSchemas.xml"));

        private static void Validar(XmlDocument xml)
        {
            var validador = new ValidarSchema();
            validador.Validar(xml, "CTe.cteSimp_v4.00.xsd", NamespaceCTe);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        [Fact]
        [Trait("DFe", "CTeSimp")]
        public void TributacaoPorPrestacaoETotaisPreservamCaminhoOrdemEValores()
        {
            var modelo = LerModelo();
            Assert.Equal(3, modelo.InfCTe.Det.Count);
            foreach (var det in modelo.InfCTe.Det)
            {
                Assert.NotNull(det.IBSCBS);
                Assert.Equal(100.25d, det.VPrestLiq);
            }

            var total = modelo.InfCTe.Total;
            Assert.Equal(300.75d, total.VTPrestLiq);
            Assert.Equal(300.75d, total.IBSCBSTot.VBCIBSCBS);
            Assert.Equal(3.01d, total.IBSCBSTot.GIBSTot.GIBSUFTot.VIBSUF);
            Assert.Equal(1.5d, total.IBSCBSTot.GIBSTot.GIBSMunTot.VIBSMun);
            Assert.Equal(4.51d, total.IBSCBSTot.GIBSTot.VIBS);
            Assert.Equal(9.02d, total.IBSCBSTot.GCBSTot.VCBS);
            Assert.Equal(0d, total.IBSCBSTot.GEstornoCred.VIBSEstCred);
            var xml = modelo.GerarXML();
            var ns = new XmlNamespaceManager(xml.NameTable);
            ns.AddNamespace("c", NamespaceCTe);
            Assert.Equal(3, xml.SelectNodes("/c:CTeSimp/c:infCte/c:det/c:IBSCBS", ns).Count);
            Assert.Empty(xml.SelectNodes("/c:CTeSimp/c:infCte/c:imp/c:IBSCBS", ns));
            var grupo = xml.GetElementsByTagName("IBSCBS", NamespaceCTe)[0];
            Assert.Equal("Comp", grupo.PreviousSibling.LocalName);
            Assert.Equal("infNFe", grupo.NextSibling.LocalName);
            Assert.Equal("vTRec", xml.GetElementsByTagName("IBSCBSTot")[0].PreviousSibling.LocalName);
            Assert.Equal(xml.InnerText, XMLUtility.Deserializar<ModeloCTeSimp>(xml).GerarXML().InnerText);
            Validar(xml);
        }

        [Theory]
        [Trait("DFe", "CTeSimp")]
        [InlineData(null)]
        [InlineData(0d)]
        [InlineData(10.25d)]
        public void ValoresLiquidosETotaisOpcionais(double? valor)
        {
            var modelo = LerModelo();
            foreach (var det in modelo.InfCTe.Det)
            {
                det.VPrestLiq = valor;
            }
            modelo.InfCTe.Total.VTPrestLiq = valor;
            modelo.InfCTe.Total.IBSCBSTot = null;
            var xml = modelo.GerarXML();
            Assert.Equal(valor.HasValue ? 3 : 0, xml.GetElementsByTagName("vPrestLiq").Count);
            Assert.Equal(valor.HasValue ? 1 : 0, xml.GetElementsByTagName("vTPrestLiq").Count);
            Assert.Equal(0, xml.GetElementsByTagName("IBSCBSTot").Count);
            var lido = XMLUtility.Deserializar<ModeloCTeSimp>(xml);
            Assert.Equal(valor, lido.InfCTe.Det[0].VPrestLiq);
            Assert.Equal(valor, lido.InfCTe.Total.VTPrestLiq);
            Validar(xml);
        }

        [Theory]
        [Trait("DFe", "CTeSimp")]
        [InlineData(TipoEmissao.ContingenciaOfflineCTe)]
        [InlineData(TipoEmissao.RegimeEspecialNFF)]
        public void TipoEmissaoNovoDominio(TipoEmissao tipo)
        {
            var modelo = LerModelo();
            modelo.InfCTe.Ide.TpEmis = tipo;
            var xml = modelo.GerarXML();
            Assert.Equal(tipo, XMLUtility.Deserializar<ModeloCTeSimp>(xml).InfCTe.Ide.TpEmis);
            Validar(xml);
        }

        [Fact]
        [Trait("DFe", "CTeSimp")]
        public void EPECLegadoPermaneceLegivelMasSchemaAtualRejeita()
        {
            var modelo = LerModelo();
            modelo.InfCTe.Ide.TpEmis = TipoEmissao.ContingenciaEPEC;
            var xml = modelo.GerarXML();
            Assert.Equal(TipoEmissao.ContingenciaEPEC, XMLUtility.Deserializar<ModeloCTeSimp>(xml).InfCTe.Ide.TpEmis);
            var validador = new ValidarSchema();
            validador.Validar(xml, "CTe.cteSimp_v4.00.xsd", NamespaceCTe);
            Assert.False(validador.Success);
            Assert.Contains("tpEmis", validador.ErrorMessage);
        }
    }
}
