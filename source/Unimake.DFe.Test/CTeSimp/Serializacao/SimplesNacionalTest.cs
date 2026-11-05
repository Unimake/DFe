using System.IO;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CTe;
using Xunit;
using Modelo = Unimake.Business.DFe.Xml.CTeSimp.CTeSimp;

namespace Unimake.DFe.Test.CTeSimp.Serializacao
{
    public class SimplesNacionalTest
    {
        private const string NS = "http://www.portalfiscal.inf.br/cte";
        private static Modelo LerModelo() => XMLUtility.Deserializar<Modelo>(File.ReadAllText(@"..\..\..\CTeSimp\Resources\CTeSimp_AtualizacaoSchemas.xml"));

        [Theory]
        [Trait("DFe", "CTeSimp")]
        [InlineData(false, false)]
        [InlineData(true, false)]
        [InlineData(false, true)]
        [InlineData(true, true)]
        public void TributacaoEMultaJurosPreservamGruposOrdemEValores(bool ibs, bool cbs)
        {
            var modelo = LerModelo();
            modelo.InfCTe.Ide.GPagAntecipado = new GPagAntecipado
            {
                ChDFePagAnt = new System.Collections.Generic.List<string> { "41261000000000000000570010000000011000000000" }
            };
            var imposto = modelo.InfCTe.Det[0].IBSCBS;
            imposto.GTribSN = new GTribSN
            {
                GIBSSN = ibs ? new GIBSSN { PIBSSN = 1.2345, VIBSSN = 0 } : null,
                GCBSSN = cbs ? new GCBSSN { PCBSSN = 2.3456, VCBSSN = 23.46 } : null
            };
            modelo.InfCTe.Ide.GMultaJuros = new GMultaJuros { ChCTeRef = "41261000000000000000570010000000011000000000" };
            modelo.InfCTe.Total.IBSCBSTot.GTribSN = new GTribSNTot { VIBSSN = 0, VCBSSN = 23.46 };
            var xml = modelo.GerarXML();
            var ns = new XmlNamespaceManager(xml.NameTable);
            ns.AddNamespace("c", NS);
            var multa = xml.SelectSingleNode("/*/c:infCte/c:ide/c:gMultaJuros", ns);
            Assert.Equal("gPagAntecipado", multa.PreviousSibling.LocalName);
            Assert.Null(multa.NextSibling);
            Assert.Equal(modelo.InfCTe.Ide.GMultaJuros.ChCTeRef, multa.FirstChild.InnerText);
            var grupo = xml.SelectSingleNode("/*/c:infCte/c:det[1]/c:IBSCBS/c:gTribSN", ns);
            Assert.Equal("gIBSCBS", grupo.PreviousSibling.LocalName);
            Assert.Equal("gEstornoCred", grupo.NextSibling.LocalName);
            Assert.Equal(ibs ? 1 : 0, grupo.SelectNodes("c:gIBSSN", ns).Count);
            Assert.Equal(cbs ? 1 : 0, grupo.SelectNodes("c:gCBSSN", ns).Count);
            if (ibs)
            {
                Assert.Equal("1.2345", grupo.SelectSingleNode("c:gIBSSN/c:pIBSSN", ns).InnerText);
                Assert.Equal("0.00", grupo.SelectSingleNode("c:gIBSSN/c:vIBSSN", ns).InnerText);
            }
            var totais = xml.SelectSingleNode("/*/c:infCte/c:total/c:IBSCBSTot/c:gTribSN", ns);
            Assert.Equal("gCBS", totais.PreviousSibling.LocalName);
            Assert.Equal("gEstornoCred", totais.NextSibling.LocalName);
            Assert.Equal(new[] { "vIBSSN", "vCBSSN" }, totais.ChildNodes.Cast<XmlNode>().Select(n => n.LocalName));
            var lido = XMLUtility.Deserializar<Modelo>(xml);
            Assert.Equal(xml.InnerText, lido.GerarXML().InnerText);
            Assert.Equal(modelo.InfCTe.Ide.GMultaJuros.ChCTeRef, lido.InfCTe.Ide.GMultaJuros.ChCTeRef);
            var validador = new ValidarSchema();
            validador.Validar(xml, "CTe.cteSimp_v4.00.xsd", NS);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        [Fact]
        [Trait("DFe", "CTeSimp")]
        public void NovosGruposSaoOmitidosQuandoNaoInformados()
        {
            var xml = LerModelo().GerarXML();
            Assert.Equal(0, xml.GetElementsByTagName("gMultaJuros", NS).Count);
            Assert.Equal(0, xml.GetElementsByTagName("gTribSN", NS).Count);
        }
    }
}
