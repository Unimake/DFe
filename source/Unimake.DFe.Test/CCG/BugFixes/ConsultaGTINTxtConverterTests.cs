using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CCG;
using Unimake.Business.DFe.Xml.NFe.Txt;
using Xunit;

namespace Unimake.DFe.Test.CCG.BugFixes
{
    [Trait("DFe", "CCG")]
    public class ConsultaGTINTxtConverterTests : IDisposable
    {
        private readonly string pasta = Path.Combine(Path.GetTempPath(), "Unimake-GTIN-Txt-" + Guid.NewGuid().ToString("N"));

        public ConsultaGTINTxtConverterTests() => Directory.CreateDirectory(pasta);

        public static IEnumerable<object[]> Pedidos()
        {
            foreach (var gtin in new[] { "12345670", "123456789012", "7891234567895", "17891234567892" })
                foreach (var bom in new[] { false, true })
                    yield return new object[] { gtin, bom };
        }

        [Theory]
        [MemberData(nameof(Pedidos))]
        public void Converter_DeveGerarNamespaceDoModeloEValidarSemTransporte(string gtin, bool bom)
        {
            var resultado = Converter("GTIN|" + gtin + "\r\nversao|1.00", bom);
            var xml = LerSeguro(resultado.Xml);
            var esperado = new ConsGTIN { Versao = "1.00", GTIN = gtin }.GerarXML();

            Assert.True(resultado.Sucesso);
            Assert.Equal("consGTIN", xml.DocumentElement.LocalName);
            Assert.Equal(esperado.DocumentElement.NamespaceURI, xml.DocumentElement.NamespaceURI);
            Assert.Equal("http://www.portalfiscal.inf.br/nfe", xml.DocumentElement.NamespaceURI);
            Assert.Equal("1.00", xml.DocumentElement.GetAttribute("versao"));
            Assert.Equal(xml.DocumentElement.NamespaceURI, xml.DocumentElement.FirstChild.NamespaceURI);
            var modelo = XMLUtility.Deserializar<ConsGTIN>(xml);
            Assert.Equal(gtin, modelo.GTIN);
            Assert.Equal("1.00", modelo.Versao);
            Assert.DoesNotContain("http://www.portalfiscal.inf.br/ccg", resultado.Xml);

            var validacao = new ValidarEstruturaXML().ValidarServico(xml, new Configuracao
            {
                TipoDFe = TipoDFe.CCG,
                CodigoUF = (int)UFBrasil.RS,
                TipoAmbiente = TipoAmbiente.Homologacao
            });
            Assert.True(validacao.Validado, validacao.MensagemRetorno);
            Assert.Equal("1", validacao.StatusValidacao);
            Assert.Equal(0, validacao.XmlAssinado.GetElementsByTagName("Signature").Count);
        }

        [Fact]
        public void VersaoAusente_DevePreservarDefault()
        {
            var xml = LerSeguro(Converter("GTIN|7891234567895", false).Xml);
            Assert.Equal("1.00", xml.DocumentElement.GetAttribute("versao"));
            Assert.Equal("7891234567895", XMLUtility.Deserializar<ConsGTIN>(xml).GTIN);
        }

        [Fact]
        public void CamposRepetidosEOutraCaixa_DevemPreservarUltimoValor()
        {
            var xml = LerSeguro(Converter("gTiN|12345670\n\nGTIN|7891234567895\nVeRsAo|1.00", true).Xml);
            Assert.Equal("7891234567895", XMLUtility.Deserializar<ConsGTIN>(xml).GTIN);
            Assert.Equal(1, xml.GetElementsByTagName("GTIN").Count);
        }

        [Theory]
        [InlineData("versao|1.00")]
        [InlineData("GTIN|")]
        [InlineData("GTIN|   ")]
        public void GtinObrigatorio_DevePreservarErroDeConversao(string txt)
        {
            var erro = Assert.Throws<FormatException>(() => Converter(txt, false));
            Assert.Equal("Informe o campo 'GTIN' no arquivo TXT.", erro.Message);
        }

        [Fact]
        public void Retorno_DevePreservarOrdemCamposVaziosECestRepetido()
        {
            const string xml = "<retConsGTIN versao=\"1.00\" xmlns=\"http://www.portalfiscal.inf.br/nfe\"><cStat>9490</cStat><xMotivo>Consulta sintética</xMotivo><GTIN>7891234567895</GTIN><tpGTIN>13</tpGTIN><xProd>PRODUTO SINTETICO</xProd><NCM></NCM><CEST>0000000</CEST><CEST>1111111</CEST></retConsGTIN>";
            const string esperado = "CStat|9490\r\nXMotivo|Consulta sintética\r\nGTIN|7891234567895\r\ntpGTIN|13\r\nxProd|PRODUTO SINTETICO\r\nNCM|\r\nCEST|0000000\r\nCEST|1111111\r\n";
            Assert.Equal(esperado, new ConsultaGTINTxtConverter().ConverterRetorno(xml));
        }

        private TxtConversaoResultado Converter(string txt, bool bom)
        {
            var arquivo = Path.Combine(pasta, Guid.NewGuid().ToString("N") + "-consgtin.txt");
            File.WriteAllText(arquivo, txt, bom ? new UTF8Encoding(true) : Encoding.Default);
            return new ConsultaGTINTxtConverter().Converter(arquivo, new TxtConversaoContexto
            {
                TipoAmbiente = TipoAmbiente.Homologacao,
                CodigoUF = UFBrasil.RS,
                TipoEmissao = TipoEmissao.Normal
            });
        }

        private static XmlDocument LerSeguro(string texto)
        {
            var xml = new XmlDocument { XmlResolver = null };
            using (var reader = XmlReader.Create(new StringReader(texto), new XmlReaderSettings { DtdProcessing = DtdProcessing.Prohibit, XmlResolver = null }))
                xml.Load(reader);
            return xml;
        }

        public void Dispose()
        {
            var alvo = Path.GetFullPath(pasta);
            var temporaria = Path.GetFullPath(Path.GetTempPath()).TrimEnd(Path.DirectorySeparatorChar) + Path.DirectorySeparatorChar;
            if (!alvo.StartsWith(temporaria, StringComparison.OrdinalIgnoreCase)
                || !Path.GetFileName(alvo).StartsWith("Unimake-GTIN-Txt-", StringComparison.Ordinal))
                throw new InvalidOperationException("Limpeza fora da fixture temporaria.");
            Directory.Delete(alvo, true);
        }
    }
}
