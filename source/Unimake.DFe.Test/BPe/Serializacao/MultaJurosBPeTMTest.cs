using System.IO;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.BPe;
using Unimake.Business.DFe.Xml.BPeTM;
using Xunit;

namespace Unimake.DFe.Test.BPe.Serializacao
{
    public class MultaJurosBPeTMTest
    {
        private const string NS = "http://www.portalfiscal.inf.br/bpe";
        private const string ChaveReferencia = "35260712345678000195630010000000011234567890";

        private static BPeTM LerModelo()
        {
            return XMLUtility.Deserializar<BPeTM>(File.ReadAllText(@"..\..\..\BPe\Resources\BPeTM_AtualizacaoSchemas.xml"));
        }

        private static void Validar(XmlDocument xml, string schema)
        {
            var validador = new ValidarSchema();
            validador.Validar(xml, "BPe." + schema + "_v1.00.xsd", NS);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        [Fact]
        [Trait("DFe", "BPe")]
        public void LeituraDeXMLAnteriorNaoReintroduzGrupoRemovido()
        {
            var xml = LerModelo().GerarXML();
            var ide = xml.DocumentElement["infBPe"]["ide"];
            var multa = xml.CreateElement("gMultaJuros", NS);
            var referencia = xml.CreateElement("chBPeRef", NS);
            referencia.InnerText = ChaveReferencia;
            multa.AppendChild(referencia);
            ide.AppendChild(multa);

            var validador = new ValidarSchema();
            validador.Validar(xml, "BPe.bpeTM_v1.00.xsd", NS);
            Assert.False(validador.Success);
            Assert.Contains("gMultaJuros", validador.ErrorMessage);

            var lido = XMLUtility.Deserializar<BPeTM>(xml);
            var gerado = lido.GerarXML();
            Assert.Equal(0, gerado.GetElementsByTagName("gMultaJuros", NS).Count);
            Validar(gerado, "bpeTM");
        }

        [Fact]
        [Trait("DFe", "BPe")]
        public void DocumentoProcessadoTambemOmiteGrupoRemovido()
        {
            var modelo = LerModelo();
            var retorno = XMLUtility.Deserializar<RetBPe>(File.ReadAllText(@"..\..\..\BPe\Resources\retBPe.xml"));
            var processado = new BPeTMProc
            {
                Versao = "1.00",
                BPeTM = modelo,
                ProtBPe = retorno.ProtBPe
            };

            var xml = processado.GerarXML();
            Assert.Equal(0, xml.GetElementsByTagName("gMultaJuros", NS).Count);
            var lido = XMLUtility.Deserializar<BPeTMProc>(xml);
            Assert.Equal(xml.InnerText, lido.GerarXML().InnerText);
            Validar(xml, "procBPeTM");
        }
    }
}
