using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using BPeTMXml = Unimake.Business.DFe.Xml.BPeTM;
using Xunit;

namespace Unimake.DFe.Test.BPe.Serializacao
{
    public class BPeTMSerializacaoTest
    {
        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\bpeTMProc.xml")]
        public void SerializacaoDesserializacaoBPeTMProc(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new BPeTMXml.BPeTMProc();
            var xml = bpe.LerXML<BPeTMXml.BPeTMProc>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\bpeTM_minimo.xml")]
        public void SerializacaoDesserializacaoBPeTM(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new BPeTMXml.BPeTM();
            var xml = bpe.LerXML<BPeTMXml.BPeTM>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }

        [Fact]
        [Trait("DFe", "BPe")]
        public void GerarChaveBPeTM()
        {
            var infBPe = new BPeTMXml.InfBPe
            {
                Versao = "1.00",
                Ide = new BPeTMXml.Ide
                {
                    CUF = UFBrasil.SP,
                    TpAmb = TipoAmbiente.Homologacao,
                    Mod = ModeloDFe.BPe,
                    Serie = 1,
                    NBP = 1,
                    CBP = "00000010",
                    Modal = ModalidadeTransporteBPe.Rodoviario,
                    DhEmi = DateTime.Now,
                    DCompet = DateTime.Parse("2026-07-01"),
                    TpEmis = TipoEmissaoBPe.Normal,
                    VerProc = "1.0",
                    TpBPe = TipoBPe.TransporteMetropolitano
                },
                Emit = new BPeTMXml.Emit
                {
                    CNPJ = "12345678000123"
                }
            };

            const string chaveEsperada = "35260712345678000123630010000000011000000108";

            Assert.Equal(chaveEsperada, infBPe.Chave);
            Assert.Equal("BPe" + chaveEsperada, infBPe.Id);
            Assert.Equal(8, infBPe.Ide.CDV);
        }
    }
}
