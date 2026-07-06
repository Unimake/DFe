using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.BPe;
using BPeTMXml = Unimake.Business.DFe.Xml.BPeTM;
using Xunit;

namespace Unimake.DFe.Test.BPe
{
    public class SerializacaoDesserializacaoTest
    {
        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\bpe_minimo.xml")]
        public void SerializacaoDesserializacaoBPe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new Unimake.Business.DFe.Xml.BPe.BPe();
            var xml = bpe.LerXML<Unimake.Business.DFe.Xml.BPe.BPe>(doc);

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

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\consStatServBPe-ped-sta.xml")]
        public void SerializacaoDesserializacaoConsStatServBPe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new Unimake.Business.DFe.Xml.BPe.ConsStatServBPe();
            var xml = bpe.LerXML<Unimake.Business.DFe.Xml.BPe.ConsStatServBPe>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\consSitBPe-ped-sit.xml")]
        public void SerializacaoDesserializacaoConsSitBPe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new Unimake.Business.DFe.Xml.BPe.ConsSitBPe();
            var xml = bpe.LerXML<Unimake.Business.DFe.Xml.BPe.ConsSitBPe>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\eventoBPe-110111-cancelamento.xml")]
        [InlineData(@"..\..\..\BPe\Resources\eventoBPe-110115-nao-embarque.xml")]
        [InlineData(@"..\..\..\BPe\Resources\eventoBPe-110116-alteracao-poltrona.xml")]
        [InlineData(@"..\..\..\BPe\Resources\eventoBPe-110117-excesso-bagagem.xml")]
        [InlineData(@"..\..\..\BPe\Resources\eventoBPe-110300-vinc-pgto.xml")]
        [InlineData(@"..\..\..\BPe\Resources\eventoBPe-110301-canc-vinc-pgto.xml")]
        public void SerializacaoDesserializacaoEventoBPe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new Unimake.Business.DFe.Xml.BPe.EventoBPe();
            var xml = bpe.LerXML<Unimake.Business.DFe.Xml.BPe.EventoBPe>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\retBPe.xml")]
        public void SerializacaoDesserializacaoRetBPe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new RetBPe();
            var xml = bpe.LerXML<RetBPe>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\retConsSitBPe.xml")]
        public void SerializacaoDesserializacaoRetConsSitBPe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new RetConsSitBPe();
            var xml = bpe.LerXML<RetConsSitBPe>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\retConsStatServBPe.xml")]
        public void SerializacaoDesserializacaoRetConsStatServBPe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new RetConsStatServBPe();
            var xml = bpe.LerXML<RetConsStatServBPe>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\retEventoBPe.xml")]
        public void SerializacaoDesserializacaoRetEventoBPe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new RetEventoBPe();
            var xml = bpe.LerXML<RetEventoBPe>(doc);

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
                    DhEmi = DateTimeOffset.Parse("2026-07-06T15:00:00-03:00"),
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
