using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.NFGas;
using Xunit;

namespace Unimake.DFe.Test.NFGas
{
    public class SerializacaoDesserializacaoTest
    {
        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\nfgas.xml")]
        public void SerializacaoDesserializacaoNFGas(string arqXML)
        {
            SerializarDesserializar<Unimake.Business.DFe.Xml.NFGas.NFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\nfgasProc.xml")]
        public void SerializacaoDesserializacaoNFGasProc(string arqXML)
        {
            SerializarDesserializar<NFGasProc>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\retNFGas.xml")]
        public void SerializacaoDesserializacaoRetNFGas(string arqXML)
        {
            SerializarDesserializar<RetNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\consStatServNFGas-ped-sta.xml")]
        public void SerializacaoDesserializacaoConsStatServNFGas(string arqXML)
        {
            SerializarDesserializar<ConsStatServNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\retConsStatServNFGas.xml")]
        public void SerializacaoDesserializacaoRetConsStatServNFGas(string arqXML)
        {
            SerializarDesserializar<RetConsStatServNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\consSitNFGas-ped-sit.xml")]
        public void SerializacaoDesserializacaoConsSitNFGas(string arqXML)
        {
            SerializarDesserializar<ConsSitNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\retConsSitNFGas.xml")]
        public void SerializacaoDesserializacaoRetConsSitNFGas(string arqXML)
        {
            SerializarDesserializar<RetConsSitNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\eventoNFGas-110111.xml")]
        public void SerializacaoDesserializacaoEventoNFGas(string arqXML)
        {
            SerializarDesserializar<EventoNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\retEventoNFGas.xml")]
        public void SerializacaoDesserializacaoRetEventoNFGas(string arqXML)
        {
            SerializarDesserializar<RetEventoNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\procEventoNFGas.xml")]
        public void SerializacaoDesserializacaoProcEventoNFGas(string arqXML)
        {
            SerializarDesserializar<ProcEventoNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\evCancNFGas.xml")]
        public void SerializacaoDesserializacaoEvCancNFGas(string arqXML)
        {
            SerializarDesserializar<EvCancNFGas>(arqXML);
        }

        [Fact]
        [Trait("DFe", "NFGas")]
        public void DeveGerarIdComChaveAcessoNFGas()
        {
            var infNFGas = new InfNFGas
            {
                Ide = new Ide
                {
                    CUF = UFBrasil.SP,
                    Mod = ModeloDFe.NFGas,
                    Serie = 1,
                    NNF = 1,
                    CNF = "0000010",
                    DhEmi = "2026-05-18T10:00:00-03:00",
                    TpEmis = TipoEmissaoNFGas.Normal,
                    NSiteAutoriz = "0"
                },
                Emit = new Emit
                {
                    CNPJ = "12345678000195"
                }
            };

            Assert.Equal(44, infNFGas.Chave.Length);
            Assert.StartsWith("3526051234567800019576001000000001100000010", infNFGas.Chave);
            Assert.Equal("NFGas" + infNFGas.Chave, infNFGas.Id);
            Assert.Equal(49, infNFGas.Id.Length);
            Assert.Equal(infNFGas.Chave[43].ToString(), infNFGas.Ide.CDV.ToString());
        }

        private static void SerializarDesserializar<T>(string arqXML) where T : XMLBase, new()
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new T().LerXML<T>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, $"XML gerado pela DLL está diferente do conteúdo do arquivo serializado.\nOriginal: {doc.InnerText}\nGerado: {doc2.InnerText}");
        }
    }
}
