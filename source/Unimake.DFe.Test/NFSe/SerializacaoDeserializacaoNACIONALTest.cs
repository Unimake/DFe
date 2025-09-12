using System.IO;
using System.Xml;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL;
using Xunit;

namespace Unimake.DFe.Test.NFSe.NACIONAL
{
    public class SerializacaoDesserializacaoNacionalTest
    {
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\ConsultarNfseEnvio-ped-sitnfse.xml")]
        public void ConsultarNFSeEnvio(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            // Carrega fixture e desserializa
            var docFixture = new XmlDocument();
            docFixture.Load(caminhoXml);
            var lido = new ConsultarNfseEnvio().LerXML<ConsultarNfseEnvio>(docFixture);

            // Sanity checks 
            Assert.Equal("1.00", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfNFSe?.Id));

            // Serializa de volta (round-trip)
            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText,
                "Round-trip diferente do fixture.");

            // Cria do zero com os mesmos valores (pega problemas de defaults/ShouldSerialize)
            var criado = new ConsultarNfseEnvio
            {
                Versao = "1.00",
                InfNFSe = new InfNFSeConsulta { Id = lido.InfNFSe.Id }
            };
            var docCriado = criado.GerarXML();

            // Compara com o round-trip (mesmo serializer, expectativa idêntica)
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText,
                "XML criado do zero difere do XML do round-trip (possível problema de defaults/namespace).");
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\ConsultarNfseRpsEnvio-ped-sitnfserps.xml")]
        public void ConsultarNFSePorRPSEnvio(string arqXml)
        {
            Assert.True(File.Exists(arqXml), $"Arquivo {arqXml} não encontrado.");

            // Carrega fixture e desserializa
            var docFixture = new XmlDocument();
            docFixture.Load(arqXml);
            var lido = new ConsultarNfseEnvio().LerXML<ConsultarNfsePorRpsEnvio>(docFixture);

            // Sanity checks 
            Assert.Equal("1.00", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfDPS?.Id));

            // Serializa de volta (round-trip)
            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText,
                "Round-trip diferente do fixture.");

            // Cria do zero com os mesmos valores (pega problemas de defaults/ShouldSerialize)
            var criado = new ConsultarNfseEnvio
            {
                Versao = "1.00",
                InfNFSe = new InfNFSeConsulta { Id = lido.InfDPS.Id }
            };
            var docCriado = criado.GerarXML();

            // Compara com o round-trip (mesmo serializer, expectativa idêntica)
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText,
                "XML criado do zero difere do XML do round-trip (possível problema de defaults/namespace).");
        }
    }
}
