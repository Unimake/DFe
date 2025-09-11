using System.IO;
using System.Xml;
using Unimake.Business.DFe.Xml.NFSe.Nacional;
using Xunit;

namespace Unimake.DFe.Test.NFSe.Nacional
{
    public class SerializacaoDesserializacaoNacionalTest
    {
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\ConsultarNfseEnvio-ped-sitnfse.xml")]
        public void ConsultarNFSeEnvio_RoundTrip_E_CriacaoDoZero_OK(string caminhoXml)
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
    }
}
