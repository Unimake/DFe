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
        public void ConsultarNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            // Carrega fixture e desserializa
            var docFixture = new XmlDocument();
            docFixture.Load(caminhoXml);
            var lido = new ConsultarNfse().LerXML<ConsultarNfse>(docFixture);

            // Sanity checks 
            Assert.Equal("1.00", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfNFSe?.Id));

            // Serializa de volta (round-trip)
            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText,
                "Round-trip diferente do fixture.");

            // Cria do zero com os mesmos valores (pega problemas de defaults/ShouldSerialize)
            var criado = new ConsultarNfse
            {
                Versao = "1.00",
                InfNFSe = new InfNFSe { Id = lido.InfNFSe.Id }
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
        public void ConsultarNFSePorRPSNACIONAL(string arqXml)
        {
            Assert.True(File.Exists(arqXml), $"Arquivo {arqXml} não encontrado.");

            // Carrega fixture e desserializa como DPS
            var docFixture = new XmlDocument();
            docFixture.Load(arqXml);

            var lido = new ConsultarNfsePorRps().LerXML<ConsultarNfsePorRps>(docFixture);

            // Sanity checks
            Assert.Equal("1.00", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfDPS?.Id));

            // Round-trip
            var docRoundTrip = lido.GerarXML();
            Assert.Equal(docFixture.OuterXml, docRoundTrip.OuterXml);

            // Cria do zero com os mesmos valores (root DPS + infDPS)
            var criado = new ConsultarNfsePorRps
            {
                Versao = lido.Versao,
                InfDPS = new InfDPS { Id = lido.InfDPS.Id }
            };
            var docCriado = criado.GerarXML();

            Assert.Equal(docRoundTrip.OuterXml, docCriado.OuterXml);
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\ConsultarNotaPdfEnvio-ped-nfsepdf.xml")]
        public void ConsultarNFSePDFNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();
            docFixture.Load(caminhoXml);

            var lido = new ConsultarNfsePDFEnvio().LerXML<ConsultarNfsePDFEnvio>(docFixture);
            Assert.Equal("1.00", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfNFSe?.Id));

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var criado = new ConsultarNfsePDFEnvio
            {
                Versao = "1.00",
                InfNFSe = new InfNFSe { Id = lido.InfNFSe.Id }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\CancelarNfseEnvio-ped-cannfse.xml")]
        public void CancelarNfseNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();
            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.00", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg?.E101101);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E101101 = new E101101
                    {
                        XDesc = lido.InfPedReg.E101101.XDesc,
                        CMotivo = lido.InfPedReg.E101101.CMotivo,
                        XMotivo = lido.InfPedReg.E101101.XMotivo
                    }
                }
            };


            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\GerarNfseEnvio-env-loterps.xml")]
        public void GerarNfseNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();
            docFixture.Load(caminhoXml);

            var lido = new DPS().LerXML<DPS>(docFixture);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");
        }
    }
}
