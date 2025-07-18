using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.NFSe
{
    public class ValidarXmlNfseNacionalTest
    {
        [Theory]
        [Trait("NFSe", "ValidarXmlNfseNacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\GerarNfseEnvio-env-loterps.xml", "NFSe.NACIONAL.DPS_v1.00.xsd")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\GerarNfseEnvio_RTC-env-loterps.xml", "NFSe.NACIONAL.DPS_v1.00.xsd")]
        public void ValidarXmlNfseNacional(string arqXML, string arqXSD)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização do teste de obter o tipo do XML.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var validar = new ValidarSchema();
            validar.Validar(doc, arqXSD, "http://www.sped.fazenda.gov.br/nfse", PadraoNFSe.NACIONAL);

            Assert.True(validar.Success, "Ocorreu um erro na validação de SCHEMA: \n" + validar.ErrorMessage);
        }
    }
}
