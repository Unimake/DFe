using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Validator;
using Unimake.Exceptions;
using Xunit;

namespace Unimake.DFe.Test.NFe
{
    public class NFeValidatorTest
    {
        /// <summary>
        /// Testa o XML da NFe com tags da RTC com CSTs 000, 550, 200, 410, 510, 620 e 800
        /// </summary>
        /// <param name="arqXML">Caminho do arquivo XML que será testado</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\RTC\NFe_CST000_CST550.xml")]
        [InlineData(@"..\..\..\NFe\Resources\RTC\NFe_CST200_CST410.xml")]
        [InlineData(@"..\..\..\NFe\Resources\RTC\NFe_CST510_CST620.xml")]
        [InlineData(@"..\..\..\NFe\Resources\RTC\NFe_CST800.xml")]
        public void ValidarNFeComIBSCBS(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da validação.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            Assert.True(ValidatorFactory.BuidValidator(doc.InnerXml)?.Validate());
        }

        /// <summary>
        /// Testa o XML da NFCe com tags da RTC com CSTs que não devem ser utilizadas para esse modelo: 550 e 800
        /// </summary>
        /// <param name="arqXML">Caminho do arquivo XML que será testado</param>
        [Theory]
        [Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\RTC\NFCe_CST550.xml")]
        [InlineData(@"..\..\..\NFe\Resources\RTC\NFCe_CST800.xml")]
        public void ValidarNFCeComIBSCBS(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da validação.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var excecao = Assert.Throws<ValidatorDFeException>(() => ValidatorFactory.BuidValidator(doc.InnerXml)?.Validate());

            Assert.Contains($"Para o modelo {ModeloDFe.NFCe.ToString()}, o CST", excecao.Message);
        }
    }
}
