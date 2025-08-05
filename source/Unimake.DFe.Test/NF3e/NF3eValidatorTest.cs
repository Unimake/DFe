using System.IO;
using System.Xml;
using Unimake.Business.DFe.Validator;
using Xunit;

namespace Unimake.DFe.Test.NF3e
{
    /// <summary>
    /// Classe de teste unitário para testar o validator da NF3e
    /// </summary>
    public class NF3eValidatorTest
    {
        /// <summary>
        /// Testa o XML da NF3e com tags da RTC com CSTs 000, 200, 410, 510, 550, 830
        /// </summary>
        /// <param name="arqXML">Caminho do arquivo XML que será testado</param>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(@"..\..\..\NF3e\Resources\RTC\NF3e_CST000_CST550_CST830.xml")]
        [InlineData(@"..\..\..\NF3e\Resources\RTC\NF3e_CST200.xml")]
        [InlineData(@"..\..\..\NF3e\Resources\RTC\NF3e_CST410_CST510.xml")]
        public void ValidarNF3eComIBSCBSPeloValidator(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da validação.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            Assert.True(ValidatorFactory.BuidValidator(doc.InnerXml)?.Validate());
        }
    }
}
