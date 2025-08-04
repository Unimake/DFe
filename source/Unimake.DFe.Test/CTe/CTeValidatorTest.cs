using System.IO;
using System.Xml;
using Unimake.Business.DFe.Validator;
using Xunit;

namespace Unimake.DFe.Test.CTe
{
    public class CTeValidatorTest
    {
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(@"..\..\..\CTe\Resources\RTC\CTeRodoviario_CST000.xml")]
        [InlineData(@"..\..\..\CTe\Resources\RTC\CTeRodoviario_CST000_CRT_1.xml")]
        [InlineData(@"..\..\..\CTe\Resources\RTC\CTeRodoviario_CST200.xml")]
        [InlineData(@"..\..\..\CTe\Resources\RTC\CTeRodoviario_CST410.xml")]
        public void ValidarCTeComIBSCBS(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da validação.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            Assert.True(ValidatorFactory.BuidValidator(doc.InnerXml)?.Validate());
        }
    }
}
