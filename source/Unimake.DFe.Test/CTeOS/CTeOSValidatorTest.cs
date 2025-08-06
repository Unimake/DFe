using System.IO;
using System.Xml;
using Unimake.Business.DFe.Validator;
using Xunit;

namespace Unimake.DFe.Test.CTeOS
{
    public class CTeOSValidatorTest
    {
        [Theory]
        [Trait("DFe", "CTeOS")]
        [InlineData(@"..\..\..\CTeOS\Resources\RTC\CTeOS_CST000.xml")]
        [InlineData(@"..\..\..\CTeOS\Resources\RTC\CTeOS_CST000_CRT_1.xml")]
        [InlineData(@"..\..\..\CTeOS\Resources\RTC\CTeOS_CST200.xml")]
        [InlineData(@"..\..\..\CTeOS\Resources\RTC\CTeOS_CST410.xml")]
        public void ValidarCTeOSPeloValidator(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da validação.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            Assert.True(ValidatorFactory.BuidValidator(doc.InnerXml)?.Validate());
        }
    }
}
