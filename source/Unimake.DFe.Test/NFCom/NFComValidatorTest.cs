using System.IO;
using System.Xml;
using Unimake.Business.DFe.Validator;
using Xunit;

namespace Unimake.DFe.Test.NFCom
{
    /// <summary>
    /// Classe de teste unitário para testar o validator da NFCom
    /// </summary>
    public class NFComValidatorTest
    {
        /// <summary>
        /// Testa o XML da NFCom com tags da RTC com CSTs 000, 200 e 410
        /// </summary>
        /// <param name="arqXML">Caminho do arquivo XML que será testado</param>
        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\RTC\NFCom_CST000_CST200_CST410.xml")]
        public void ValidarNFComComIBSCBSPeloValidator(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da validação.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            Assert.True(ValidatorFactory.BuidValidator(doc.InnerXml)?.Validate());
        }
    }
}
