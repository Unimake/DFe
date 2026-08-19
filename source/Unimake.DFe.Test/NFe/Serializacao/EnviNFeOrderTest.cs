using System;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;
using System.Xml.Serialization;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Serializacao
{
    /// <summary>
    /// Garante que a ordem dos elementos da NFe não dependa da ordem de reflexão do runtime.
    /// </summary>
    [Trait("DFe", "NFe")]
    public class EnviNFeOrderTest
    {
        /// <summary>
        /// Deve definir Order em todos os atributos XmlElement da estrutura da NFe.
        /// </summary>
        [Fact]
        public void TodosOsXmlElementDevemPossuirOrder()
        {
            var serializer = new XmlSerializer(typeof(EnviNFe));
            Assert.NotNull(serializer);

            var linhas = File.ReadAllLines(CaminhoEnviNFe());
            var atributos = linhas.Where(x => x.Contains("[XmlElement(")).ToArray();
            var semOrder = atributos.Where(x => !Regex.IsMatch(x, @"\bOrder\s*=\s*\d+")).ToArray();

            Assert.True(atributos.Length >= 1000, $"Quantidade inesperada de atributos XmlElement: {atributos.Length}.");
            Assert.True(semOrder.Length == 0, "XmlElement sem Order: " + string.Join(Environment.NewLine, semOrder));
        }

        private static string CaminhoEnviNFe([CallerFilePath] string arquivoTeste = null)
        {
            return Path.GetFullPath(Path.Combine(
                Path.GetDirectoryName(arquivoTeste),
                @"..\..\..\.NET Standard\Unimake.Business.DFe\Xml\NFe\EnviNFe.cs"));
        }
    }
}
