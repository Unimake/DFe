using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe.NACIONAL
{
    public class ValidacaoSchemaTests
    {
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\2025_09_26-env-loterps.xml")]
        public void ValidarDPS(string xmlDPS)
        {
            Assert.True(File.Exists(xmlDPS), $"Arquivo {xmlDPS} não encontrado.");

            var xmlDoc = new XmlDocument();
            xmlDoc.Load(xmlDPS);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = TipoAmbiente.Producao,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeGerarNfse,
                SchemaVersao = "1.00"
            };

            var gerarNFSe = new GerarNfse(xmlDoc.OuterXml, configuracao);
            //gerarNFSe.Executar();
        }
    }
}
