using System.IO;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL;
using Xunit;
using NFSeXml = Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.NFSe;

namespace Unimake.DFe.Test.NFSe.Servicos
{
    public class RetornoSchemaNacionalTest
    {
        [Fact]
        [Trait("DFe", "NFSe")]
        public void ResultDosServicosDeveLerNFSeAtualizadaSemTransporte()
        {
            var modelo = XMLUtility.Deserializar<NFSeXml>(File.ReadAllText(
                @"..\..\..\NFSe\Resources\NACIONAL\1.01\RetornoNACIONAL.xml"));
            modelo.InfNFSe.DPS = XMLUtility.Deserializar<DPS>(File.ReadAllText(
                @"..\..\..\NFSe\Resources\NACIONAL\1.01\DPS-AjusteBC.xml"));
            modelo.InfNFSe.Valores.VCalcAjusteBCISSQN = 100;
            var xml = modelo.GerarXML();
            var configuracao = new Configuracao { PadraoNFSe = PadraoNFSe.NACIONAL };
            var gerar = new Business.DFe.Servicos.NFSe.GerarNfse
            {
                Configuracoes = configuracao, RetornoWSXML = xml, RetornoWSString = xml.OuterXml
            };
            var consultar = new Business.DFe.Servicos.NFSe.ConsultarNfse
            {
                Configuracoes = configuracao, RetornoWSXML = xml, RetornoWSString = xml.OuterXml
            };
            Assert.Equal(100, gerar.Result.InfNFSe.Valores.VCalcAjusteBCISSQN);
            Assert.Equal(2, consultar.Result.InfNFSe.DPS.InfDPS.IBSCBS.BensMoveis.Count);
        }
    }
}
