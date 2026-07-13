using Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta;
using Xunit;

namespace Unimake.DFe.Test.NFSe.Serializacao
{
    public class NFSeNacionalCLocIncidTest
    {
        [Fact]
        [Trait("DFe", "NFSe")]
        public void DeveDesserializarCLocIncidComNilComoZero()
        {
            var xml = @"<temp>
  <LoteDFe>
    <NSU>57</NSU>
    <TipoDocumento>NFSE</TipoDocumento>
    <ArquivoXml>
      <NFSe xmlns=""http://www.sped.fazenda.gov.br/nfse"" versao=""1.01"">
        <infNFSe>
          <xLocEmi>Municipio emissor</xLocEmi>
          <nNFSe>123</nNFSe>
          <cLocIncid p3:nil=""true"" xmlns:p3=""http://www.w3.org/2001/XMLSchema-instance""/>
          <cStat>100</cStat>
        </infNFSe>
      </NFSe>
    </ArquivoXml>
  </LoteDFe>
</temp>";

            var retDistribuicaoNFSe = new RetDistribuicaoNFSe().LoadFromXML(xml);

            Assert.Equal("57", retDistribuicaoNFSe.LoteDFe[0].NSU);
            Assert.Equal(0, retDistribuicaoNFSe.LoteDFe[0].ArquivoXml.NFSe.InfNFSe.CLocIncid);
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void DeveDesserializarCLocIncidComValorNormal()
        {
            var xml = @"<temp>
  <LoteDFe>
    <NSU>26</NSU>
    <TipoDocumento>NFSE</TipoDocumento>
    <ArquivoXml>
      <NFSe xmlns=""http://www.sped.fazenda.gov.br/nfse"" versao=""1.01"">
        <infNFSe>
          <xLocEmi>Municipio emissor</xLocEmi>
          <nNFSe>123</nNFSe>
          <cLocIncid>4314902</cLocIncid>
          <cStat>100</cStat>
        </infNFSe>
      </NFSe>
    </ArquivoXml>
  </LoteDFe>
</temp>";

            var retDistribuicaoNFSe = new RetDistribuicaoNFSe().LoadFromXML(xml);

            Assert.Equal("26", retDistribuicaoNFSe.LoteDFe[0].NSU);
            Assert.Equal(4314902, retDistribuicaoNFSe.LoteDFe[0].ArquivoXml.NFSe.InfNFSe.CLocIncid);
        }
    }
}
