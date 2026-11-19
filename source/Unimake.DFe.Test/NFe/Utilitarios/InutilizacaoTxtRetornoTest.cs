using System.Text;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Utilitarios;

[Trait("DFe", "NFe")]
public class InutilizacaoTxtRetornoTest
{
    [Theory]
    [InlineData(55, 102, false)]
    [InlineData(55, 102, true)]
    [InlineData(65, 102, false)]
    [InlineData(65, 102, true)]
    [InlineData(55, 241, false)]
    [InlineData(55, 241, true)]
    [InlineData(65, 241, false)]
    [InlineData(65, 241, true)]
    public void PreservaBytesDaLinhaComMotivoNoFormatoLegado(int modelo, int codigo, bool comDelimitadores)
    {
        var descricao = codigo == 102 ? "Inutilização homologada" : "Rejeição da faixa";
        var motivo = comDelimitadores ? descricao + "; motivo; sintético" : descricao;
        var motivoEsperado = comDelimitadores ? descricao + "  motivo  sintético" : descricao;
        var faixa = codigo == 102
            ? $"<ano>26</ano><CNPJ>99999999000191</CNPJ><mod>{modelo}</mod><serie>59</serie><nNFIni>50</nNFIni><nNFFin>52</nNFFin>"
            : "";
        var protocolo = codigo == 102 ? "<nProt>135260000000001</nProt>" : "";
        var xml = $"""
            <retInutNFe xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00"><infInut>
              <tpAmb>2</tpAmb><verAplic>TESTE</verAplic><cStat>{codigo}</cStat><xMotivo>{motivo}</xMotivo><cUF>35</cUF>{faixa}
              <dhRecbto>2026-11-19T12:00:00-03:00</dhRecbto>{protocolo}
            </infInut></retInutNFe>
            """;
        var documento = new XmlDocument { XmlResolver = null };
        documento.LoadXml(xml);
        var schema = new ValidarSchema();
        schema.Validar(documento, "NFe.retInutNFe_v4.00.xsd");
        Assert.True(schema.Success, schema.ErrorMessage);

        var retorno = new InutilizacaoTxtConverter().ConverterRetorno(xml);

        Assert.Equal(Encoding.Default.GetBytes($"2;{codigo};{motivoEsperado};35;\r\n"), Encoding.Default.GetBytes(retorno));
    }
}
