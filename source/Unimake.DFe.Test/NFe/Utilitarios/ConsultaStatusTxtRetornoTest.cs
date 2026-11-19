using System.Text;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Utilitarios;

[Trait("DFe", "NFe")]
public class ConsultaStatusTxtRetornoTest
{
    [Theory]
    [InlineData(107, "1", false)]
    [InlineData(107, "1", true)]
    [InlineData(108, null, false)]
    [InlineData(108, null, true)]
    [InlineData(107, "0", false)]
    public void PreservaOLayoutLegadoDoStatus(int codigo, string tMed, bool temDelimitador)
    {
        const string data = "2026-11-19T12:00:00-03:00";
        var descricao = codigo == 107 ? "Serviço em operação" : "Serviço paralisado momentaneamente";
        var motivo = temDelimitador ? descricao + "; motivo; sintético" : descricao;
        var motivoEsperado = temDelimitador ? descricao + "  motivo  sintético" : descricao;
        var elementoTempo = tMed == null ? "" : $"<tMed>{tMed}</tMed>";
        var xml = $"""
            <retConsStatServ xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">
              <tpAmb>2</tpAmb><verAplic>TESTE</verAplic><cStat>{codigo}</cStat><xMotivo>{motivo}</xMotivo>
              <cUF>41</cUF><dhRecbto>{data}</dhRecbto>{elementoTempo}
            </retConsStatServ>
            """;
        var documento = new XmlDocument { XmlResolver = null };
        documento.LoadXml(xml);
        var schema = new ValidarSchema();
        schema.Validar(documento, "NFe.retConsStatServ_v4.00.xsd");
        Assert.True(schema.Success, schema.ErrorMessage);

        var retorno = new ConsultaStatusTxtConverter().ConverterRetorno(xml);

        var tempoEsperado = tMed == null ? "" : tMed + ";";
        Assert.Equal(Encoding.Default.GetBytes($"2;{codigo};{motivoEsperado};41;{data};{tempoEsperado}\r\n"),
            Encoding.Default.GetBytes(retorno));
    }
}
