using System;
using System.IO;
using System.Text;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFe.Txt;
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

    [Theory]
    [InlineData("", TipoEmissao.Normal)]
    [InlineData("tpEmis|1", TipoEmissao.Normal)]
    [InlineData("tpEmis|6", TipoEmissao.ContingenciaSVCAN)]
    [InlineData("TPEMIS|07", TipoEmissao.ContingenciaSVCRS)]
    [InlineData("tpEmis|6\r\ntpEmis|1", TipoEmissao.Normal)]
    [InlineData("tpEmis|9", TipoEmissao.Normal)]
    public void PedidoParaEnvio_SelecionaRotaSemAlterarXml(string campos, TipoEmissao esperado)
    {
        var arquivo = Path.GetTempFileName();
        try
        {
            File.WriteAllText(arquivo, "tpAmb|2\r\ncUF|41\r\nversao|4.00\r\n" + campos);
            var conversor = new ConsultaStatusTxtConverter();
            var contexto = new TxtConversaoContexto { TipoEmissao = TipoEmissao.ContingenciaSVCAN };
            var resultado = conversor.ConverterParaEnvio(arquivo, contexto, out var tipoEmissao);

            Assert.Equal(esperado, tipoEmissao);
            Assert.Equal(conversor.Converter(arquivo, contexto).Xml, resultado.Xml);
            var documento = new XmlDocument { XmlResolver = null };
            documento.LoadXml(resultado.Xml);
            Assert.Empty(documento.GetElementsByTagName("tpEmis"));
            var schema = new ValidarSchema();
            schema.Validar(documento, "NFe.consStatServ_v4.00.xsd");
            Assert.True(schema.Success, schema.ErrorMessage);
        }
        finally
        {
            File.Delete(arquivo);
        }
    }

    [Fact]
    public void PedidoParaEnvio_RecusaSvcSpEPreservaConversaoAtual()
    {
        var arquivo = Path.GetTempFileName();
        try
        {
            File.WriteAllText(arquivo, "tpAmb|2\r\ncUF|41\r\ntpEmis|8\r\n");
            var conversor = new ConsultaStatusTxtConverter();
            var contexto = new TxtConversaoContexto();

            Assert.True(conversor.Converter(arquivo, contexto).Sucesso);
            var erro = Assert.Throws<FormatException>(() => conversor.ConverterParaEnvio(arquivo, contexto, out _));
            Assert.Contains("SVC-SP", erro.Message);
        }
        finally
        {
            File.Delete(arquivo);
        }
    }

    [Theory]
    [InlineData("tpEmis|6x", "6x")]
    [InlineData("tpEmis|", "")]
    [InlineData("tpEmis|2147483648", "2147483648")]
    [InlineData("tpEmis|6\r\ntpEmis|7x", "7x")]
    [InlineData("tpEmis|6x\r\ntpEmis|7", "6x")]
    public void PedidoParaEnvio_RecusaEmissaoPresenteInvalidaEPreservaConversaoAtual(string campos, string valorInvalido)
    {
        var arquivo = Path.GetTempFileName();
        try
        {
            File.WriteAllText(arquivo, "tpAmb|2\r\ncUF|41\r\n" + campos);
            var conversor = new ConsultaStatusTxtConverter();
            var contexto = new TxtConversaoContexto();

            Assert.True(conversor.Converter(arquivo, contexto).Sucesso);
            var erro = Assert.Throws<FormatException>(() => conversor.ConverterParaEnvio(arquivo, contexto, out _));
            Assert.Contains("tpEmis", erro.Message);
            Assert.Contains($"'{valorInvalido}'", erro.Message);
        }
        finally
        {
            File.Delete(arquivo);
        }
    }
}
