using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Utilitarios;

/// <summary>
/// Testa os segmentos TXT novos diretamente no conversor da DLL.
/// </summary>
[Trait("DFe", "NFe")]
public class NFeTxtNovoSchemaTest
{
    private static string[] MassaBase() => File.ReadAllLines(Path.Combine(
        Environment.CurrentDirectory, @"NFe\Resources\Txt", "000000001-rtc-zerado-nfe.txt"));

    private static string[] AdicionarApos(string[] linhas, string prefixo, params string[] segmentos)
    {
        var resultado = new List<string>();
        foreach (var linha in linhas)
        {
            resultado.Add(linha);
            if (linha.StartsWith(prefixo, StringComparison.Ordinal)) resultado.AddRange(segmentos);
        }
        return resultado.ToArray();
    }

    private static NFeTxtConversaoResultado Converter(string[] linhas)
    {
        var arquivo = Path.GetTempFileName();
        try
        {
            File.WriteAllLines(arquivo, linhas);
            return new NFeTxtConverter().Converter(arquivo);
        }
        finally
        {
            File.Delete(arquivo);
        }
    }

    [Fact]
    public void DeveGerarValoresLiquidosIcmsPrevistoEPagamentosVinculados()
    {
        var linhas = MassaBase();
        var indicePrimeiroIcms = Array.FindIndex(linhas, linha => linha.StartsWith("N02|", StringComparison.Ordinal));
        linhas[indicePrimeiroIcms] = "NB01|0.00|";
        linhas = AdicionarApos(linhas, "I|", "I11A|25.0000000001|37.50|");
        linhas = AdicionarApos(linhas, "W02|", "W01A|75.30|");
        linhas = AdicionarApos(linhas, "YA|",
            "YV01|1|TRANSACAO-01|01|99999999000191|99999999|",
            "YV01|990|TRANSACAO-02|03|99999999000191|99999999|");

        var resultado = Converter(linhas);
        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        Assert.Equal(2, xml.SelectNodes("//*[local-name()='prod']/*[local-name()='vUnComLiq']").Count);
        Assert.Equal("25.0000000001", xml.SelectSingleNode("//*[local-name()='vUnComLiq']")?.InnerText);
        Assert.Equal("37.50", xml.SelectSingleNode("//*[local-name()='vProdLiq']")?.InnerText);
        Assert.Equal("0.00", xml.SelectSingleNode("//*[local-name()='ICMSPrevistoPagtoAntecip']/*[local-name()='vICMSPrevisto']")?.InnerText);
        Assert.Equal("75.30", xml.SelectSingleNode("//*[local-name()='total']/*[local-name()='vProdLiqTot']")?.InnerText);
        Assert.Equal(2, xml.SelectNodes("//*[local-name()='gPgtoVinc']/*[local-name()='gPgto']").Count);
        Assert.Equal("990", xml.SelectNodes("//*[local-name()='gPgtoVinc']/*[local-name()='gPgto']")[1].Attributes["nPag"].Value);
        Assert.Equal("TRANSACAO-02", xml.SelectNodes("//*[local-name()='gPgtoVinc']/*[local-name()='gPgto']")[1].Attributes["idTransacao"].Value);

        var filhosProduto = xml.SelectSingleNode("//*[local-name()='prod']").ChildNodes.Cast<XmlNode>().Select(no => no.LocalName).ToList();
        var indiceValorBruto = filhosProduto.IndexOf("vProd");
        Assert.Equal(new[] { "vProd", "vUnComLiq", "vProdLiq", "cEANTrib" }, filhosProduto.Skip(indiceValorBruto).Take(4));
        Assert.Equal("vProdLiqTot", xml.SelectSingleNode("//*[local-name()='total']").FirstChild.LocalName);

        var validacao = new ValidarSchema();
        validacao.Validar(xml, "NFe.nfe_v4.00.xsd", "http://www.portalfiscal.inf.br/nfe");
        Assert.False(validacao.Success);
        Assert.Contains("Signature", validacao.ErrorMessage);
        Assert.Equal(1, validacao.ErrorMessage.Split(new[] { "Linha:" }, StringSplitOptions.None).Length - 1);
    }

    [Fact]
    public void DeveOmitirNovosCamposQuandoTxtNaoOsInforma()
    {
        var resultado = Converter(MassaBase());
        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = Assert.Single(resultado.Documentos).Xml;
        foreach (var tag in new[] { "vUnComLiq", "vProdLiq", "vProdLiqTot", "ICMSPrevistoPagtoAntecip", "gPgtoVinc" })
        {
            Assert.DoesNotContain("<" + tag, xml);
        }
    }

    [Fact]
    public void DevePreservarValoresLiquidosZeradosInformadosExplicitamente()
    {
        var linhas = AdicionarApos(MassaBase(), "I|", "I11A|0.0000000000|0.00|");
        linhas = AdicionarApos(linhas, "W02|", "W01A|0.00|");

        var resultado = Converter(linhas);
        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        Assert.Equal(2, xml.SelectNodes("//*[local-name()='vUnComLiq' and text()='0']").Count);
        Assert.Equal(2, xml.SelectNodes("//*[local-name()='vProdLiq' and text()='0.00']").Count);
        Assert.Equal("0.00", xml.SelectSingleNode("//*[local-name()='vProdLiqTot']")?.InnerText);

        var validacao = new ValidarSchema();
        validacao.Validar(xml, "NFe.nfe_v4.00.xsd", "http://www.portalfiscal.inf.br/nfe");
        Assert.False(validacao.Success);
        Assert.Contains("Signature", validacao.ErrorMessage);
        Assert.Equal(1, validacao.ErrorMessage.Split(new[] { "Linha:" }, StringSplitOptions.None).Length - 1);
    }

    [Fact]
    public void DevePreservarCnpjAlfanumericoDosPagamentosVinculados()
    {
        var resultado = Converter(AdicionarApos(MassaBase(), "YA|",
            "YV01|1|TRANSACAO-ALFANUMERICA|01|ABCD1234EFGH91|ABCD1234|"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        Assert.Equal("ABCD1234EFGH91", xml.SelectSingleNode("//*[local-name()='gPgtoVinc']/*[local-name()='gPgto']/*[local-name()='CNPJReceb']")?.InnerText);
        Assert.Equal("ABCD1234", xml.SelectSingleNode("//*[local-name()='gPgtoVinc']/*[local-name()='gPgto']/*[local-name()='CNPJBasePSP']")?.InnerText);

        var validacao = new ValidarSchema();
        validacao.Validar(xml, "NFe.nfe_v4.00.xsd", "http://www.portalfiscal.inf.br/nfe");
        Assert.False(validacao.Success);
        Assert.Contains("Signature", validacao.ErrorMessage);
        Assert.Equal(1, validacao.ErrorMessage.Split(new[] { "Linha:" }, StringSplitOptions.None).Length - 1);
    }

    [Theory]
    [InlineData("I11A|25.00||", "vUnComLiq e vProdLiq")]
    [InlineData("I11A||37.50|", "vUnComLiq e vProdLiq")]
    [InlineData("I11A|25.12345678901|37.50|", "casas decimais")]
    public void DeveCriticarParLiquidoIncompletoOuPrecisaoInvalida(string segmento, string erro)
    {
        var resultado = Converter(AdicionarApos(MassaBase(), "I|", segmento));
        Assert.False(resultado.Sucesso);
        Assert.Contains(erro, resultado.MensagemErro);
    }

    [Fact]
    public void DeveCriticarIcmsPrevistoComOutroIcmsNoMesmoItem()
    {
        var resultado = Converter(AdicionarApos(MassaBase(), "N02|", "NB01|0.00|"));
        Assert.False(resultado.Sucesso);
        Assert.Contains("NB01 junto com outro grupo de ICMS", resultado.MensagemErro);
    }

    [Theory]
    [InlineData("YV01|1|TRANSACAO-01|01|99999999000191||", "CNPJBasePSP")]
    [InlineData("YV01|0|TRANSACAO-01|01|99999999000191|99999999|", "nPag")]
    [InlineData("YV01|01|TRANSACAO-01|01|99999999000191|99999999|", "zeros à esquerda")]
    [InlineData("YV01|991|TRANSACAO-01|01|99999999000191|99999999|", "nPag")]
    [InlineData("YV01|1|TRANSACAO-01|AB|99999999000191|99999999|", "tpMeioPgto deve conter dois dígitos")]
    [InlineData("YV01|1|TRANSACAO-01|01|ABCd1234EFGH91|ABCD1234|", "CNPJReceb")]
    [InlineData("YV01|1|TRANSACAO-01|01|ABCD1234EFGH9A|ABCD1234|", "CNPJReceb")]
    [InlineData("YV01|1|TRANSACAO-01|01|ABCD1234EFGH91|ABcD1234|", "CNPJBasePSP")]
    public void DeveCriticarPagamentoIncompletoOuInvalido(string segmento, string erro)
    {
        var resultado = Converter(AdicionarApos(MassaBase(), "YA|", segmento));
        Assert.False(resultado.Sucesso);
        Assert.Contains(erro, resultado.MensagemErro);
    }

    [Theory]
    [InlineData("YV01|1|TRANSACAO-02|03|99999999000191|99999999|", "nPag repetido")]
    [InlineData("YV01|2|TRANSACAO-01|03|99999999000191|99999999|", "idTransacao repetido")]
    public void DeveCriticarIdentificadoresDePagamentoRepetidos(string segundoPagamento, string erro)
    {
        var resultado = Converter(AdicionarApos(MassaBase(), "YA|",
            "YV01|1|TRANSACAO-01|01|99999999000191|99999999|", segundoPagamento));
        Assert.False(resultado.Sucesso);
        Assert.Contains(erro, resultado.MensagemErro);
    }

    [Fact]
    public void DeveCriticarMaisDeNoventaENovePagamentosVinculados()
    {
        var segmentos = Enumerable.Range(1, 100)
            .Select(numero => "YV01|" + numero + "|TRANSACAO-" + numero.ToString("000") +
                "|01|99999999000191|99999999|")
            .ToArray();
        var resultado = Converter(AdicionarApos(MassaBase(), "YA|", segmentos));
        Assert.False(resultado.Sucesso);
        Assert.Contains("no máximo 99 pagamentos", resultado.MensagemErro);
    }
}
