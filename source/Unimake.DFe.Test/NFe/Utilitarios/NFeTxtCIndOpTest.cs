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
/// Garante a conversão do indicador do local da operação no TXT de NF-e.
/// </summary>
[Trait("DFe", "NFe")]
public class NFeTxtCIndOpTest
{
    private static string[] MassaBase() => File.ReadAllLines(Path.Combine(
        Environment.CurrentDirectory, @"NFe\Resources\Txt", "000000001-rtc-zerado-nfe.txt"));

    private static string[] AdicionarAposB(params string[] segmentos)
    {
        var resultado = new List<string>();
        foreach (var linha in MassaBase())
        {
            resultado.Add(linha);
            if (linha.StartsWith("B|", StringComparison.Ordinal)) resultado.AddRange(segmentos);
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
    public void DeveGerarCIndOpNaOrdemDoIdeEManterZeroInicial()
    {
        var linhas = AdicionarAposB("B25d|010104|",
            "F|99999999000191||RUA EXEMPLO|100||CENTRO|3549805|SAO JOSE DO RIO PRETO|SP|15000000|1058|BRASIL|17999999999|||");
        var resultado = Converter(linhas);

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var ide = xml.SelectSingleNode("//*[local-name()='ide']");
        var filhos = ide.ChildNodes.Cast<XmlNode>().Select(no => no.LocalName).ToList();
        Assert.Equal("010104", ide.SelectSingleNode("*[local-name()='cIndOp']")?.InnerText);
        Assert.True(filhos.IndexOf("indPres") < filhos.IndexOf("cIndOp"));
        Assert.True(filhos.IndexOf("cIndOp") < filhos.IndexOf("procEmi"));
        Assert.NotNull(xml.SelectSingleNode("//*[local-name()='retirada']"));

        var validacao = new ValidarSchema();
        validacao.Validar(xml, "NFe.nfe_v4.00.xsd", "http://www.portalfiscal.inf.br/nfe");
        Assert.False(validacao.Success);
        Assert.Contains("Signature", validacao.ErrorMessage);
        Assert.Equal(1, validacao.ErrorMessage.Split(new[] { "Linha:" }, StringSplitOptions.None).Length - 1);
    }

    [Fact]
    public void DeveOmitirCIndOpQuandoSegmentoNaoFoiInformado()
    {
        var resultado = Converter(MassaBase());
        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        Assert.DoesNotContain("<cIndOp>", Assert.Single(resultado.Documentos).Xml);
    }

    [Theory]
    [InlineData("B25d||")]
    [InlineData("B25d|01010|")]
    [InlineData("B25d|0101040|")]
    [InlineData("B25d|01010A|")]
    [InlineData("B25d|010104|EXTRA|")]
    public void DeveCriticarCIndOpForaDoFormatoDoXsd(string segmento)
    {
        var resultado = Converter(AdicionarAposB(segmento));
        Assert.False(resultado.Sucesso);
        Assert.Contains("B25d: cIndOp deve conter exatamente seis dígitos", resultado.MensagemErro);
    }

    [Fact]
    public void DeveCriticarCIndOpRepetido()
    {
        var resultado = Converter(AdicionarAposB("B25d|010104|", "B25d|010105|"));
        Assert.False(resultado.Sucesso);
        Assert.Contains("cIndOp foi informado mais de uma vez", resultado.MensagemErro);
    }

    [Fact]
    public void DeveCriticarCIndOpNaNfce()
    {
        var linhas = AdicionarAposB("B25d|010104|");
        var indiceB = Array.FindIndex(linhas, linha => linha.StartsWith("B|", StringComparison.Ordinal));
        linhas[indiceB] = linhas[indiceB].Replace("|55|1|1|", "|65|1|1|");

        var resultado = Converter(linhas);
        Assert.False(resultado.Sucesso);
        Assert.Contains("cIndOp é permitido somente na NF-e modelo 55", resultado.MensagemErro);
    }

    [Fact]
    public void DeveCriticarCIndOpAntesDaIdentificacao()
    {
        var linhas = new List<string>(MassaBase());
        var indiceB = linhas.FindIndex(linha => linha.StartsWith("B|", StringComparison.Ordinal));
        linhas.Insert(indiceB, "B25d|010104|");

        var resultado = Converter(linhas.ToArray());
        Assert.False(resultado.Sucesso);
        Assert.Contains("informe o segmento B antes de cIndOp", resultado.MensagemErro);
    }
}
