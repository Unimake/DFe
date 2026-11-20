using System;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Business.DFe.Xml.NFe.Txt;
using Xunit;

namespace Unimake.DFe.Test.NFe.Utilitarios;

[Trait("DFe", "NFe")]
public class CodificacaoEntradaTxtTest
{
    private static readonly Encoding Windows1252 = CodePagesEncodingProvider.Instance.GetEncoding(1252);
    private const string Texto = "Justificação sintética para teste de codificação";

    [Theory]
    [InlineData("UTF8")]
    [InlineData("UTF8BOM")]
    [InlineData("UTF16LE")]
    [InlineData("UTF16BE")]
    [InlineData("UTF32LE")]
    [InlineData("UTF32BE")]
    [InlineData("Windows1252")]
    [InlineData("Ambigua")]
    public void DetectaCodificacaoDaEntradaNosDoisLeitores(string caso)
    {
        var formato = caso switch
        {
            "UTF8" => new UTF8Encoding(false),
            "UTF8BOM" => new UTF8Encoding(true),
            "UTF16LE" => Encoding.Unicode,
            "UTF16BE" => Encoding.BigEndianUnicode,
            "UTF32LE" => new UTF32Encoding(false, true),
            "UTF32BE" => new UTF32Encoding(true, true),
            _ => Windows1252
        };
        var entrada = caso == "Ambigua" ? "Teste sintetico caf\u00c3\u00a9" : Texto;
        var esperado = caso == "Ambigua" ? "Teste sintetico café" : Texto;
        var nfe = new NFeTxtConverter();
        var inutilizacao = new InutilizacaoTxtConverter();
        ComArquivo(ComPreambulo(formato, CriarNota(entrada)), arquivo =>
        {
            var resultado = nfe.ConverterComDeteccaoCodificacao(arquivo, Windows1252);
            Assert.True(resultado.Sucesso, resultado.MensagemErro);
            Assert.Equal(esperado, Campo(Assert.Single(resultado.Documentos).Xml, "xProd"));
        });
        ComArquivo(ComPreambulo(formato, CriarInutilizacao(entrada)), arquivo =>
        {
            var resultado = inutilizacao.Converter(arquivo, Contexto(Windows1252));
            Assert.Equal(esperado, Campo(resultado.Xml, "xJust"));
        });
    }

    [Theory]
    [InlineData(8)]
    [InlineData(16)]
    [InlineData(32)]
    public void BomComConteudoTruncadoDeveFalharSemFallback(int formato)
    {
        var codificacao = formato switch
        {
            8 => new UTF8Encoding(true),
            16 => Encoding.Unicode,
            _ => Encoding.UTF32
        };
        ComArquivo([.. ComPreambulo(codificacao, CriarNota(Texto)), 0xC3], arquivo =>
        {
            var resultado = new NFeTxtConverter().ConverterComDeteccaoCodificacao(arquivo, Windows1252);
            Assert.False(resultado.Sucesso);
            Assert.Empty(resultado.Documentos);
        });
        ComArquivo([.. ComPreambulo(codificacao, CriarInutilizacao(Texto)), 0xC3], arquivo =>
        {
            Assert.Throws<DecoderFallbackException>(() =>
                new InutilizacaoTxtConverter().Converter(arquivo, Contexto(Windows1252)));
        });
    }

    [Fact]
    public void Utf8ValidoComLayoutInvalidoContinuaFalhando()
    {
        ComArquivo(Encoding.UTF8.GetBytes("SEGMENTO INVÁLIDO"), arquivo =>
        {
            var nota = new NFeTxtConverter().ConverterComDeteccaoCodificacao(arquivo, Windows1252);
            Assert.False(nota.Sucesso);
            Assert.Empty(nota.Documentos);
            Assert.Throws<FormatException>(() =>
                new InutilizacaoTxtConverter().Converter(arquivo, Contexto(Windows1252)));
        });
    }

    [Fact]
    public void Utf8TruncadoSemBomPermiteWindows1252()
    {
        var texto = CriarInutilizacao("Justificativa sintetica café");
        ComArquivo(Windows1252.GetBytes(texto), arquivo =>
        {
            var resultado = new InutilizacaoTxtConverter().Converter(arquivo, Contexto(Windows1252));
            Assert.Equal("Justificativa sintetica café", Campo(resultado.Xml, "xJust"));
        });
    }

    [Fact]
    public void EntradasHistoricasContinuamUsandoOPadraoDoRuntime()
    {
        var formato = Encoding.Default;
        var campoEsperado = formato.GetString(Windows1252.GetBytes(Texto));
        var converter = new NFeTxtConverter();
        ComArquivo(Windows1252.GetBytes(CriarNota(Texto)), arquivo =>
        {
            var nota = converter.Converter(arquivo);
            Assert.True(nota.Sucesso, nota.MensagemErro);
            Assert.Equal(campoEsperado, Campo(Assert.Single(nota.Documentos).Xml, "xProd"));
            Assert.Equal(Assert.Single(nota.Documentos).Xml, Assert.Single(converter.Converter(arquivo, VersaoLeiauteMonofasia.Legado).Documentos).Xml);
        });
        ComArquivo(Windows1252.GetBytes(CriarInutilizacao(Texto)), arquivo =>
        {
            var resultado = new InutilizacaoTxtConverter().Converter(arquivo, Contexto());
            Assert.Equal(campoEsperado, Campo(resultado.Xml, "xJust"));
        });
    }

#if INTEROP
    [Fact]
    public void SuperficieComMantemMembrosHistoricosEOcultaPoliticaGerenciada()
    {
        var conversor = typeof(NFeTxtConverter);
        var contexto = typeof(TxtConversaoContexto);
        Assert.Equal(ClassInterfaceType.AutoDual, conversor.GetCustomAttribute<ClassInterfaceAttribute>().Value);
        Assert.Equal("Unimake.Business.DFe.Xml.NFe.NFeTxtConverter", conversor.GetCustomAttribute<ProgIdAttribute>().Value);
        Assert.Equal(ClassInterfaceType.AutoDual, contexto.GetCustomAttribute<ClassInterfaceAttribute>().Value);
        Assert.Equal("Unimake.Business.DFe.Xml.NFe.Txt.TxtConversaoContexto", contexto.GetCustomAttribute<ProgIdAttribute>().Value);

        var novoMetodo = conversor.GetMethod(nameof(NFeTxtConverter.ConverterComDeteccaoCodificacao));
        var novaPropriedade = contexto.GetProperty(nameof(TxtConversaoContexto.CodificacaoWindows1252));
        Assert.False(novoMetodo.GetCustomAttribute<ComVisibleAttribute>().Value);
        Assert.False(novaPropriedade.GetCustomAttribute<ComVisibleAttribute>().Value);
        Assert.NotNull(conversor.GetMethod(nameof(NFeTxtConverter.Converter), [typeof(string)]));
        Assert.NotNull(conversor.GetMethod(nameof(NFeTxtConverter.Converter), [typeof(string), typeof(VersaoLeiauteMonofasia)]));
        Assert.NotNull(typeof(InutilizacaoTxtConverter).GetMethod(nameof(InutilizacaoTxtConverter.Converter), [typeof(string), contexto]));
    }
#endif

    private static TxtConversaoContexto Contexto(Encoding codificacao = null) =>
        new() { CodigoUF = UFBrasil.PR, Modelo = ModeloDFe.NFe, TipoAmbiente = TipoAmbiente.Homologacao, CodificacaoWindows1252 = codificacao };

    private static string Campo(string xml, string nome)
    {
        var documento = new XmlDocument { XmlResolver = null };
        documento.LoadXml(xml);
        return documento.GetElementsByTagName(nome, "http://www.portalfiscal.inf.br/nfe")[0].InnerText;
    }

    private static byte[] ComPreambulo(Encoding codificacao, string texto) =>
        [.. codificacao.GetPreamble(), .. codificacao.GetBytes(texto)];

    private static void ComArquivo(byte[] bytes, Action<string> verificar)
    {
        var arquivo = Path.GetTempFileName();
        try
        {
            File.WriteAllBytes(arquivo, bytes);
            verificar(arquivo);
            Assert.Equal(bytes, File.ReadAllBytes(arquivo));
        }
        finally
        {
            File.Delete(arquivo);
        }
    }

    private static string CriarInutilizacao(string justificativa) =>
        $"tpAmb|2\r\ncUF|41\r\nano|26\r\nCNPJ|99999999000191\r\nmod|55\r\nserie|59\r\nnNFIni|50\r\nnNFFin|52\r\nxJust|{justificativa}";

    private static string CriarNota(string descricao) => string.Join("\r\n", new[]
    {
        "NOTAFISCAL|", "A|4.00||",
        "B|41|10001001|OPERACAO SINTETICA DE TESTE|55|59|1001|2026-08-10T12:00:00-03:00||1|1|4118402|1|1||2|1|1|1|0|UNINFE-TESTE|||",
        "C|EMPRESA SINTETICA DE TESTE||ISENTO||||1||", "C02|99999999000191|",
        "C05|RUA DE TESTE|100||CENTRO|4118402|MUNICIPIO DE TESTE|PR|87700000|1058|BRASIL||",
        "E|NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL|9||||||",
        "E03|12345678909|",
        "E05|AVENIDA SINTETICA|200||TESTE|4118402|MUNICIPIO DE TESTE|PR|87700000|1058|BRASIL||",
        "H|1||",
        $"I|TESTE-1|SEM GTIN|{descricao}|84714900|||||||5102|UN|1|10.00|10.00|SEM GTIN|UN|1|10.00|||||1||||",
        "N10d|0|102|", "Q04|04|", "S04|04|",
        "W02|0|0|0|0|0|10.00|0|0|0|0|0|0|0|0|10.00|0|",
        "X|9|", "YA|0|90|||||"
    });
}
