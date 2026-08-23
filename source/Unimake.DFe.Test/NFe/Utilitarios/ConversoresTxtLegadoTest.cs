using System;
using System.IO;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Business.DFe.Xml.NFe.Txt;
using Xunit;

namespace Unimake.DFe.Test.NFe.Utilitarios;

public class ConversoresTxtLegadoTest
{
    private static TxtConversaoContexto Contexto => new() { TipoAmbiente = TipoAmbiente.Homologacao, CodigoUF = UFBrasil.PR, Modelo = ModeloDFe.NFe, TipoEmissao = TipoEmissao.Normal };

    [Theory]
    [InlineData("tpAmb|2\r\ncUF|41\r\nversao|4.00", "consStatServ")]
    [InlineData("tpAmb|2\r\nchNFe|41260812345678000195550010000000011000000010\r\nversao|4.00", "consSitNFe")]
    [InlineData("UF|PR\r\nCNPJ|12345678000195\r\nversao|2.00", "ConsCad")]
    public void ConvertePedidosSimples(string txt, string raiz)
    {
        var arquivo = Criar(txt);
        try
        {
            var resultado = raiz switch
            {
                "consStatServ" => new ConsultaStatusTxtConverter().Converter(arquivo, Contexto),
                "consSitNFe" => new ConsultaSituacaoTxtConverter().Converter(arquivo, Contexto),
                _ => new ConsultaCadastroTxtConverter().Converter(arquivo, Contexto)
            };
            Assert.True(resultado.Sucesso);
            Assert.Contains("<" + raiz, resultado.Xml);
        }
        finally { File.Delete(arquivo); }
    }

    [Fact]
    public void ConverteConsultaGtin()
    {
        var arquivo = Criar("GTIN|7891234567895\r\nversao|1.00");
        try
        {
            var resultado = new Unimake.Business.DFe.Xml.CCG.ConsultaGTINTxtConverter().Converter(arquivo, Contexto);
            Assert.True(resultado.Sucesso);
            Assert.Contains("<consGTIN", resultado.Xml);
            Assert.Contains("<GTIN>7891234567895</GTIN>", resultado.Xml);
        }
        finally { File.Delete(arquivo); }
    }

    [Fact]
    public void ConverteInutilizacao()
    {
        var arquivo = Criar("ano|26\r\nCNPJ|12345678000195\r\nserie|1\r\nnNFIni|10\r\nnNFFin|12\r\nxJust|Justificativa sintética válida para inutilização\r\n");
        try
        {
            var resultado = new InutilizacaoTxtConverter().Converter(arquivo, Contexto);
            Assert.True(resultado.Sucesso);
            Assert.Contains("<inutNFe", resultado.Xml);
            Assert.Contains("<nNFIni>10</nNFIni>", resultado.Xml);
        }
        finally { File.Delete(arquivo); }
    }

    [Theory]
    [InlineData(true, "chNFe|41260812345678000195550010000000011000000010", "consChNFe")]
    [InlineData(false, "ultNSU|000000000000001", "distNSU")]
    public void ConverteDistribuicaoNFeECTe(bool nfe, string consulta, string grupo)
    {
        var arquivo = Criar("tpAmb|2\r\ncUFAutor|41\r\nCNPJ|12345678000195\r\n" + consulta);
        try
        {
            var resultado = nfe
                ? new DistribuicaoDFeTxtConverter().Converter(arquivo, Contexto)
                : new Unimake.Business.DFe.Xml.CTe.DistribuicaoDFeTxtConverter().Converter(arquivo, Contexto);
            Assert.True(resultado.Sucesso);
            Assert.Contains("<distDFeInt", resultado.Xml);
            Assert.Contains("<" + grupo + ">", resultado.Xml);
        }
        finally { File.Delete(arquivo); }
    }

    [Fact]
    public void ConverteRetornoStatusNaOrdemLegada()
    {
        const string xml = "<retConsStatServ><tpAmb>2</tpAmb><cStat>107</cStat><xMotivo>Servico em operacao</xMotivo><cUF>41</cUF><dhRecbto>2026-08-22T10:00:00-03:00</dhRecbto><tMed>1</tMed></retConsStatServ>";
        Assert.Equal("2;107;Servico em operacao;41;2026-08-22T10:00:00-03:00;1;\r\n", new ConsultaStatusTxtConverter().ConverterRetorno(xml));
    }

    [Fact]
    public void ConverteLoteDeEventosNoRetorno()
    {
        const string xml = "<retEnvEvento><retEvento><infEvento><tpAmb>2</tpAmb><cOrgao>41</cOrgao><cStat>135</cStat><xMotivo>Registrado</xMotivo><chNFe>41260812345678000195550010000000011000000010</chNFe><tpEvento>110110</tpEvento><xEvento>CCe</xEvento><nSeqEvento>1</nSeqEvento><CNPJDest>12345678000195</CNPJDest><dhRegEvento>2026-08-22T10:00:00-03:00</dhRegEvento><nProt>1</nProt></infEvento></retEvento></retEnvEvento>";
        var retorno = new EventoTxtConverter().ConverterRetorno(xml);
        Assert.Contains("2;41;135;Registrado;", retorno);
        Assert.EndsWith(";1;\r\n", retorno);
    }

    [Fact]
    public void ConverteDoisEventosPreservandoOsRespectivosDados()
    {
        const string chave1 = "41260812345678000195550010000000011000000010";
        const string chave2 = "41260812345678000195550010000000021000000020";
        var arquivo = Criar(
            "idLote|1\r\nevento|1\r\nCNPJ|12345678000195\r\nchNFe|" + chave1 + "\r\ntpEvento|210210\r\n"
            + "evento|2\r\nCNPJ|12345678000195\r\nchNFe|" + chave2 + "\r\ntpEvento|210220\r\n");
        try
        {
            var resultado = new EventoTxtConverter().Converter(arquivo, Contexto);
            Assert.Equal(2, resultado.Xml.Split(new[] { "<evento " }, StringSplitOptions.None).Length - 1);
            Assert.Contains(chave1, resultado.Xml);
            Assert.Contains(chave2, resultado.Xml);
            Assert.Contains("<tpEvento>210210</tpEvento>", resultado.Xml);
            Assert.Contains("<tpEvento>210220</tpEvento>", resultado.Xml);
        }
        finally { File.Delete(arquivo); }
    }

    [Fact]
    public void DistribuicaoDeEventoPreservaNomeTextualDoTipoLegado()
    {
        const string chave = "41260812345678000195550010000000011000000010";
        const string evento = "<procEventoNFe><evento><infEvento><chNFe>" + chave + "</chNFe><tpEvento>110110</tpEvento><nSeqEvento>2</nSeqEvento></infEvento></evento></procEventoNFe>";
        var zip = Unimake.Business.DFe.Utility.Compress.GZIPCompress(evento);
        var retorno = "<retDistDFeInt><tpAmb>2</tpAmb><verAplic>T</verAplic><cStat>138</cStat><xMotivo>OK</xMotivo><dhResp>D</dhResp><ultNSU>1</ultNSU><maxNSU>1</maxNSU><loteDistDFeInt><docZip NSU=\"1\" schema=\"procEventoNFe_v1.00.xsd\">" + zip + "</docZip></loteDistDFeInt></retDistDFeInt>";

        Assert.Contains("procEventoNFe;1;" + chave + ";tpEvCCe;02", new DistribuicaoDFeTxtConverter().ConverterRetorno(retorno));
    }

    private static string Criar(string conteudo)
    {
        var arquivo = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N") + ".txt");
        File.WriteAllText(arquivo, conteudo);
        return arquivo;
    }
}
