using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Utilitarios;

/// <summary>
/// Testa a conversão de TXT de NFe/NFCe para XML em memória.
/// </summary>
[Trait("DFe", "NFe")]
public class NFeTxtConverterTest
{
    /// <summary>
    /// Deve converter um TXT de regressão sem criar arquivos XML no disco.
    /// </summary>
    [Theory]
    [InlineData("NFe_000250887_07_43_31-nfe-orig.txt")]
    [InlineData("0000042301054300027600113072026-NFE.txt")]
    [InlineData("CST_SEM_CLASSTRIB_SEM_NotaCredito03Retorno_SemImpostoIBSCBS.txt")]
    [InlineData("NFE_Devolucao_00003.txt")]
    [InlineData("NFe_ReformaTributaria_1_prod-nfe.txt")]
    [InlineData("NFe_ReformaTributaria_3_prods-nfe.txt")]
    [InlineData("NFe_Reforma_Tributaria-nfe.txt")]
    [InlineData("NFe_Reforma_Tributaria_Monofasica-nfe.txt")]
    [InlineData("NFE_Venda_00002.txt")]
    [InlineData("NFe_Venda_para_o_Governo.txt")]
    [InlineData("NFCe-4.00.txt")]
    public void ConverterDeveRetornarXmlEmMemoria(string nomeArquivo)
    {
        var arquivo = Path.Combine(Environment.CurrentDirectory, @"NFe\Resources\Txt", nomeArquivo);
        var resultado = new NFeTxtConverter().Converter(arquivo);

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var documento = Assert.Single(resultado.Documentos);
        Assert.False(string.IsNullOrWhiteSpace(documento.Xml));
        Assert.False(string.IsNullOrWhiteSpace(documento.Chave));

        var xml = new XmlDocument();
        xml.LoadXml(documento.Xml);
        Assert.Equal("NFe", xml.DocumentElement.Name);
    }
}
