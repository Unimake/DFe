using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;
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
        Assert.Equal(44, documento.Chave.Length);

        var xml = new XmlDocument();
        xml.LoadXml(documento.Xml);
        Assert.Equal("NFe", xml.DocumentElement.Name);
        Assert.Equal("http://www.portalfiscal.inf.br/nfe", xml.DocumentElement.NamespaceURI);
        var infNFe = xml.DocumentElement.SelectSingleNode("*[local-name()='infNFe']");
        var id = infNFe.Attributes["Id"].Value;
        Assert.Equal("NFe", id.Substring(0, 3));
        Assert.Equal(47, id.Length);
        Assert.Equal(documento.Chave, id.Substring(3));
        Assert.Equal(documento.Chave.Substring(43, 1), xml.DocumentElement.SelectSingleNode("*[local-name()='infNFe']/*[local-name()='ide']/*[local-name()='cDV']").InnerText);
    }

    /// <summary>
    /// Deve manter o XML de referência de cada TXT de regressão durante a migração para o modelo oficial.
    /// </summary>
    [Theory]
    [InlineData("NFe_000250887_07_43_31-nfe-orig.txt", "4751f82da7d984b6eff0269380ac1a0078a8a458a06a0a57ac8f08fdc6808e7d")]
    [InlineData("0000042301054300027600113072026-NFE.txt", "656b2e4a2a8da0aba599edd1b45c1efdc563d58e15d9862f1df14b69ec5f6030")]
    [InlineData("CST_SEM_CLASSTRIB_SEM_NotaCredito03Retorno_SemImpostoIBSCBS.txt", "d65f0564a93e1f047b58b3980f5de1b6f637eb3a5f291ffe9a00df2f0f799ef6")]
    [InlineData("NFE_Devolucao_00003.txt", "a927e05abdf374845b43837cfe6f3360c7a07fb312c4be22d994a864fe23b21c")]
    [InlineData("NFe_ReformaTributaria_1_prod-nfe.txt", "1fc754371080a87ee42d99fdf39ef9891ebddaeaea74549e6c0aa51cb26ed91e")]
    [InlineData("NFe_ReformaTributaria_3_prods-nfe.txt", "e8214766f92cd58e33d430499bd22024c7edacc2c4b72c288307605f31d7f61f")]
    [InlineData("NFe_Reforma_Tributaria-nfe.txt", "d33ccbd3d3ccaec70aaddbb305c86372edfb35f175bd835ab8b536f816e68cbf")]
    [InlineData("NFe_Reforma_Tributaria_Monofasica-nfe.txt", "7d0689545b29cde304678e9b4b232bac9330ebd64e57be5abcc7041cb85f6928")]
    [InlineData("NFE_Venda_00002.txt", "bbf5b92b9d1afbeb7706af0d2a928905ac46ed4531aa0bcc9383e4fc47f5f300")]
    [InlineData("NFe_Venda_para_o_Governo.txt", "fc1c485846a22b0bd63bf1c886d9c8fba1f485383948f2ee4049c2b355243316")]
    [InlineData("NFCe-4.00.txt", "8d00d8b5128ca917ca6231ce4f72e654b24d16a04a640c99521e57f933aff040")]
    public void ConverterDeveManterXmlDeReferencia(string nomeArquivo, string hashEsperado)
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo(nomeArquivo));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = Assert.Single(resultado.Documentos).Xml;
        var bytes = Encoding.UTF8.GetBytes(xml);
        var hash = SHA256.Create().ComputeHash(bytes);

        Assert.Equal(hashEsperado, BitConverter.ToString(hash).Replace("-", string.Empty).ToLowerInvariant());
    }

    /// <summary>
    /// Deve manter o campo de chave do segmento A opcional quando ele contem somente o tipo do documento.
    /// </summary>
    [Theory]
    [InlineData("NFe_000250887_07_43_31-nfe-orig.txt")]
    [InlineData("NFCe-4.00.txt")]
    public void ConverterDeveAceitarSegmentoAComApenasTipoDocumento(string nomeArquivo)
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo(nomeArquivo));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        Assert.NotEmpty(resultado.Documentos);
    }

    /// <summary>
    /// Deve comparar o marcador do segmento A sem diferenciar maiusculas e apos remover espacos.
    /// </summary>
    [Theory]
    [InlineData("nfe")]
    [InlineData(" NFCe ")]
    public void ConverterDeveNormalizarMarcadorExatoDoSegmentoA(string marcador)
    {
        var linhas = File.ReadAllLines(CaminhoArquivo("NFE_Venda_00002.txt"));
        var indiceSegmentoA = Array.FindIndex(linhas, linha => linha.StartsWith("A|"));
        var campos = linhas[indiceSegmentoA].Split('|');
        campos[2] = marcador;
        linhas[indiceSegmentoA] = string.Join("|", campos);

        var resultado = ConverterTemporario(linhas);

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        Assert.Single(resultado.Documentos);
    }

    /// <summary>
    /// Deve rejeitar layouts anteriores ao 4.00, que nao fazem parte do contrato do conversor.
    /// </summary>
    [Fact]
    public void ConverterDeveRejeitarVersaoDiferenteDeQuatro()
    {
        var linhas = File.ReadAllLines(CaminhoArquivo("NFE_Venda_00002.txt"));
        var indiceSegmentoA = Array.FindIndex(linhas, linha => linha.StartsWith("A|"));
        var campos = linhas[indiceSegmentoA].Split('|');
        campos[1] = "3.10";
        linhas[indiceSegmentoA] = string.Join("|", campos);

        var resultado = ConverterTemporario(linhas);

        Assert.False(resultado.Sucesso);
        Assert.Empty(resultado.Documentos);
        Assert.Contains("Somente a versão 4.00 da NFe/NFCe é suportada", resultado.MensagemErro);
    }

    /// <summary>
    /// Deve rejeitar conteudo que apenas contenha o marcador NFe, pois somente o marcador exato pode ser ignorado.
    /// </summary>
    [Fact]
    public void ConverterDeveRejeitarMarcadorParcialNoSegmentoA()
    {
        var linhas = File.ReadAllLines(CaminhoArquivo("NFE_Venda_00002.txt"));
        var indiceSegmentoA = Array.FindIndex(linhas, linha => linha.StartsWith("A|"));
        var campos = linhas[indiceSegmentoA].Split('|');
        campos[2] = "abcNFe";
        linhas[indiceSegmentoA] = string.Join("|", campos);

        var resultado = ConverterTemporario(linhas);

        Assert.False(resultado.Sucesso);
        Assert.Empty(resultado.Documentos);
        Assert.Contains("Chave de acesso inválida no segmento A", resultado.MensagemErro);
    }

    /// <summary>
    /// Deve preservar a validacao obrigatoria do grupo de pagamento existente no conversor anterior.
    /// </summary>
    [Fact]
    public void ConverterDeveRejeitarNotaSemPagamento()
    {
        var linhasOriginais = File.ReadAllLines(CaminhoArquivo("NFE_Venda_00002.txt"));
        var linhasSemPagamento = Array.FindAll(linhasOriginais, linha => !linha.StartsWith("YA|"));

        var resultado = ConverterTemporario(linhasSemPagamento);

        Assert.False(resultado.Sucesso);
        Assert.Empty(resultado.Documentos);
        Assert.Contains("Falta definir valores do pagamento, tag <pag>.", resultado.MensagemErro);
    }

    /// <summary>
    /// Deve converter todas as notas existentes no mesmo arquivo TXT.
    /// </summary>
    [Fact]
    public void ConverterDeveRetornarTodasAsNotasDoArquivo()
    {
        var nota = File.ReadAllLines(CaminhoArquivo("NFE_Venda_00002.txt"));

        var resultado = ConverterTemporario(CombinarNotas(nota, nota));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        Assert.Equal(2, resultado.Documentos.Count);
        Assert.All(resultado.Documentos, documento => Assert.False(string.IsNullOrWhiteSpace(documento.Xml)));
    }

    /// <summary>
    /// Nao deve retornar a primeira nota quando uma nota posterior do mesmo arquivo for invalida.
    /// </summary>
    [Fact]
    public void ConverterDeveDescartarTodoOLoteQuandoUmaNotaForInvalida()
    {
        var primeiraNota = File.ReadAllLines(CaminhoArquivo("NFE_Venda_00002.txt"));
        var segundaNota = (string[])primeiraNota.Clone();
        var indiceSegmentoA = Array.FindIndex(segundaNota, linha => linha.StartsWith("A|"));
        var campos = segundaNota[indiceSegmentoA].Split('|');
        campos[2] = "abcNFe";
        segundaNota[indiceSegmentoA] = string.Join("|", campos);

        var resultado = ConverterTemporario(CombinarNotas(primeiraNota, segundaNota));

        Assert.False(resultado.Sucesso);
        Assert.Empty(resultado.Documentos);
        Assert.Contains("Chave de acesso inválida no segmento A", resultado.MensagemErro);
    }

    /// <summary>
    /// Deve preservar a convencao historica em que cDV zero representa campo nao informado no TXT.
    /// </summary>
    [Fact]
    public void ConverterDeveTratarCdvZeroComoNaoInformado()
    {
        var linhas = File.ReadAllLines(CaminhoArquivo("NFE_Venda_00002.txt"));
        var segmentoB = Array.Find(linhas, linha => linha.StartsWith("B|"));

        Assert.Equal("0", segmentoB.Split('|')[14]);

        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("NFE_Venda_00002.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        Assert.Single(resultado.Documentos);
    }

    /// <summary>
    /// Deve manter os ajustes pontuais exigidos pelo XML de referencia da conversao TXT.
    /// </summary>
    [Fact]
    public void ConverterDeveAplicarAjustesDeCompatibilidadeDoXml()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("NFe_Reforma_Tributaria-nfe.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.Equal(3, xml.SelectNodes("//*[local-name()='gPagAntecipado']/*[local-name()='refNFe']").Count);
        Assert.Null(xml.SelectSingleNode("//*[local-name()='gPagAntecipado']/*[local-name()='refDFe']"));
        Assert.NotNull(xml.SelectSingleNode("//*[local-name()='IS']/*[local-name()='pISEspec']"));
        Assert.Null(xml.SelectSingleNode("//*[local-name()='IS']/*[local-name()='adRemIS']"));

        foreach (XmlElement elemento in xml.SelectNodes("//*[local-name()='gIBSCBS']/*[local-name()='vIBS']"))
        {
            Assert.True(decimal.Parse(elemento.InnerText, System.Globalization.CultureInfo.InvariantCulture) >= 0);
        }
    }

    /// <summary>
    /// Deve preservar a ausencia de indDeduzDeson no ICMS40 quando vICMSDeson nao foi informado.
    /// </summary>
    [Fact]
    public void ConverterNaoDeveGerarIndDeduzDesonNoIcms40SemVicmsDeson()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("NFe_000250887_07_43_31-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        Assert.NotNull(xml.SelectSingleNode("//*[local-name()='ICMS40']"));
        Assert.Null(xml.SelectSingleNode("//*[local-name()='ICMS40']/*[local-name()='indDeduzDeson']"));
    }

    /// <summary>
    /// Deve rejeitar o cDV informado no segmento B quando diverge da chave calculada.
    /// </summary>
    [Fact]
    public void ConverterDeveRejeitarDigitoVerificadorDivergenteNoSegmentoB()
    {
        var linhas = File.ReadAllLines(CaminhoArquivo("NFE_Venda_00002.txt"));
        var indiceSegmentoB = Array.FindIndex(linhas, linha => linha.StartsWith("B|"));
        var campos = linhas[indiceSegmentoB].Split('|');
        campos[14] = campos[14] == "9" ? "8" : "9";
        linhas[indiceSegmentoB] = string.Join("|", campos);

        ValidarFalhaDeDigitoVerificador(linhas);
    }

    /// <summary>
    /// Deve rejeitar o cDV da chave de 44 posicoes recebida pelo segmento A quando diverge do calculado.
    /// </summary>
    [Fact]
    public void ConverterDeveRejeitarDigitoVerificadorDivergenteNaChaveDoSegmentoA()
    {
        var arquivo = CaminhoArquivo("NFE_Venda_00002.txt");
        var chaveValida = Assert.Single(new NFeTxtConverter().Converter(arquivo).Documentos).Chave;
        var chaveInvalida = chaveValida.Substring(0, 43) + (chaveValida.EndsWith("9") ? "8" : "9");
        var linhas = File.ReadAllLines(arquivo);
        var indiceSegmentoA = Array.FindIndex(linhas, linha => linha.StartsWith("A|"));
        var campos = linhas[indiceSegmentoA].Split('|');
        campos[2] = chaveInvalida;
        linhas[indiceSegmentoA] = string.Join("|", campos);

        ValidarFalhaDeDigitoVerificador(linhas);
    }

    private static string CaminhoArquivo(string nomeArquivo) =>
        Path.Combine(Environment.CurrentDirectory, @"NFe\Resources\Txt", nomeArquivo);

    private static void ValidarFalhaDeDigitoVerificador(string[] linhas)
    {
        var resultado = ConverterTemporario(linhas);

        Assert.False(resultado.Sucesso);
        Assert.Empty(resultado.Documentos);
        Assert.Contains("Dígito verificador informado no TXT diverge da chave de acesso calculada.", resultado.MensagemErro);
    }

    private static NFeTxtConversaoResultado ConverterTemporario(string[] linhas)
    {
        var arquivoTemporario = Path.GetTempFileName();
        try
        {
            File.WriteAllLines(arquivoTemporario, linhas);
            return new NFeTxtConverter().Converter(arquivoTemporario);
        }
        finally
        {
            File.Delete(arquivoTemporario);
        }
    }

    private static string[] CombinarNotas(string[] primeiraNota, string[] segundaNota)
    {
        var resultado = new string[primeiraNota.Length + segundaNota.Length - 1];
        Array.Copy(primeiraNota, 0, resultado, 0, primeiraNota.Length);
        Array.Copy(segundaNota, 1, resultado, primeiraNota.Length, segundaNota.Length - 1);
        return resultado;
    }
}
