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
    [InlineData("NFe_ALCZFMCBS_Tipo1-nfe.txt")]
    [InlineData("NFe_ALCZFMCBS_Tipo2-nfe.txt")]
    [InlineData("NFe_ReformaTributaria_3_prods-nfe.txt")]
    [InlineData("NFe_Reforma_Tributaria-nfe.txt")]
    [InlineData("NFe_Reforma_Tributaria_Monofasica-nfe.txt")]
    [InlineData("NFE_Venda_00002.txt")]
    [InlineData("NFe_Venda_para_o_Governo.txt")]
    [InlineData("NFCe-4.00.txt")]
    [InlineData("versaoprouducao-nfe-orig.txt")]
    [InlineData("000580_08606985000105_001-nfe.txt")]
    [InlineData("0000072301054300027600116072026-NFE-orig.txt")]
    [InlineData("0000092301054300027600116072026-NFE-orig.txt")]
    [InlineData("0000112301054300027600116072026-NFE-orig.txt")]
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
    [InlineData("NFe_000250887_07_43_31-nfe-orig.txt", "1dfeb1ca43795977a207c36fda58365832ff7ee99605692c6dd9895c2a65e870")]
    [InlineData("0000042301054300027600113072026-NFE.txt", "174dc230d9d4174df3e7a3ef14b4d25f1173ac4138812b88c3003b2d0a5b8bd6")]
    [InlineData("CST_SEM_CLASSTRIB_SEM_NotaCredito03Retorno_SemImpostoIBSCBS.txt", "3e2f1e54ac159cd1b03b0024f1317b0eed96e46babe0cc615ac7c9deb1a60a06")]
    [InlineData("NFE_Devolucao_00003.txt", "a927e05abdf374845b43837cfe6f3360c7a07fb312c4be22d994a864fe23b21c")]
    [InlineData("NFe_ReformaTributaria_1_prod-nfe.txt", "d0cd1dc2a69bbf8f4f72f0130a7f993e4e44bcccd8f6e737994b34f2c36ac678")]
    [InlineData("NFe_ReformaTributaria_3_prods-nfe.txt", "e8214766f92cd58e33d430499bd22024c7edacc2c4b72c288307605f31d7f61f")]
    [InlineData("NFe_Reforma_Tributaria-nfe.txt", "aed2a7ab8509318407bc830fb0b7ac663e6ec238bd73641da8a0998bbb58d774")]
    [InlineData("NFe_Reforma_Tributaria_Monofasica-nfe.txt", "7d0689545b29cde304678e9b4b232bac9330ebd64e57be5abcc7041cb85f6928")]
    [InlineData("NFE_Venda_00002.txt", "bbf5b92b9d1afbeb7706af0d2a928905ac46ed4531aa0bcc9383e4fc47f5f300")]
    [InlineData("NFe_Venda_para_o_Governo.txt", "f7d0bb8621a22a7c7cdbadde40dded3d21caffaf5fa0df92d4c6c1ed56522c64")]
    [InlineData("NFCe-4.00.txt", "b59fbb7ff20b02c095265abae22a7b9ca4bfb3bcafc390ff5283e4111b5f4904")]
    [InlineData("versaoprouducao-nfe-orig.txt", "64523fb5d72dce0cbed48f73bd57340dbcefb9b9b6ad7ce7e870e9e4448dd11b")]
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
    /// Deve despachar segmentos cujo sufixo alfabetico foi informado em minusculo no TXT.
    /// </summary>
    [Fact]
    public void ConverterDeveProcessarSegmentoComSufixoEmMinusculo()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("NFCe-4.00.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.NotNull(xml.SelectSingleNode("//*[local-name()='imposto']/*[local-name()='ICMS']/*[local-name()='ICMSSN102']"));
    }

    /// <summary>
    /// Deve selecionar ICMSSN102 pelo CSOSN mesmo quando o TXT utiliza o layout N10c.
    /// </summary>
    [Fact]
    public void ConverterDeveSelecionarIcmsSn102PeloCsosnDoLayoutN10c()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("000580_08606985000105_001-nfe.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.Equal(2, xml.SelectNodes("//*[local-name()='imposto']/*[local-name()='ICMS']/*[local-name()='ICMSSN102']").Count);
        Assert.Null(xml.SelectSingleNode("//*[local-name()='imposto']/*[local-name()='ICMS']/*[local-name()='ICMSSN101']"));
    }

    /// <summary>
    /// Deve manter o volume e o lacre informados, ainda que quantidade e pesos sejam zero.
    /// </summary>
    [Fact]
    public void ConverterDeveManterVolumeZeradoComLacre()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("0000092301054300027600116072026-NFE-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.Equal("000000", xml.SelectSingleNode("//*[local-name()='transp']/*[local-name()='vol']/*[local-name()='lacres']/*[local-name()='nLacre']")?.InnerText);
    }

    /// <summary>
    /// Deve omitir os valores retidos zerados do ICMS60 nas mesmas condições da conversão histórica.
    /// </summary>
    [Fact]
    public void ConverterDeveOmitirValoresRetidosZeradosDoIcms60()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("0000112301054300027600116072026-NFE-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var icms60 = xml.SelectSingleNode("//*[local-name()='det'][2]/*[local-name()='imposto']/*[local-name()='ICMS']/*[local-name()='ICMS60']");

        Assert.NotNull(icms60);
        Assert.Equal(2, icms60.ChildNodes.Count);
        Assert.Null(icms60.SelectSingleNode("*[local-name()='vBCSTRet' or local-name()='pST' or local-name()='vICMSSubstituto' or local-name()='vICMSSTRet']"));
    }

    /// <summary>
    /// Deve escolher o grupo de ICMS pelo CST final e omitir IPI sem CST, como a conversão histórica.
    /// </summary>
    [Fact]
    public void ConverterDeveManterSelecaoHistoricaDosGruposDeImposto()
    {
        var resultadoIcms = new NFeTxtConverter().Converter(CaminhoArquivo("0000112301054300027600116072026-NFE-orig.txt"));
        var resultadoIpi = new NFeTxtConverter().Converter(CaminhoArquivo("0000092301054300027600116072026-NFE-orig.txt"));

        Assert.True(resultadoIcms.Sucesso, resultadoIcms.MensagemErro);
        Assert.True(resultadoIpi.Sucesso, resultadoIpi.MensagemErro);
        var xmlIcms = new XmlDocument();
        var xmlIpi = new XmlDocument();
        xmlIcms.LoadXml(Assert.Single(resultadoIcms.Documentos).Xml);
        xmlIpi.LoadXml(Assert.Single(resultadoIpi.Documentos).Xml);

        Assert.NotNull(xmlIcms.SelectSingleNode("//*[local-name()='det'][1]/*[local-name()='imposto']/*[local-name()='ICMS']/*[local-name()='ICMS00']"));
        Assert.Null(xmlIcms.SelectSingleNode("//*[local-name()='det'][1]/*[local-name()='imposto']/*[local-name()='ICMS']/*[local-name()='ICMS60']"));
        Assert.Null(xmlIpi.SelectSingleNode("//*[local-name()='det'][2]/*[local-name()='imposto']/*[local-name()='IPI']"));
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
    /// Deve converter os grupos de devolucao de tributos informados nos segmentos UB17, UB36 e UB55.
    /// </summary>
    [Fact]
    public void ConverterDeveProcessarGruposDevolucaoTributos()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("NFe_GrupoDevolucaoTributos-nfe.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        ValidarGrupoDevolucaoTributos(xml, "gIBSUF", "25.1234", "0.01");
        ValidarGrupoDevolucaoTributos(xml, "gIBSMun", "30.1234", "0.02");
        ValidarGrupoDevolucaoTributos(xml, "gCBS", "40.1234", "0.03");
    }

    /// <summary>
    /// Nao deve gerar os grupos de devolucao de tributos quando seus campos nao foram informados.
    /// </summary>
    [Fact]
    public void ConverterNaoDeveGerarGruposDevolucaoTributosSemValores()
    {
        var linhas = File.ReadAllLines(CaminhoArquivo("NFe_GrupoDevolucaoTributos-nfe.txt"));
        foreach (var segmento in new[] { "UB17|", "UB36|", "UB55|" })
        {
            var indiceSegmento = Array.FindIndex(linhas, linha => linha.StartsWith(segmento));
            var campos = linhas[indiceSegmento].Split('|');
            campos[4] = string.Empty;
            campos[5] = string.Empty;
            linhas[indiceSegmento] = string.Join("|", campos);
        }

        var resultado = ConverterTemporario(linhas);

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        Assert.Null(xml.SelectSingleNode("//*[local-name()='gIBSUF']/*[local-name()='gDevTrib']"));
        Assert.Null(xml.SelectSingleNode("//*[local-name()='gIBSMun']/*[local-name()='gDevTrib']"));
        Assert.Null(xml.SelectSingleNode("//*[local-name()='gCBS']/*[local-name()='gDevTrib']"));
    }

    /// <summary>
    /// Nao deve gerar o grupo de devolucao quando somente o valor devolvido foi informado.
    /// </summary>
    [Fact]
    public void ConverterNaoDeveGerarGrupoDevolucaoTributosSemPercentual()
    {
        var linhas = File.ReadAllLines(CaminhoArquivo("NFe_GrupoDevolucaoTributos-nfe.txt"));
        foreach (var segmento in new[] { "UB17|", "UB36|", "UB55|" })
        {
            var indiceSegmento = Array.FindIndex(linhas, linha => linha.StartsWith(segmento));
            var campos = linhas[indiceSegmento].Split('|');
            campos[4] = string.Empty;
            linhas[indiceSegmento] = string.Join("|", campos);
        }

        var resultado = ConverterTemporario(linhas);

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        Assert.Null(xml.SelectSingleNode("//*[local-name()='gIBSUF']/*[local-name()='gDevTrib']"));
        Assert.Null(xml.SelectSingleNode("//*[local-name()='gIBSMun']/*[local-name()='gDevTrib']"));
        Assert.Null(xml.SelectSingleNode("//*[local-name()='gCBS']/*[local-name()='gDevTrib']"));
    }

    /// <summary>
    /// Deve converter o grupo de operacoes em areas incentivadas da CBS.
    /// </summary>
    [Theory]
    [InlineData("NFe_ALCZFMCBS_Tipo1-nfe.txt", "1", null, "1.2345", "12.34")]
    [InlineData("NFe_ALCZFMCBS_Tipo2-nfe.txt", "2", "123456789012", "2.3456", "23.45")]
    public void ConverterDeveProcessarGrupoAreasIncentivadasCbs(string nomeArquivo, string tipoEsperado, string processoEsperado, string aliquotaEsperada, string valorEsperado)
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo(nomeArquivo));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var grupo = xml.SelectSingleNode("//*[local-name()='gCBS']/*[local-name()='gALCZFMCBS']");
        Assert.NotNull(grupo);
        Assert.Equal(tipoEsperado, grupo.SelectSingleNode("*[local-name()='tpALCZFMCBS']")?.InnerText);
        Assert.Equal(aliquotaEsperada, grupo.SelectSingleNode("*[local-name()='pAliqEfetRegCBS']")?.InnerText);
        Assert.Equal(valorEsperado, grupo.SelectSingleNode("*[local-name()='vTribRegCBS']")?.InnerText);

        var processo = grupo.SelectSingleNode("*[local-name()='nProcSuframa']");
        if (processoEsperado == null)
        {
            Assert.Null(processo);
        }
        else
        {
            Assert.Equal(processoEsperado, processo?.InnerText);
        }
    }

    /// <summary>
    /// Deve converter a inscricao do emitente na Suframa quando informada no segmento C.
    /// </summary>
    [Fact]
    public void ConverterDeveProcessarIsufEmitInformada()
    {
        var linhas = File.ReadAllLines(CaminhoArquivo("NFE_Venda_00002.txt"));
        var indiceSegmento = Array.FindIndex(linhas, linha => linha.StartsWith("C|"));
        var campos = linhas[indiceSegmento].Split('|');
        campos[8] = "12345678";
        linhas[indiceSegmento] = string.Join("|", campos);

        var resultado = ConverterTemporario(linhas);

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        Assert.Equal("12345678", xml.SelectSingleNode("//*[local-name()='emit']/*[local-name()='ISUFEmit']")?.InnerText);
    }

    /// <summary>
    /// Nao deve gerar a inscricao do emitente na Suframa quando nao informada no segmento C.
    /// </summary>
    [Fact]
    public void ConverterNaoDeveGerarIsufEmitNaoInformada()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("NFE_Venda_00002.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        Assert.Null(xml.SelectSingleNode("//*[local-name()='emit']/*[local-name()='ISUFEmit']"));
    }

    /// <summary>
    /// Deve converter o novo grupo de compra governamental e suas referencias de documentos anteriores.
    /// </summary>
    [Fact]
    public void ConverterDeveProcessarNovoGrupoCompraGovernamental()
    {
        var linhas = File.ReadAllLines(CaminhoArquivo("NFe_Venda_para_o_Governo.txt"));
        var indiceSegmento = Array.FindIndex(linhas, linha => linha.StartsWith("BB01|"));
        var linhasComReferencias = new string[linhas.Length + 1];
        Array.Copy(linhas, 0, linhasComReferencias, 0, indiceSegmento);
        linhasComReferencias[indiceSegmento] = "BB01|6|47.2730|4|12345678901234567890123456789012345678901234|";
        linhasComReferencias[indiceSegmento + 1] = "BB05|12345678901234567890123456789012345678901235|";
        Array.Copy(linhas, indiceSegmento + 1, linhasComReferencias, indiceSegmento + 2, linhas.Length - indiceSegmento - 1);

        var resultado = ConverterTemporario(linhasComReferencias);

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var grupo = xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='gCompraGov']");

        Assert.NotNull(grupo);
        Assert.Equal("6", grupo.SelectSingleNode("*[local-name()='tpEnteGov']")?.InnerText);
        Assert.Equal("47.2730", grupo.SelectSingleNode("*[local-name()='pRedutor']")?.InnerText);
        Assert.Equal("4", grupo.SelectSingleNode("*[local-name()='tpOperGov']")?.InnerText);
        Assert.Equal(2, grupo.SelectNodes("*[local-name()='refDFeAnt']").Count);
    }

    /// <summary>
    /// Nao deve interpretar o antigo segmento B31 como grupo de compra governamental.
    /// </summary>
    [Fact]
    public void ConverterNaoDeveProcessarAntigoBlocoB31()
    {
        var linhas = File.ReadAllLines(CaminhoArquivo("NFe_Venda_para_o_Governo.txt"));
        var indiceSegmento = Array.FindIndex(linhas, linha => linha.StartsWith("BB01|"));
        linhas[indiceSegmento] = "B31|4|60.00|1|";

        var resultado = ConverterTemporario(linhas);

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        Assert.Null(xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='gCompraGov']"));
    }

    private static void ValidarGrupoDevolucaoTributos(XmlDocument xml, string grupoPai, string percentualEsperado, string valorEsperado)
    {
        var grupo = xml.SelectSingleNode("//*[local-name()='" + grupoPai + "']/*[local-name()='gDevTrib']");

        Assert.NotNull(grupo);
        Assert.Equal(percentualEsperado, grupo.SelectSingleNode("*[local-name()='pDevTrib']")?.InnerText);
        Assert.Equal(valorEsperado, grupo.SelectSingleNode("*[local-name()='vDevTrib']")?.InnerText);
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

        Assert.Equal(3, xml.SelectNodes("//*[local-name()='gPagAntecipado']/*[local-name()='refDFe']").Count);
        Assert.Null(xml.SelectSingleNode("//*[local-name()='gPagAntecipado']/*[local-name()='refNFe']"));
        Assert.NotNull(xml.SelectSingleNode("//*[local-name()='IS']/*[local-name()='adRemIS']"));
        Assert.Null(xml.SelectSingleNode("//*[local-name()='IS']/*[local-name()='pISEspec']"));

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
