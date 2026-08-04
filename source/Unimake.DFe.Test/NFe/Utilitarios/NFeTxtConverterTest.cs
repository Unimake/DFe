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
    [InlineData("novaVersao-nfe.txt")]
    [InlineData("35260747498059000115550010004029951909226874-nfe-orig.txt")]
    [InlineData("002310_01_01_31_07_2026-nfe-orig.txt")]
    [InlineData("000479_09531276000170_003_31_07_2026-nfe-orig.txt")]
    [InlineData("nfe-nfe-orig.txt")]
    [InlineData("14222_43343052000335_1_31_7_2026-nfe-orig.txt")]
    [InlineData("046481_01391063000189_0_03_08_2026-nfe-orig.txt")]
    [InlineData("Nota_Fiscal_20265.txt")]
    [InlineData("20819_22716895000289_1_382026-nfe.txt")]
    [InlineData("2140_01955703000136_4_8_2026-nfe-orig.txt")]
    [InlineData("000071619_37870375000112_001_03_08_2026-nfe-orig.txt")]
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
    /// Deve omitir o desconto da fatura quando o campo correspondente do segmento Y02 estiver vazio.
    /// </summary>
    [Fact]
    public void ConverterDeveOmitirDescontoVazioDaFatura()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("20819_22716895000289_1_382026-nfe.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var documento = Assert.Single(resultado.Documentos);
        var xml = new XmlDocument();
        xml.LoadXml(documento.Xml);

        var fatura = xml.SelectSingleNode("//*[local-name()='cobr']/*[local-name()='fat']");
        Assert.NotNull(fatura);
        Assert.Null(fatura.SelectSingleNode("*[local-name()='vDesc']"));
        Assert.Equal("13961.12", fatura.SelectSingleNode("*[local-name()='vOrig']")?.InnerText);
        Assert.Equal("13961.12", fatura.SelectSingleNode("*[local-name()='vLiq']")?.InnerText);
    }

    /// <summary>
    /// Deve omitir pMVAST do ICMSSN900 quando o campo opcional correspondente estiver vazio no segmento N10h.
    /// </summary>
    [Fact]
    public void ConverterDeveOmitirMargemValorAdicionadoStVaziaDoIcmsSn900()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("2140_01955703000136_4_8_2026-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        var grupos = xml.SelectNodes("//*[local-name()='ICMSSN900']");
        Assert.Equal(149, grupos.Count);
        Assert.Equal(0, xml.SelectNodes("//*[local-name()='ICMSSN900']/*[local-name()='pMVAST']").Count);
    }

    /// <summary>
    /// Deve gerar pMVAST zerado no ICMSSN900 quando a modalidade de cálculo ST utiliza margem de valor agregado.
    /// </summary>
    [Fact]
    public void ConverterDeveGerarMargemValorAdicionadoStZeradaQuandoModalidadeForMva()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("NFe_ReformaTributaria_1_prod-nfe.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        var icms = xml.SelectSingleNode("//*[local-name()='ICMSSN900']");
        Assert.NotNull(icms);
        Assert.Equal("4", icms.SelectSingleNode("*[local-name()='modBCST']")?.InnerText);
        Assert.Equal("0.0000", icms.SelectSingleNode("*[local-name()='pMVAST']")?.InnerText);
    }

    /// <summary>
    /// Deve preservar nas descrições de produto a representação textual de aspas produzida pelo conversor legado.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarAspasDasDescricoesComoLegado()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("2140_01955703000136_4_8_2026-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        var descricoesComAspas = xml.SelectNodes("//*[local-name()='xProd'][contains(text(), '&quot;')]");
        Assert.Equal(4, descricoesComAspas.Count);
        Assert.Equal("BRIDAO DE FERRO MODELO &quot;D&quot; SIMPLES", descricoesComAspas[0].InnerText);
    }

    /// <summary>
    /// Deve omitir os subgrupos opcionais de ST e desoneração do ICMS90 quando todos os respectivos valores estiverem zerados.
    /// </summary>
    [Fact]
    public void ConverterDeveOmitirGruposZeradosDoIcms90()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("000071619_37870375000112_001_03_08_2026-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        var icms90 = xml.SelectSingleNode("//*[local-name()='ICMS90']");
        Assert.NotNull(icms90);
        Assert.Equal(6, icms90.ChildNodes.Count);
        Assert.Null(icms90.SelectSingleNode("*[local-name()='modBCST']"));
        Assert.Null(icms90.SelectSingleNode("*[local-name()='vICMSDeson']"));
        Assert.Equal("11.15", xml.SelectSingleNode("//*[local-name()='det']/*[local-name()='prod']/*[local-name()='vOutro']")?.InnerText);
    }

    /// <summary>
    /// Deve usar PISOutr e COFINSOutr quando Q02 e S02 informarem CST 49, preservando base, alíquota e valor.
    /// </summary>
    [Fact]
    public void ConverterDeveSelecionarPisECofinsOutrosPeloCstDosSegmentosDeAliquota()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("000071619_37870375000112_001_03_08_2026-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.Equal("49", xml.SelectSingleNode("//*[local-name()='PISOutr']/*[local-name()='CST']")?.InnerText);
        Assert.Equal("167.72", xml.SelectSingleNode("//*[local-name()='PISOutr']/*[local-name()='vBC']")?.InnerText);
        Assert.Equal("1.6500", xml.SelectSingleNode("//*[local-name()='PISOutr']/*[local-name()='pPIS']")?.InnerText);
        Assert.Equal("2.77", xml.SelectSingleNode("//*[local-name()='PISOutr']/*[local-name()='vPIS']")?.InnerText);
        Assert.Equal("49", xml.SelectSingleNode("//*[local-name()='COFINSOutr']/*[local-name()='CST']")?.InnerText);
        Assert.Equal("7.6000", xml.SelectSingleNode("//*[local-name()='COFINSOutr']/*[local-name()='pCOFINS']")?.InnerText);
        Assert.Equal("12.75", xml.SelectSingleNode("//*[local-name()='COFINSOutr']/*[local-name()='vCOFINS']")?.InnerText);
    }

    /// <summary>
    /// Deve gerar o hash CSRT usando o segredo informado no ZD concatenado com a chave de acesso definitiva.
    /// </summary>
    [Fact]
    public void ConverterDeveGerarHashCsrtComAChaveDeAcesso()
    {
        const string csrt = "CSRTTESTE0123456789012345678";
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("14222_43343052000335_1_31_7_2026-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var documento = Assert.Single(resultado.Documentos);
        var xml = new XmlDocument();
        xml.LoadXml(documento.Xml);

        var hashCsrt = xml.SelectSingleNode("//*[local-name()='infRespTec']/*[local-name()='hashCSRT']")?.InnerText;
        Assert.Equal(Unimake.Business.DFe.Utility.Converter.CalculateSHA1Hash(csrt + documento.Chave), hashCsrt);
        Assert.NotEqual(csrt, hashCsrt);
    }

    /// <summary>
    /// Deve selecionar IPITrib pelo CST mesmo quando o TXT utiliza o segmento O08 sem valores de cálculo.
    /// </summary>
    [Fact]
    public void ConverterDeveGerarIpiTribParaCst99InformadoNoO08()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("046481_01391063000189_0_03_08_2026-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.Equal(20, xml.SelectNodes("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='IPI']/*[local-name()='IPITrib' and *[local-name()='CST']='99']").Count);
        Assert.Equal(0, xml.SelectNodes("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='IPI']/*[local-name()='IPINT']").Count);
    }

    /// <summary>
    /// Deve omitir os campos opcionais zerados do ICMS51, preservando o XML produzido pelo conversor legado.
    /// </summary>
    [Fact]
    public void ConverterDeveOmitirCamposOpcionaisZeradosDoIcms51()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("Nota_Fiscal_20265.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var icms51 = xml.SelectSingleNode("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='ICMS']/*[local-name()='ICMS51']");

        Assert.NotNull(icms51);
        Assert.Equal(3, icms51.ChildNodes.Count);
        Assert.Equal("0", icms51.SelectSingleNode("*[local-name()='orig']")?.InnerText);
        Assert.Equal("51", icms51.SelectSingleNode("*[local-name()='CST']")?.InnerText);
        Assert.Equal("3", icms51.SelectSingleNode("*[local-name()='modBC']")?.InnerText);
    }

    /// <summary>
    /// Deve selecionar os grupos Outros de PIS e COFINS pelo CST 49 mesmo nos segmentos Q04 e S04.
    /// </summary>
    [Fact]
    public void ConverterDeveGerarPisECofinsOutrosParaCst49InformadoEmQ04ES04()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("Nota_Fiscal_20265.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.NotNull(xml.SelectSingleNode("//*[local-name()='PISOutr']/*[local-name()='CST' and text()='49']"));
        Assert.NotNull(xml.SelectSingleNode("//*[local-name()='COFINSOutr']/*[local-name()='CST' and text()='49']"));
        Assert.Null(xml.SelectSingleNode("//*[local-name()='PISNT']"));
        Assert.Null(xml.SelectSingleNode("//*[local-name()='COFINSNT']"));
    }

    /// <summary>
    /// Deve preservar o IPI devolvido informado no segmento UA.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarIpiDevolvidoDoSegmentoUa()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("Nota_Fiscal_20265.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.Equal("100.00", xml.SelectSingleNode("//*[local-name()='impostoDevol']/*[local-name()='pDevol']")?.InnerText);
        Assert.Equal("150.48", xml.SelectSingleNode("//*[local-name()='impostoDevol']/*[local-name()='IPI']/*[local-name()='vIPIDevol']")?.InnerText);
    }

    /// <summary>
    /// Deve manter as informações adicionais do item depois dos grupos obrigatórios do detalhe.
    /// </summary>
    [Fact]
    public void ConverterDeveGerarInfAdProdDepoisDoImposto()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("002310_01_01_31_07_2026-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var detalhe = xml.SelectSingleNode("//*[local-name()='det']");
        var produto = detalhe.SelectSingleNode("*[local-name()='prod']");
        var imposto = detalhe.SelectSingleNode("*[local-name()='imposto']");
        var informacaoAdicional = detalhe.SelectSingleNode("*[local-name()='infAdProd']");

        Assert.NotNull(produto);
        Assert.NotNull(imposto);
        Assert.NotNull(informacaoAdicional);
        Assert.Same(produto, imposto.PreviousSibling);
        Assert.Same(imposto, informacaoAdicional.PreviousSibling);
        Assert.Equal("DEC:52921/IMP.REC.SUBSTITUICAO/ART.313-Y", informacaoAdicional.InnerText);
    }

    /// <summary>
    /// Deve preservar os campos zerados do ICMS cobrado anteriormente quando a operação não é para consumidor final.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarCamposZeradosDoIcmsSn500ParaNaoConsumidorFinal()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("002310_01_01_31_07_2026-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var detalhe = xml.SelectSingleNode("//*[local-name()='det']");
        var icmsSn500 = detalhe.SelectSingleNode("*[local-name()='imposto']/*[local-name()='ICMS']/*[local-name()='ICMSSN500']");

        Assert.NotNull(icmsSn500);
        Assert.Equal("0.00", icmsSn500.SelectSingleNode("*[local-name()='vBCSTRet']").InnerText);
        Assert.Equal("0.0000", icmsSn500.SelectSingleNode("*[local-name()='pST']").InnerText);
        Assert.Equal("0.00", icmsSn500.SelectSingleNode("*[local-name()='vICMSSubstituto']").InnerText);
        Assert.Equal("0.00", icmsSn500.SelectSingleNode("*[local-name()='vICMSSTRet']").InnerText);
    }

    /// <summary>
    /// Deve preservar os campos zerados do ICMSSN500 em todos os itens informados pelo TXT.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarIcmsSn500ZeradoEmTodosOsItens()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("000479_09531276000170_003_31_07_2026-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var grupos = xml.SelectNodes("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='ICMS']/*[local-name()='ICMSSN500']");

        Assert.Equal(2, grupos.Count);
        foreach (XmlNode grupo in grupos)
        {
            Assert.Equal("0.00", grupo.SelectSingleNode("*[local-name()='vBCSTRet']").InnerText);
            Assert.Equal("0.0000", grupo.SelectSingleNode("*[local-name()='pST']").InnerText);
            Assert.Equal("0.00", grupo.SelectSingleNode("*[local-name()='vICMSSubstituto']").InnerText);
            Assert.Equal("0.00", grupo.SelectSingleNode("*[local-name()='vICMSSTRet']").InnerText);
        }
    }

    /// <summary>
    /// Deve selecionar os grupos não tributados de PIS e COFINS pelo CST, mesmo nos segmentos Q05 e S05.
    /// </summary>
    [Fact]
    public void ConverterDeveSelecionarPisECofinsNaoTributadosPeloCst()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("nfe-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.Equal(5, xml.SelectNodes("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='PIS']/*[local-name()='PISNT' and *[local-name()='CST']='04']").Count);
        Assert.Equal(5, xml.SelectNodes("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='COFINS']/*[local-name()='COFINSNT' and *[local-name()='CST']='04']").Count);
        Assert.Equal(0, xml.SelectNodes("//*[local-name()='PISOutr']").Count);
        Assert.Equal(0, xml.SelectNodes("//*[local-name()='COFINSOutr']").Count);
    }

    /// <summary>
    /// Deve manter o XML de referência de cada TXT de regressão durante a migração para o modelo oficial.
    /// </summary>
    [Theory]
    [InlineData("NFe_000250887_07_43_31-nfe-orig.txt", "7eb292fea3549c5ceedd9220d8ed328a012a2e5263edf5366dbafcd80118a482")]
    [InlineData("0000042301054300027600113072026-NFE.txt", "174dc230d9d4174df3e7a3ef14b4d25f1173ac4138812b88c3003b2d0a5b8bd6")]
    [InlineData("CST_SEM_CLASSTRIB_SEM_NotaCredito03Retorno_SemImpostoIBSCBS.txt", "7bcbe40ef98b8e84d5687f028953f18a4b7f18525b3f3eece1a64538092fa8cd")]
    [InlineData("NFE_Devolucao_00003.txt", "a927e05abdf374845b43837cfe6f3360c7a07fb312c4be22d994a864fe23b21c")]
    [InlineData("NFe_ReformaTributaria_1_prod-nfe.txt", "d0cd1dc2a69bbf8f4f72f0130a7f993e4e44bcccd8f6e737994b34f2c36ac678")]
    [InlineData("NFe_ReformaTributaria_3_prods-nfe.txt", "e8214766f92cd58e33d430499bd22024c7edacc2c4b72c288307605f31d7f61f")]
    [InlineData("NFe_Reforma_Tributaria-nfe.txt", "9e4bfde2755564884af7d5fda2a4526cae24ee1b9732a677087c3fa20f3e1dfe")]
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
        var hashAtual = BitConverter.ToString(hash).Replace("-", string.Empty).ToLowerInvariant();

        Assert.True(hashEsperado == hashAtual, $"Hash esperado: {hashEsperado}. Hash atual: {hashAtual}.");
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
    /// Deve converter a nova versao do TXT com destinatario sem indIEDest e grupos de IBS/CBS.
    /// </summary>
    [Fact]
    public void ConverterDeveProcessarNovaVersaoComDestinatarioEReformaTributaria()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("novaVersao-nfe.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.Equal("9", xml.SelectSingleNode("//*[local-name()='dest']/*[local-name()='indIEDest']")?.InnerText);
        Assert.Equal(12, xml.SelectNodes("//*[local-name()='infNFe']/*[local-name()='det']").Count);
        Assert.Equal(12, xml.SelectNodes("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='IBSCBS']").Count);
    }

    /// <summary>
    /// Nao deve gerar indDeduzDeson isolado no ICMS20 quando vICMSDeson for zero.
    /// </summary>
    [Fact]
    public void ConverterNaoDeveGerarIndDeduzDesonNoIcms20SemValorDesonerado()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("35260747498059000115550010004029951909226874-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.Equal(3, xml.SelectNodes("//*[local-name()='ICMS20']").Count);
        Assert.Null(xml.SelectSingleNode("//*[local-name()='ICMS20']/*[local-name()='indDeduzDeson']"));
    }

    /// <summary>
    /// Deve preservar a redução do IBS municipal na posição do layout legado do segmento UB36.
    /// </summary>
    [Fact]
    public void ConverterDeveGerarReducaoIbsMunicipalDoUb36LegadoComCampoFinalAdicional()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("35260747498059000115550010004029951909226874-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        var reducaoIbsMunicipal = xml.SelectSingleNode("//*[local-name()='det'][12]/*[local-name()='imposto']/*[local-name()='IBSCBS']/*[local-name()='gIBSCBS']/*[local-name()='gIBSMun']/*[local-name()='gRed']");
        Assert.NotNull(reducaoIbsMunicipal);
        Assert.Equal("60.0000", reducaoIbsMunicipal.SelectSingleNode("*[local-name()='pRedAliq']")?.InnerText);
        Assert.Equal("0.0000", reducaoIbsMunicipal.SelectSingleNode("*[local-name()='pAliqEfet']")?.InnerText);
    }

    /// <summary>
    /// As massas TXT não devem voltar a conter os dados identificáveis removidos durante a anonimização.
    /// </summary>
    [Fact]
    public void MassasTxtNaoDevemConterDadosIdentificaveisConhecidos()
    {
        var dadosIdentificaveis = new[]
        {
            "EMERSON SILVA GUEDES",
            "contato@roguelimp.com.br",
            "05976103804",
            "mepagodi@gmail.com",
            "WONENFE@GMAIL.COM",
            "JULIANO KOCH",
            "51999626374",
            "suporte@microprisma.com.br",
            "WYLBER NASSA",
            "DEBORA PJ",
            "RUA SANTO ANDRE|134",
            "R. PEDRO VITORATO",
            "RUA GENERAL MARIANTE",
            "48577324915",
            "04690036934",
            "92991289953",
            "SOC.COM.MAT.P/CONSTR.LUIZ LOPES LTDA",
            "00454749000109",
            "108680702113",
            "956224310481",
            "RUA MAJOR OTAVIANO",
            "R. OLIVEIRA CATRAMBI",
            "1122911633",
            "11997556655",
            "luizlopes.nfe@uol.com.br",
            "VENDEDOR: 0110 WAGNER",
            "AUTO VIDROS PRUDENTE",
            "562319803111",
            "592009166115",
            "45523719000811",
            "RUA ANTONIO RUIZ",
            "AVENIDA XV DE NOVEMBRO",
            "Marco Thomaz",
            "marco@duesoft.com.br",
            "1839167600",
            "PLACA: FRT 6828",
            "J. R. DE OLIVEIRA AUTO ELETRICA",
            "38136977000103",
            "03640467000194",
            "401300590118",
            "401035229111",
            "1436215947",
            "36025222",
            "RUA OTAVIO CONEGUNDES DE SOUZA",
            "SUPERM. JAU SERVE LTDA",
            "nfe@jauserve.com.br",
            "carlos.tagiarolli@jauserve.com.br",
            "AVENIDA JOAO SANZOVO",
            "Florestal Alvorada Florestamento e Reflorestamento Ltda",
            "Industria de Compensados Sudati Ltda",
            "João Henrique Buckta",
            "joao.henrique@valorflorestal.com.br",
            "8X77VU0XB39URUYTGYSU7IU14UQB",
            "NET LIGHT LTDA.",
            "notafiscal@zummo.com.br",
            "RUA MATOS COSTA",
            "278064462111",
            "1146128926",
            "ARVENSIS COSMETICOS LTDA",
            "ARVENSIS COSMETICOS",
            "GMN EMBALAGENS LTDA",
            "RUA DOMICIANO MARTINS DE ANDRADE",
            "RUA DR MILTON LADEIRA",
            "3232258011",
            "J.A. HARD NUTRITION",
            "PEDRO HENRIQUE MELLO CASAGRANDE",
            "materiaprimasuplementosfw@gmail.com",
            "Rua Jambeiro",
            "R 21 DE ABRIL",
            "ATIVA DISTRIBUICAO E LOGISTICA LTDA",
            "sac@underlabznutrition.com",
            "M-126863",
            "134004",
            "0042882644996",
            "0618231258819",
            "Conquista Industria de Artigos Para Selaria",
            "CONQUISTA IND. DE ART. P/SELARIA",
            "Rua Ezidio Balladelli",
            "RUA EZIDIO BALADELLI",
            "DEOCLECIO ALVES DE ARAUJO",
            "RUA MEN DE SA",
            "59623500904",
            "9013566450",
            "0443351392",
            "CENTERKASA COMERCIAL LTDA",
            "NOVA ROCHA IND TINTAS LTDA",
            "devolucoes@leinertex.com.br",
            "AV ANAPOLIS",
            "AV JATAI",
            "VILA CONCORDIA",
            "PQ IND AP VICE P JOSE ALENCAR",
            "102575584",
            "103120939",
            "6232081448",
            "6232750800",
            "420396)"
        };

        var pasta = Path.GetDirectoryName(CaminhoArquivo("novaVersao-nfe.txt"));
        foreach (var arquivo in Directory.GetFiles(pasta, "*.txt"))
        {
            var conteudo = File.ReadAllText(arquivo);
            foreach (var dadoIdentificavel in dadosIdentificaveis)
            {
                Assert.True(
                    conteudo.IndexOf(dadoIdentificavel, StringComparison.OrdinalIgnoreCase) < 0,
                    $"O arquivo '{Path.GetFileName(arquivo)}' contém o dado identificável '{dadoIdentificavel}'.");
            }
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
        var linhas = File.ReadAllLines(CaminhoArquivo("NFE_Venda_00002.txt"));
        var indiceEmissor = Array.FindIndex(linhas, linha => linha.StartsWith("C02|"));
        var camposEmissor = linhas[indiceEmissor].Split('|');
        camposEmissor[1] = "08606985000105";
        linhas[indiceEmissor] = string.Join("|", camposEmissor);

        var indiceSegmentoB = Array.FindIndex(linhas, linha => linha.StartsWith("B|"));
        var camposSegmentoB = linhas[indiceSegmentoB].Split('|');
        camposSegmentoB[14] = string.Empty;
        linhas[indiceSegmentoB] = string.Join("|", camposSegmentoB);

        var conversaoValida = ConverterTemporario(linhas);
        Assert.True(conversaoValida.Sucesso, conversaoValida.MensagemErro);
        var chaveValida = Assert.Single(conversaoValida.Documentos).Chave;
        var chaveInvalida = chaveValida.Substring(0, 43) + (chaveValida.EndsWith("9") ? "8" : "9");

        camposSegmentoB[14] = chaveValida.Substring(43, 1);
        linhas[indiceSegmentoB] = string.Join("|", camposSegmentoB);
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
