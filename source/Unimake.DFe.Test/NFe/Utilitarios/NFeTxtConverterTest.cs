using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Utility;
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
    [InlineData("58_78789542000182_4_8_2026-nfe-orig.txt")]
    [InlineData("000001_01_01_05_08_2026-nfe-orig.txt")]
    [InlineData("08785-NFe.TXT")]
    [InlineData("31260803742159000170550020000003051000234068-NFE-orig.txt")]
    [InlineData("31260803742159000170550020000003051000234068-NFE-orig-v5.txt")]
    [InlineData("060218_32336224000165_001_06_08_2026-nfe-orig.txt")]
    [InlineData("NT60860218.TXT")]
    [InlineData("27260821287558000170650010001143821778530846-nfe-orig.txt")]
    [InlineData("41260801182867000178550010001800011567804549-nfe-orig.txt")]
    [InlineData("41260806225442000112550010002455051903698959-nfe-orig.txt")]
    [InlineData("nfe000077-NFE.txt")]
    [InlineData("NFe_2998-nfe-orig-v2.txt")]
    [InlineData("NFe_2999-nfe-orig-v2.txt")]
    [InlineData("000023655_11092080000179_001_11_08_2026-nfe-orig.txt")]
    [InlineData("398_15528301000160_1_11_08_2026-NFE-orig.txt")]
    [InlineData("399_15528301000160_1_11_08_2026-NFE-orig.txt")]
    [InlineData("35260847498059000115550010004030011909226990-nfe.txt")]
    [InlineData("35260847498059000115550010004030021004029993-nfe.txt")]
    [InlineData("0000056689-nfe-orig.txt")]
    [InlineData("NFe_000049184_08_27_14-nfe.txt")]
    [InlineData("002320_01_01_17_08_2026-nfe.txt")]
    [InlineData("000017136_19041494000180_001_19_08_2026-nfe-orig.txt")]
    [InlineData("035814-nfe-orig.txt")]
    [InlineData("161540-nfe-orig.txt")]
    [InlineData("000015493-nfe.txt")]
    [InlineData("000000892-nfe.txt")]
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
    /// Deve selecionar os grupos PISAliq e COFINSAliq pelo CST mesmo quando o ERP usa os segmentos Q04 e S04.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarGruposPisECofinsDaNfe35814()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("035814-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var pis = xml.SelectSingleNode("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='PIS']/*");
        var cofins = xml.SelectSingleNode("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='COFINS']/*");

        Assert.Equal("PISAliq", pis?.LocalName);
        Assert.Equal("01", pis?.SelectSingleNode("*[local-name()='CST']")?.InnerText);
        Assert.Equal("0.00", pis?.SelectSingleNode("*[local-name()='vBC']")?.InnerText);
        Assert.Equal("COFINSAliq", cofins?.LocalName);
        Assert.Equal("01", cofins?.SelectSingleNode("*[local-name()='CST']")?.InnerText);
        Assert.Equal("0.00", cofins?.SelectSingleNode("*[local-name()='vBC']")?.InnerText);
    }

    /// <summary>
    /// Deve preservar os impostos por alíquota e os grupos da Reforma Tributária da NFCe 161540.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarImpostosDaNfce161540()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("161540-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.Equal("65", xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='mod']")?.InnerText);
        Assert.Null(xml.SelectSingleNode("//*[local-name()='dest']"));
        Assert.Equal("32.00", xml.SelectSingleNode("//*[local-name()='ICMS00']/*[local-name()='vBC']")?.InnerText);
        Assert.Equal("5.44", xml.SelectSingleNode("//*[local-name()='ICMS00']/*[local-name()='vICMS']")?.InnerText);
        Assert.Equal("26.56", xml.SelectSingleNode("//*[local-name()='PISAliq']/*[local-name()='vBC']")?.InnerText);
        Assert.Equal("0.44", xml.SelectSingleNode("//*[local-name()='PISAliq']/*[local-name()='vPIS']")?.InnerText);
        Assert.Equal("26.56", xml.SelectSingleNode("//*[local-name()='COFINSAliq']/*[local-name()='vBC']")?.InnerText);
        Assert.Equal("2.02", xml.SelectSingleNode("//*[local-name()='COFINSAliq']/*[local-name()='vCOFINS']")?.InnerText);
        Assert.Equal("000001", xml.SelectSingleNode("//*[local-name()='IBSCBS']/*[local-name()='cClassTrib']")?.InnerText);
        Assert.Equal("24.10", xml.SelectSingleNode("//*[local-name()='IBSCBS']/*[local-name()='gIBSCBS']/*[local-name()='vBC']")?.InnerText);
    }

    /// <summary>
    /// Deve omitir percentuais opcionais zerados e preservar os valores do ICMS-ST da NFe 15493.
    /// </summary>
    [Fact]
    public void ConverterDeveOmitirPercentuaisZeradosDoIcms10DaNfe15493()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("000015493-nfe.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var icms = xml.SelectSingleNode("//*[local-name()='ICMS10']");

        Assert.NotNull(icms);
        Assert.Null(icms.SelectSingleNode("*[local-name()='pMVAST']"));
        Assert.Null(icms.SelectSingleNode("*[local-name()='pRedBCST']"));
        Assert.Equal("5585.21", icms.SelectSingleNode("*[local-name()='vBCST']")?.InnerText);
        Assert.Equal("18.0000", icms.SelectSingleNode("*[local-name()='pICMSST']")?.InnerText);
        Assert.Equal("335.12", icms.SelectSingleNode("*[local-name()='vICMSST']")?.InnerText);
    }

    /// <summary>
    /// Deve preservar a referência à nota de produtor, os itens e os pagamentos da NFe 892.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarReferenciaProdutorItensEPagamentosDaNfe892()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("000000892-nfe.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        ValidarReferenciaProdutorItensEPagamentosDaNfe892(xml);

        var validacao = new ValidarSchema();
        validacao.Validar(xml, "NFe.nfe_v4.00.xsd", "http://www.portalfiscal.inf.br/nfe");
        Assert.False(validacao.Success);
        Assert.Contains("Signature", validacao.ErrorMessage);
    }

    /// <summary>
    /// Deve preservar todos os pagamentos e dados dos cartões informados em segmentos YA consecutivos.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarPagamentosDaNfce17136()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("000017136_19041494000180_001_19_08_2026-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var pagamentos = xml.SelectNodes("//*[local-name()='detPag']");
        var cartoes = xml.SelectNodes("//*[local-name()='detPag']/*[local-name()='card']");

        Assert.Equal(5, pagamentos.Count);
        Assert.Equal(4, cartoes.Count);
        Assert.Equal("196.00", pagamentos[0].SelectSingleNode("*[local-name()='vPag']")?.InnerText);
        Assert.Equal("295.00", pagamentos[1].SelectSingleNode("*[local-name()='vPag']")?.InnerText);
        Assert.Equal("300.00", pagamentos[2].SelectSingleNode("*[local-name()='vPag']")?.InnerText);
        Assert.Equal("65.00", pagamentos[3].SelectSingleNode("*[local-name()='vPag']")?.InnerText);
        Assert.Equal("24.00", pagamentos[4].SelectSingleNode("*[local-name()='vPag']")?.InnerText);
        Assert.Equal("AUT001", cartoes[0].SelectSingleNode("*[local-name()='cAut']")?.InnerText);
        Assert.Equal("AUT004", cartoes[3].SelectSingleNode("*[local-name()='cAut']")?.InnerText);

        var validacao = new ValidarSchema();
        validacao.Validar(xml, "NFe.nfe_v4.00.xsd", "http://www.portalfiscal.inf.br/nfe");
        Assert.False(validacao.Success);
        Assert.Contains("Signature", validacao.ErrorMessage);
    }

    /// <summary>
    /// Deve reconhecer o cabeçalho da Reforma Tributária que informa os tipos de nota de
    /// crédito/débito e omite o campo opcional cMunFGIBS.
    /// </summary>
    [Theory]
    [InlineData("35260847498059000115550010004030011909226990-nfe.txt", "6", "07", null, "1", 0)]
    [InlineData("35260847498059000115550010004030021004029993-nfe.txt", "5", null, "03", "0", 2)]
    public void ConverterDeveProcessarNotaDeCreditoEDebitoSemMunicipioFatoGeradorIbs(
        string nomeArquivo,
        string finalidadeEsperada,
        string tipoDebitoEsperado,
        string tipoCreditoEsperado,
        string tipoOperacaoEsperado,
        int referenciasEsperadas)
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo(nomeArquivo));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.Equal(finalidadeEsperada, xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='finNFe']").InnerText);
        Assert.Equal(tipoOperacaoEsperado, xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='tpNF']").InnerText);
        Assert.Equal(tipoDebitoEsperado, xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='tpNFDebito']")?.InnerText);
        Assert.Equal(tipoCreditoEsperado, xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='tpNFCredito']")?.InnerText);
        Assert.Null(xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='cMunFGIBS']"));
        Assert.Equal(referenciasEsperadas, xml.SelectNodes("//*[local-name()='ide']/*[local-name()='NFref']").Count);
        Assert.NotNull(xml.SelectSingleNode("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='ICMS']"));
        Assert.NotNull(xml.SelectSingleNode("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='IBSCBS']"));

        var validacao = new ValidarSchema();
        validacao.Validar(xml, "NFe.nfe_v4.00.xsd", "http://www.portalfiscal.inf.br/nfe");
        Assert.False(validacao.Success);
        Assert.Contains("Signature", validacao.ErrorMessage);
    }

    /// <summary>
    /// Deve preservar a origem da mercadoria e o crédito do Simples Nacional informados no segmento N10c.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarOrigemECreditoDoIcmsSn101()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("0000056689-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var icms = xml.SelectSingleNode("//*[local-name()='ICMSSN101']");

        Assert.NotNull(icms);
        Assert.True(icms.SelectSingleNode("*[local-name()='orig']") != null, xml.OuterXml);
        Assert.Equal("0", icms.SelectSingleNode("*[local-name()='orig']")?.InnerText);
        Assert.Equal("101", icms.SelectSingleNode("*[local-name()='CSOSN']")?.InnerText);
        Assert.Equal("3.9500", icms.SelectSingleNode("*[local-name()='pCredSN']")?.InnerText);
        Assert.Equal("17.78", icms.SelectSingleNode("*[local-name()='vCredICMSSN']")?.InnerText);

        var validacao = new ValidarSchema();
        validacao.Validar(xml, "NFe.nfe_v4.00.xsd", "http://www.portalfiscal.inf.br/nfe");
        Assert.False(validacao.Success);
        Assert.Contains("Signature", validacao.ErrorMessage);
    }

    /// <summary>
    /// Deve serializar emitente, destinatário e detalhe na sequência definida pelo schema em qualquer runtime.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarOrdemDeEmitenteDestinatarioEDetalhe()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("NFe_000049184_08_27_14-nfe.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        var emitente = xml.SelectSingleNode("//*[local-name()='emit']");
        var destinatario = xml.SelectSingleNode("//*[local-name()='dest']");
        var detalhe = xml.SelectSingleNode("//*[local-name()='det']");
        Assert.Equal("CNPJ,xNome,xFant,enderEmit,IE,IM,CNAE,CRT", NomesElementosFilhos(emitente));
        Assert.Equal("CNPJ,xNome,enderDest,indIEDest,email", NomesElementosFilhos(destinatario));
        Assert.Equal("prod,imposto,infAdProd,vItem", NomesElementosFilhos(detalhe));

        var validacao = new ValidarSchema();
        validacao.Validar(xml, "NFe.nfe_v4.00.xsd", "http://www.portalfiscal.inf.br/nfe");
        Assert.False(validacao.Success);
        Assert.Contains("Signature", validacao.ErrorMessage);
    }

    /// <summary>
    /// Deve interpretar o layout completo do produto sem deslocar os campos posteriores ao benefício fiscal.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarProdutoIpiEReformaDaNFe2320()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("002320_01_01_17_08_2026-nfe.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var produto = xml.SelectSingleNode("//*[local-name()='det']/*[local-name()='prod']");

        Assert.Equal("ABRACADEIRA ROSCA SEM FIM 51X64(200X212) INCA", produto.SelectSingleNode("*[local-name()='xProd']")?.InnerText);
        Assert.Equal("73269090", produto.SelectSingleNode("*[local-name()='NCM']")?.InnerText);
        Assert.Equal("1006200", produto.SelectSingleNode("*[local-name()='CEST']")?.InnerText);
        Assert.Equal("SP010830", produto.SelectSingleNode("*[local-name()='cBenef']")?.InnerText);
        Assert.Null(produto.SelectSingleNode("*[local-name()='EXTIPI']"));
        Assert.Equal("5124", produto.SelectSingleNode("*[local-name()='CFOP']")?.InnerText);
        Assert.Equal("500.0000", produto.SelectSingleNode("*[local-name()='qCom']")?.InnerText);
        Assert.Equal("8.7000", produto.SelectSingleNode("*[local-name()='vUnCom']")?.InnerText);
        Assert.Equal("53", xml.SelectSingleNode("//*[local-name()='IPI']//*[local-name()='CST']")?.InnerText);
        Assert.Equal("4219.50", xml.SelectSingleNode("//*[local-name()='IBSCBS']/*[local-name()='gIBSCBS']/*[local-name()='vBC']")?.InnerText);
        Assert.Equal("4392.18", xml.SelectSingleNode("//*[local-name()='det']/*[local-name()='vItem']")?.InnerText);
        Assert.Equal("4261.68", xml.SelectSingleNode("//*[local-name()='total']/*[local-name()='vNFTot']")?.InnerText);

        var validacao = new ValidarSchema();
        validacao.Validar(xml, "NFe.nfe_v4.00.xsd", "http://www.portalfiscal.inf.br/nfe");
        Assert.False(validacao.Success);
        Assert.Contains("Signature", validacao.ErrorMessage);
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
    /// Deve preservar modBC como zero e omitir os demais campos opcionais vazios do ICMSSN900.
    /// </summary>
    [Fact]
    public void ConverterDeveOmitirCamposVaziosDoIcmsSn900()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("08785-NFe.TXT"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        var icms = xml.SelectSingleNode("//*[local-name()='ICMSSN900']");
        Assert.NotNull(icms);
        Assert.Equal(3, icms.ChildNodes.Count);
        Assert.Equal("0", icms.SelectSingleNode("*[local-name()='orig']")?.InnerText);
        Assert.Equal("900", icms.SelectSingleNode("*[local-name()='CSOSN']")?.InnerText);
        Assert.Equal("0", icms.SelectSingleNode("*[local-name()='modBC']")?.InnerText);
    }

    /// <summary>
    /// Deve manter o grupo II zerado quando o item possui declaração de importação.
    /// </summary>
    [Fact]
    public void ConverterDeveManterImpostoImportacaoZeradoQuandoItemPossuiDi()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("31260803742159000170550020000003051000234068-NFE-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        var impostoImportacao = xml.SelectSingleNode("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='II']");
        Assert.NotNull(impostoImportacao);
        Assert.Equal(4, impostoImportacao.ChildNodes.Count);
        Assert.Equal("0.00", impostoImportacao.SelectSingleNode("*[local-name()='vBC']")?.InnerText);
        Assert.Equal("0.00", impostoImportacao.SelectSingleNode("*[local-name()='vDespAdu']")?.InnerText);
        Assert.Equal("0.00", impostoImportacao.SelectSingleNode("*[local-name()='vII']")?.InnerText);
        Assert.Equal("0.00", impostoImportacao.SelectSingleNode("*[local-name()='vIOF']")?.InnerText);
    }

    /// <summary>
    /// Deve omitir modBC zerado do ICMS51 como o conversor legado.
    /// </summary>
    [Fact]
    public void ConverterDeveOmitirModalidadeBaseCalculoZeradaDoIcms51()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("31260803742159000170550020000003051000234068-NFE-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        var icms51 = xml.SelectSingleNode("//*[local-name()='ICMS51']");
        Assert.NotNull(icms51);
        Assert.Equal(2, icms51.ChildNodes.Count);
        Assert.Null(icms51.SelectSingleNode("*[local-name()='modBC']"));
    }

    /// <summary>
    /// Deve limitar quantidades a quatro casas sem perder a precisão válida dos valores unitários.
    /// </summary>
    [Fact]
    public void ConverterDeveNormalizarCasasDecimaisDoProdutoComoLegado()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("31260803742159000170550020000003051000234068-NFE-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.Equal("22919.6000", xml.SelectSingleNode("//*[local-name()='prod']/*[local-name()='qCom']")?.InnerText);
        Assert.Equal("22919.6000", xml.SelectSingleNode("//*[local-name()='prod']/*[local-name()='qTrib']")?.InnerText);
        Assert.Equal("14.046314", xml.SelectSingleNode("//*[local-name()='prod']/*[local-name()='vUnCom']")?.InnerText);
        Assert.Equal("14.046314", xml.SelectSingleNode("//*[local-name()='prod']/*[local-name()='vUnTrib']")?.InnerText);
        Assert.Equal("321935.90", xml.SelectSingleNode("//*[local-name()='prod']/*[local-name()='vProd']")?.InnerText);
    }

    /// <summary>
    /// Deve manter completo o bloco principal do ICMS51 quando algum valor for informado.
    /// </summary>
    [Fact]
    public void ConverterDeveManterIcms51CompletoQuandoBaseForPositiva()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("31260803742159000170550020000003051000234068-NFE-orig-v5.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var icms51 = xml.SelectSingleNode("//*[local-name()='ICMS51']");

        Assert.NotNull(icms51);
        Assert.Equal(9, icms51.ChildNodes.Count);
        Assert.Null(icms51.SelectSingleNode("*[local-name()='modBC']"));
        Assert.Equal("0.0000", icms51.SelectSingleNode("*[local-name()='pRedBC']")?.InnerText);
        Assert.Equal("398422.66", icms51.SelectSingleNode("*[local-name()='vBC']")?.InnerText);
        Assert.Equal("0.0000", icms51.SelectSingleNode("*[local-name()='pICMS']")?.InnerText);
        Assert.Equal("0.00", icms51.SelectSingleNode("*[local-name()='vICMSOp']")?.InnerText);
        Assert.Equal("0.0000", icms51.SelectSingleNode("*[local-name()='pDif']")?.InnerText);
        Assert.Equal("0.00", icms51.SelectSingleNode("*[local-name()='vICMSDif']")?.InnerText);
        Assert.Equal("0.00", icms51.SelectSingleNode("*[local-name()='vICMS']")?.InnerText);
    }

    /// <summary>
    /// Deve vincular o DFe referenciado ao item correspondente.
    /// </summary>
    [Fact]
    public void ConverterDeveManterDfeReferenciadoEmTodosOsItens()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("nfe000077-NFE.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var referencias = xml.SelectNodes("//*[local-name()='det']/*[local-name()='DFeReferenciado']");

        Assert.Equal(3, referencias.Count);
        Assert.Equal("35260796597620000129550010001408741335108850", referencias[0].SelectSingleNode("*[local-name()='chaveAcesso']")?.InnerText);
        Assert.Equal("991", referencias[0].SelectSingleNode("*[local-name()='nItem']")?.InnerText);
        Assert.Equal("25", referencias[1].SelectSingleNode("*[local-name()='nItem']")?.InnerText);
        Assert.Equal("15", referencias[2].SelectSingleNode("*[local-name()='nItem']")?.InnerText);
    }

    /// <summary>
    /// Deve manter rastreabilidade, valor do item e responsável técnico.
    /// </summary>
    [Fact]
    public void ConverterDeveManterRastroValorItemEResponsavelTecnico()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("41260801182867000178550010001800011567804549-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var itens = xml.SelectNodes("//*[local-name()='det']");

        Assert.Equal(3, itens.Count);
        Assert.Equal("LOTE001", itens[0].SelectSingleNode("*[local-name()='prod']/*[local-name()='rastro']/*[local-name()='nLote']")?.InnerText);
        Assert.Equal("25.000", itens[0].SelectSingleNode("*[local-name()='prod']/*[local-name()='rastro']/*[local-name()='qLote']")?.InnerText);
        Assert.Equal("2026-08-05", itens[0].SelectSingleNode("*[local-name()='prod']/*[local-name()='rastro']/*[local-name()='dFab']")?.InnerText);
        Assert.Equal("2028-08-05", itens[0].SelectSingleNode("*[local-name()='prod']/*[local-name()='rastro']/*[local-name()='dVal']")?.InnerText);
        Assert.Equal("1351.25", itens[0].SelectSingleNode("*[local-name()='vItem']")?.InnerText);
        Assert.Equal("2064.00", itens[1].SelectSingleNode("*[local-name()='vItem']")?.InnerText);
        Assert.Equal("1074.75", itens[2].SelectSingleNode("*[local-name()='vItem']")?.InnerText);
        Assert.Equal("02", xml.SelectSingleNode("//*[local-name()='infRespTec']/*[local-name()='idCSRT']")?.InnerText);
        Assert.Equal("AAAAAAAAAAAAAAAAAAAAAAAAAAA=", xml.SelectSingleNode("//*[local-name()='infRespTec']/*[local-name()='hashCSRT']")?.InnerText);
    }

    /// <summary>
    /// Deve converter a NFC-e em contingência com chave calculada e impostos equivalentes ao legado.
    /// </summary>
    [Theory]
    [InlineData("060218_32336224000165_001_06_08_2026-nfe-orig.txt")]
    [InlineData("NT60860218.TXT")]
    public void ConverterDeveManterNfceEmContingenciaComChaveCalculada(string nomeArquivo)
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo(nomeArquivo));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var documento = Assert.Single(resultado.Documentos);
        Assert.Equal(44, documento.Chave.Length);
        var xml = new XmlDocument();
        xml.LoadXml(documento.Xml);

        Assert.Equal("65", xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='mod']")?.InnerText);
        Assert.Equal("9", xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='tpEmis']")?.InnerText);
        Assert.Equal("2026-08-06T10:08:59-03:00", xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='dhCont']")?.InnerText);
        Assert.Equal("PROBLEMA DE CONECTIVIDADE PARA TESTE", xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='xJust']")?.InnerText);
        Assert.Equal("25.41", xml.SelectSingleNode("//*[local-name()='ICMS00']/*[local-name()='vICMS']")?.InnerText);
        Assert.Equal("2.0000", xml.SelectSingleNode("//*[local-name()='ICMS00']/*[local-name()='pFCP']")?.InnerText);
        Assert.Equal("2.54", xml.SelectSingleNode("//*[local-name()='ICMS00']/*[local-name()='vFCP']")?.InnerText);
        Assert.Equal("2.10", xml.SelectSingleNode("//*[local-name()='PISAliq']/*[local-name()='vPIS']")?.InnerText);
        Assert.Equal("9.65", xml.SelectSingleNode("//*[local-name()='COFINSAliq']/*[local-name()='vCOFINS']")?.InnerText);
        Assert.Equal("127.03", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vNF']")?.InnerText);
    }

    /// <summary>
    /// Deve manter ICMSSN500 e Reforma Tributária quando os grupos possuem campos opcionais vazios.
    /// </summary>
    [Fact]
    public void ConverterDeveManterIcmsSn500EReformaComCamposVazios()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("27260821287558000170650010001143821778530846-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var documento = Assert.Single(resultado.Documentos);
        Assert.Equal("27260821287558000170650010001143821800677760", documento.Chave);
        var xml = new XmlDocument();
        xml.LoadXml(documento.Xml);

        var icms = xml.SelectSingleNode("//*[local-name()='ICMSSN500']");
        Assert.NotNull(icms);
        Assert.Equal(2, icms.ChildNodes.Count);
        Assert.Equal("500", icms.SelectSingleNode("*[local-name()='CSOSN']")?.InnerText);
        Assert.Equal("06", xml.SelectSingleNode("//*[local-name()='PISNT']/*[local-name()='CST']")?.InnerText);
        Assert.Equal("06", xml.SelectSingleNode("//*[local-name()='COFINSNT']/*[local-name()='CST']")?.InnerText);
        Assert.Equal("000", xml.SelectSingleNode("//*[local-name()='IBSCBS']/*[local-name()='CST']")?.InnerText);
        Assert.Equal("000001", xml.SelectSingleNode("//*[local-name()='IBSCBS']/*[local-name()='cClassTrib']")?.InnerText);
        Assert.Equal("9.00", xml.SelectSingleNode("//*[local-name()='gIBSCBS']/*[local-name()='vBC']")?.InnerText);
        Assert.Equal("0.1000", xml.SelectSingleNode("//*[local-name()='gIBSUF']/*[local-name()='pIBSUF']")?.InnerText);
        Assert.Equal("0.01", xml.SelectSingleNode("//*[local-name()='gIBSUF']/*[local-name()='vIBSUF']")?.InnerText);
        Assert.Equal("0.9000", xml.SelectSingleNode("//*[local-name()='gCBS']/*[local-name()='pCBS']")?.InnerText);
        Assert.Equal("0.08", xml.SelectSingleNode("//*[local-name()='gCBS']/*[local-name()='vCBS']")?.InnerText);
        Assert.Equal("9.00", xml.SelectSingleNode("//*[local-name()='det']/*[local-name()='vItem']")?.InnerText);
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
    /// Deve preservar a compatibilidade quando o ZD já contém o CSRT concatenado com a chave de acesso.
    /// </summary>
    [Fact]
    public void ConverterDeveGerarHashCsrtQuandoZdJaContemAChaveDeAcesso()
    {
        const string csrt = "CSRTTESTE0123456789012345678";
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("41260806225442000112550010002455051903698959-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var documento = Assert.Single(resultado.Documentos);
        var xml = new XmlDocument();
        xml.LoadXml(documento.Xml);

        var hashCsrt = xml.SelectSingleNode("//*[local-name()='infRespTec']/*[local-name()='hashCSRT']")?.InnerText;
        Assert.Equal(Unimake.Business.DFe.Utility.Converter.CalculateSHA1Hash(csrt + documento.Chave), hashCsrt);
        Assert.Equal(28, hashCsrt?.Length);
        Assert.NotNull(xml.SelectSingleNode("//*[local-name()='PISOutr']/*[local-name()='vBC']"));
        Assert.Null(xml.SelectSingleNode("//*[local-name()='PISOutr']/*[local-name()='qBCProd']"));
        Assert.NotNull(xml.SelectSingleNode("//*[local-name()='COFINSOutr']/*[local-name()='vBC']"));
        Assert.Null(xml.SelectSingleNode("//*[local-name()='COFINSOutr']/*[local-name()='qBCProd']"));
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
    /// Deve preservar pRedBC explicitamente zerado quando o ICMS51 contém valores de cálculo.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarReducaoZeradaDoIcms51ComValoresDeCalculo()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("58_78789542000182_4_8_2026-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        var grupos = xml.SelectNodes("//*[local-name()='ICMS51']");
        Assert.Equal(30, grupos.Count);
        foreach (XmlNode grupo in grupos)
        {
            Assert.Equal("0.0000", grupo.SelectSingleNode("*[local-name()='pRedBC']")?.InnerText);
        }
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
    /// Deve preservar a ordem do detalhe quando o fluxo Somente Validar normaliza a NFe pelo objeto oficial.
    /// </summary>
    [Fact]
    public void NormalizacaoDaNFeDeveManterInfAdProdDepoisDoImposto()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("41260801182867000178550010001800811409310317-nfe.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var nfe = XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(Assert.Single(resultado.Documentos).Xml);
        var xmlNormalizado = XMLUtility.Serializar(nfe);
        var detalhe = xmlNormalizado.SelectSingleNode("//*[local-name()='det']");
        var produto = detalhe.SelectSingleNode("*[local-name()='prod']");
        var imposto = detalhe.SelectSingleNode("*[local-name()='imposto']");
        var informacaoAdicional = detalhe.SelectSingleNode("*[local-name()='infAdProd']");

        Assert.NotNull(produto);
        Assert.NotNull(imposto);
        Assert.NotNull(informacaoAdicional);
        Assert.Same(produto, detalhe.FirstChild);
        Assert.Same(imposto, informacaoAdicional.PreviousSibling);
        Assert.Equal("INFORMACAO ADICIONAL DO ITEM PARA TESTE DE ORDENACAO", informacaoAdicional.InnerText);
    }

    /// <summary>
    /// Deve interpretar os campos de outras despesas e tributos totais exatamente nas posições do layout TXT.
    /// </summary>
    [Fact]
    public void ConverterDeveEvidenciarDivergenciaDeVOutroInformadaPeloErp()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("NFe_2998-nfe-orig-v2.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var itens = xml.SelectNodes("//*[local-name()='det']/*[local-name()='prod']/*[local-name()='vOutro']");

        Assert.Equal(2, itens.Count);
        Assert.Equal("0.65", itens[0].InnerText);
        Assert.Equal("0.64", itens[1].InnerText);
        Assert.Equal("0.00", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vOutro']")?.InnerText);
        Assert.Equal("654.40", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vNF']")?.InnerText);
        Assert.Equal("1.29", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vTotTrib']")?.InnerText);
    }

    /// <summary>
    /// Deve manter os totais coerentes quando o ERP informa vTotTrib no segmento M, sem usar vOutro do produto.
    /// </summary>
    [Fact]
    public void ConverterDeveAceitarBlocosCorrigidosDeTributosAproximados()
    {
        var linhas = File.ReadAllLines(CaminhoArquivo("NFe_2998-nfe-orig-v2.txt"));
        var valoresTributos = new[] { "0.65", "0.64" };
        var indiceItem = 0;

        for (var i = 0; i < linhas.Length; i++)
        {
            if (linhas[i].StartsWith("I|", StringComparison.Ordinal))
            {
                var campos = linhas[i].Split('|');
                campos[23] = "0.00";
                linhas[i] = string.Join("|", campos);
            }
            else if (linhas[i].StartsWith("M|", StringComparison.Ordinal))
            {
                linhas[i] = "M|" + valoresTributos[indiceItem++] + "|";
            }
        }

        var resultado = ConverterTemporario(linhas);

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var tributosItens = xml.SelectNodes("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='vTotTrib']");

        Assert.Equal(2, tributosItens.Count);
        Assert.Equal("0.65", tributosItens[0].InnerText);
        Assert.Equal("0.64", tributosItens[1].InnerText);
        Assert.Equal(0, xml.SelectNodes("//*[local-name()='det']/*[local-name()='prod']/*[local-name()='vOutro']").Count);
        Assert.Equal("0.00", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vOutro']")?.InnerText);
        Assert.Equal("654.40", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vNF']")?.InnerText);
        Assert.Equal("1.29", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vTotTrib']")?.InnerText);
    }

    /// <summary>
    /// Deve evidenciar o total de tributos aproximados informado sem os valores correspondentes nos itens.
    /// </summary>
    [Fact]
    public void ConverterDeveEvidenciarVtotTribTotalSemValoresNosItens()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("NFe_2999-nfe-orig-v2.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.Equal(0, xml.SelectNodes("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='vTotTrib']").Count);
        Assert.Equal("2.80", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vTotTrib']")?.InnerText);
    }

    /// <summary>
    /// Deve aceitar a distribuição do vTotTrib entre os itens quando a soma corresponde ao total da NFe.
    /// </summary>
    [Fact]
    public void ConverterDeveAceitarVtotTribDistribuidoNosItens()
    {
        var linhas = File.ReadAllLines(CaminhoArquivo("NFe_2999-nfe-orig-v2.txt"));
        var valoresTributos = new[] { "2.05", "0.75" };
        var indiceItem = 0;

        for (var i = 0; i < linhas.Length; i++)
        {
            if (linhas[i].StartsWith("M|", StringComparison.Ordinal))
            {
                linhas[i] = "M|" + valoresTributos[indiceItem++] + "|";
            }
        }

        var resultado = ConverterTemporario(linhas);

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var tributosItens = xml.SelectNodes("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='vTotTrib']");

        Assert.Equal(2, tributosItens.Count);
        Assert.Equal("2.05", tributosItens[0].InnerText);
        Assert.Equal("0.75", tributosItens[1].InnerText);
        Assert.Equal("2.80", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vTotTrib']")?.InnerText);
    }

    /// <summary>
    /// Deve preservar referência, tributos aproximados, total e pagamento da NFC-e de devolução.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarTotaisDaNfceDevolucao23655()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("000023655_11092080000179_001_11_08_2026-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.Equal("65", xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='mod']")?.InnerText);
        Assert.Equal("35260899999999000191550010000000011000000017", xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='NFref']/*[local-name()='refNFe']")?.InnerText);
        Assert.Equal("27.58", xml.SelectSingleNode("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='vTotTrib']")?.InnerText);
        Assert.Equal("85.00", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vProd']")?.InnerText);
        Assert.Equal("85.00", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vNF']")?.InnerText);
        Assert.Equal("27.58", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vTotTrib']")?.InnerText);
        Assert.Equal("20", xml.SelectSingleNode("//*[local-name()='detPag']/*[local-name()='tPag']")?.InnerText);
        Assert.Equal("85.00", xml.SelectSingleNode("//*[local-name()='detPag']/*[local-name()='vPag']")?.InnerText);
    }

    /// <summary>
    /// Deve preservar cobrança, múltiplos pagamentos e Reforma Tributária da NFe 398.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarCobrancaPagamentosEReformaDaNfe398()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("398_15528301000160_1_11_08_2026-NFE-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var pagamentos = xml.SelectNodes("//*[local-name()='detPag']");

        Assert.Equal(2, xml.SelectNodes("//*[local-name()='dup']").Count);
        Assert.Equal(2, pagamentos.Count);
        Assert.Equal("01", pagamentos[0].SelectSingleNode("*[local-name()='tPag']")?.InnerText);
        Assert.Equal("50.00", pagamentos[0].SelectSingleNode("*[local-name()='vPag']")?.InnerText);
        Assert.Equal("99", pagamentos[1].SelectSingleNode("*[local-name()='tPag']")?.InnerText);
        Assert.Equal("NAO INFORMADO", pagamentos[1].SelectSingleNode("*[local-name()='xPag']")?.InnerText);
        Assert.Equal("50.00", pagamentos[1].SelectSingleNode("*[local-name()='vPag']")?.InnerText);
        Assert.Equal("30.96", xml.SelectSingleNode("//*[local-name()='det']/*[local-name()='imposto']/*[local-name()='vTotTrib']")?.InnerText);
        Assert.Equal("0.10", xml.SelectSingleNode("//*[local-name()='gIBSUF']/*[local-name()='vIBSUF']")?.InnerText);
        Assert.Equal("0.90", xml.SelectSingleNode("//*[local-name()='gCBS']/*[local-name()='vCBS']")?.InnerText);
        Assert.Equal("100.00", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vNF']")?.InnerText);
    }

    /// <summary>
    /// Deve preservar IPI, item fora do total e Reforma Tributária da NFe 399.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarIpiEItemForaDoTotalDaNfe399()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("399_15528301000160_1_11_08_2026-NFE-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var ipiTrib = xml.SelectSingleNode("//*[local-name()='IPITrib']");

        Assert.Equal("0", xml.SelectSingleNode("//*[local-name()='prod']/*[local-name()='indTot']")?.InnerText);
        Assert.Equal("50", ipiTrib.SelectSingleNode("*[local-name()='CST']")?.InnerText);
        Assert.Equal("0.00", ipiTrib.SelectSingleNode("*[local-name()='vBC']")?.InnerText);
        Assert.Equal("0.0000", ipiTrib.SelectSingleNode("*[local-name()='pIPI']")?.InnerText);
        Assert.Equal("5.00", ipiTrib.SelectSingleNode("*[local-name()='vIPI']")?.InnerText);
        Assert.Equal("0.01", xml.SelectSingleNode("//*[local-name()='gIBSUF']/*[local-name()='vIBSUF']")?.InnerText);
        Assert.Equal("0.05", xml.SelectSingleNode("//*[local-name()='gCBS']/*[local-name()='vCBS']")?.InnerText);
        Assert.Equal("5.00", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vIPI']")?.InnerText);
        Assert.Equal("5.00", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vNF']")?.InnerText);
        Assert.Equal("90", xml.SelectSingleNode("//*[local-name()='detPag']/*[local-name()='tPag']")?.InnerText);
    }

    /// <summary>
    /// Deve demonstrar que IPI sem ICMS conduz ao ramo de ISSQN do schema e aceitar o bloco de ICMS do Simples Nacional.
    /// </summary>
    [Fact]
    public void Nfe399DeveValidarSchemaQuandoInformarIcmsDoSimplesNacional()
    {
        const string namespaceNFe = "http://www.portalfiscal.inf.br/nfe";
        var conversaoOriginal = new NFeTxtConverter().Converter(CaminhoArquivo("399_15528301000160_1_11_08_2026-NFE-orig.txt"));
        Assert.True(conversaoOriginal.Sucesso, conversaoOriginal.MensagemErro);
        var xmlOriginal = new XmlDocument();
        xmlOriginal.LoadXml(Assert.Single(conversaoOriginal.Documentos).Xml);

        Assert.Null(xmlOriginal.SelectSingleNode("//*[local-name()='imposto']/*[local-name()='ICMS']"));
        Assert.NotNull(xmlOriginal.SelectSingleNode("//*[local-name()='imposto']/*[local-name()='IPI']"));
        Assert.NotNull(xmlOriginal.SelectSingleNode("//*[local-name()='imposto']/*[local-name()='PIS']"));
        var validacaoOriginal = new ValidarSchema();
        validacaoOriginal.Validar(xmlOriginal, "NFe.nfe_v4.00.xsd", namespaceNFe);
        Assert.False(validacaoOriginal.Success);
        Assert.Contains("PIS", validacaoOriginal.ErrorMessage);
        Assert.Contains("ISSQN", validacaoOriginal.ErrorMessage);

        var linhas = File.ReadAllLines(CaminhoArquivo("399_15528301000160_1_11_08_2026-NFE-orig.txt"));
        var indiceIpi = Array.FindIndex(linhas, linha => linha.StartsWith("O|", StringComparison.Ordinal));
        var linhasCorrigidas = new string[linhas.Length + 2];
        Array.Copy(linhas, 0, linhasCorrigidas, 0, indiceIpi);
        linhasCorrigidas[indiceIpi] = "N|";
        linhasCorrigidas[indiceIpi + 1] = "N10d|0|400|";
        Array.Copy(linhas, indiceIpi, linhasCorrigidas, indiceIpi + 2, linhas.Length - indiceIpi);

        var conversaoCorrigida = ConverterTemporario(linhasCorrigidas);
        Assert.True(conversaoCorrigida.Sucesso, conversaoCorrigida.MensagemErro);
        var xmlCorrigido = new XmlDocument();
        xmlCorrigido.LoadXml(Assert.Single(conversaoCorrigida.Documentos).Xml);

        Assert.Equal("400", xmlCorrigido.SelectSingleNode("//*[local-name()='ICMSSN102']/*[local-name()='CSOSN']")?.InnerText);
        var validacaoCorrigida = new ValidarSchema();
        validacaoCorrigida.Validar(xmlCorrigido, "NFe.nfe_v4.00.xsd", namespaceNFe);
        Assert.False(validacaoCorrigida.Success);
        Assert.DoesNotContain("elemento filho 'PIS'", validacaoCorrigida.ErrorMessage);
        Assert.DoesNotContain("child element 'PIS'", validacaoCorrigida.ErrorMessage);
        Assert.Contains("Signature", validacaoCorrigida.ErrorMessage);
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
    [InlineData("NFe_Reforma_Tributaria-nfe.txt", "44db7065458c6eb5130e5bdfb12c06ea1197de342fc6385eed8b2f027ff8e250")]
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
    /// Deve preservar a NFC-e em contingência com CSOSN 102 e tributos federais sem valores de cálculo.
    /// </summary>
    [Fact]
    public void ConverterDevePreservarNfceEmContingenciaComTributosSemValores()
    {
        var resultado = new NFeTxtConverter().Converter(CaminhoArquivo("000001_01_01_05_08_2026-nfe-orig.txt"));

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);

        Assert.Equal("65", xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='mod']")?.InnerText);
        Assert.Equal("6", xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='tpEmis']")?.InnerText);
        Assert.Equal("2026-08-05T13:00:00-03:00", xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='dhCont']")?.InnerText);
        Assert.Equal("SEFAZ SP FORA DO AR", xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='xJust']")?.InnerText);
        Assert.Equal("102", xml.SelectSingleNode("//*[local-name()='ICMSSN102']/*[local-name()='CSOSN']")?.InnerText);
        Assert.Equal("99", xml.SelectSingleNode("//*[local-name()='PISOutr']/*[local-name()='CST']")?.InnerText);
        Assert.Equal("99", xml.SelectSingleNode("//*[local-name()='COFINSOutr']/*[local-name()='CST']")?.InnerText);
        Assert.Equal("232.30", xml.SelectSingleNode("//*[local-name()='det']/*[local-name()='vItem']")?.InnerText);
        Assert.Equal("232.30", xml.SelectSingleNode("//*[local-name()='total']/*[local-name()='vNFTot']")?.InnerText);
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

        Assert.Equal(3, xml.SelectNodes("//*[local-name()='gPagAntecipado']/*[local-name()='refNFe']").Count);
        Assert.Null(xml.SelectSingleNode("//*[local-name()='gPagAntecipado']/*[local-name()='refDFe']"));
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
            "AGILLE COMERCIO DE MEDICAMENTOS LTDA",
            "OON ONCOLOGIA, ORTOPEDIA E NEUROLOGIA VET LTDA",
            "cmanhaesvet@gmail.com",
            "AV DAS AGUIAS",
            "RUA FELIPE NEVES",
            "AVENIDA ATLANTICA N 720",
            "nfe@agillemed.com.br",
            "OXI GENESES COM.GASES EQUIPAMENTOS LTDA EPP",
            "METACAULIM BRASIL INDUSTRIA COMERCIO LTDA",
            "RUA  AGOSTINHO BALESTRIN",
            "AV.HUMBERTO CERESER",
            "vendas@metacaulim.com.br",
            "LOTUS CENTRAL DE DIST DE HIGIENICOS LTDA",
            "TEXTIL BICOLOR INDUSTRIA E COM DE CONFEC",
            "R DR JOAO ALTES DE LIMA",
            "VENDEDOR: VIVIANE",
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
            "SOC.COM.MAT.P/CONSTR.LUIZ LOPES LTDA",
            "108680702113",
            "RUA MAJOR OTAVIANO",
            "ROD. RAPOSO TAVARES KM-18 5",
            "100441666118",
            "60561719000557",
            "COMERCIO DE PRODUTOS AGROVETERINARIOS LTDA",
            "CASA DO FAZENDEIRO",
            "00131341545",
            "36912368000173",
            "AV MATO GROSSO 201",
            "6634381569",
            "JOSE ADELMO DE JESUS",
            "45184984100",
            "RUA JOSE ANDRE VAJAO",
            "VOL IMPORTS - MG",
            "0032376020050",
            "30999720000173",
            "AV DOUTOR ROFLES CECILIO",
            "3432124039",
            "MILLS PESADOS LOCACAO SERVICOS E LOGISTICA SA",
            "671666958115",
            "gestaonotas.pesados@mills.com.br",
            "01633840003099",
            "R FIORAVANTE MANCINO",
            "1154306482",
            "CLIENTE RETIRA",
            "DHIEFFERSON FELIPE RENDE SANTOS",
            "5500021454",
            "SN/013244",
            "FROTA 1417",
            "M. L. SCHWERTNER PLANTAS",
            "MLS PLANTAS",
            "1280058657",
            "28508340000147",
            "PRIMEIRO DE MAIO",
            "51997012925",
            "PAULO JAIR HOLDEFER",
            "05579449020",
            "2131010863",
            "R BOA VISTA",
            "5135624755",
            "JASMIM PLANTAS ORNAMENTAIS",
            "00001280019163",
            "R RS 122 KM 09",
            "87215802000105",
            "68711275",
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
            "CIARIN COMERCIO E INDUSTRIA DE ARTIGOS P/ SELARIA LTDA",
            "CIARIN METAIS",
            "AGROPECUARIA GALPAO DO BOIADEIRO LTDA EPP",
            "SUDOESTE TRANSPORTES LTDA",
            "RUA EZIDIO BALLADELLI",
            "RUA SALDANHA MARINHO",
            "RUA ALMERINDA SILVEIRA COELHO",
            "8330316005",
            "9012364374",
            "01468972000178",
            "02343801000851",
            "4433513934",
            "236235023",
            "devolucoes@leinertex.com.br",
            "AV ANAPOLIS",
            "AV JATAI",
            "VILA CONCORDIA",
            "PQ IND AP VICE P JOSE ALENCAR",
            "102575584",
            "103120939",
            "6232081448",
            "6232750800",
            "420396)",
            "ALTO DA BOA VISTA MATERIAS DE CONSTRUCAO LTDA",
            "CENTERKASA",
            "RUA JACINTO RAMOS",
            "6235133655",
            "58033)",
            "TINTA LEINERTEX ACR FOSCA 18L AREIA",
            "7898360090686",
            "08561701000101",
            "082853",
            "739532",
            "430893",
            "526811"
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

    private static string NomesElementosFilhos(XmlNode elemento)
    {
        var nomes = new StringBuilder();
        foreach (XmlNode filho in elemento.ChildNodes)
        {
            if (filho.NodeType != XmlNodeType.Element)
            {
                continue;
            }
            if (nomes.Length > 0)
            {
                nomes.Append(',');
            }
            nomes.Append(filho.LocalName);
        }
        return nomes.ToString();
    }

    private static void ValidarReferenciaProdutorItensEPagamentosDaNfe892(XmlDocument xml)
    {
        var referencia = xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='NFref']/*[local-name()='refNFP']");
        Assert.NotNull(referencia);
        Assert.Null(referencia.SelectSingleNode("*[local-name()='CNPJ']"));
        Assert.Equal("11144477735", referencia.SelectSingleNode("*[local-name()='CPF']")?.InnerText);
        Assert.Equal("1234567890", referencia.SelectSingleNode("*[local-name()='IE']")?.InnerText);
        Assert.Equal("04", referencia.SelectSingleNode("*[local-name()='mod']")?.InnerText);
        Assert.Equal("890", referencia.SelectSingleNode("*[local-name()='serie']")?.InnerText);
        Assert.Equal("1", referencia.SelectSingleNode("*[local-name()='nNF']")?.InnerText);

        Assert.Equal(6, xml.SelectNodes("//*[local-name()='det']").Count);
        Assert.Equal(6, xml.SelectNodes("//*[local-name()='ICMSSN102']").Count);
        Assert.Equal(6, xml.SelectNodes("//*[local-name()='ICMSSN102']/*[local-name()='orig' and text()='0']").Count);
        Assert.Equal(6, xml.SelectNodes("//*[local-name()='ICMSSN102']/*[local-name()='CSOSN' and text()='102']").Count);
        Assert.Equal(0, xml.SelectNodes("//*[local-name()='IPI']/*[local-name()='CNPJProd']").Count);
        Assert.Equal(6, xml.SelectNodes("//*[local-name()='prod']/*[local-name()='indEscala' and text()='S']").Count);
        Assert.Equal("4700.00", xml.SelectSingleNode("//*[local-name()='ICMSTot']/*[local-name()='vNF']")?.InnerText);
        Assert.Equal(0, xml.SelectNodes("//*[local-name()='cobr']/*[local-name()='fat']").Count);

        var pagamentos = xml.SelectNodes("//*[local-name()='pag']/*[local-name()='detPag']");
        Assert.Equal(1, pagamentos.Count);
        Assert.Equal(1, xml.SelectNodes("//*[local-name()='detPag']/*[local-name()='tPag' and text()='90']").Count);
        Assert.Equal(1, xml.SelectNodes("//*[local-name()='detPag']/*[local-name()='vPag' and text()='0.00']").Count);
        Assert.Equal(0, xml.SelectNodes("//*[local-name()='detPag']/*[local-name()='indPag']").Count);
        Assert.Equal(0, xml.SelectNodes("//*[local-name()='detPag']/*[local-name()='xPag']").Count);
    }

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
