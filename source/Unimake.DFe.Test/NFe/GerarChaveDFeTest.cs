using System;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test;

/// <summary>
/// Testa a geração da chave dos DFe´s
/// </summary>
public class GerarChaveDFeTest
{
    /// <summary>
    /// Testar gerar a chave da NFe com CNPJ Alfanumérico e numérico
    /// </summary>
    /// <param name="cnpj"></param>
    [Theory]
    [Trait("DFe", "NFe")]
    [InlineData("ABC123DEF45612", "412603ABC123DEF45612550200000001111135489250", ModeloDFe.NFe, TipoEmissao.Normal)]
    [InlineData("06117473000150", "41260306117473000150550200000001111135489253", ModeloDFe.NFe, TipoEmissao.Normal)]
    [InlineData("67157681021", "41260300067157681021550200000001111135489251", ModeloDFe.NFe, TipoEmissao.Normal)] 
    [InlineData("06117473000150", "41260306117473000150550200000001117135489252", ModeloDFe.NFe, TipoEmissao.ContingenciaSVCRS)] 
    public void MontarChaveDFeTest(string cnpj, string chaveCorreta, ModeloDFe modelo, TipoEmissao tipoEmissao)
    {
        var cUF = UFBrasil.PR;
        var dhEmi = DateTime.Parse("2026-03-17");
        var cnpjOrCpf = cnpj;
        var mod = modelo;
        var serie = 20;
        var nNF = 111;
        var tpEmis = tipoEmissao;
        var cNF = "13548925";

        var chaveNFe = Business.DFe.Utility.XMLUtility.MontarChaveDFe(cUF, dhEmi, cnpjOrCpf, mod, serie, nNF, tpEmis, cNF);

        Assert.True(chaveNFe.Equals(chaveCorreta), "Chave da NFe gerada está diferente! [Chave gerada: " + chaveNFe + "] [Como deveria ser a chave: " + chaveCorreta + "]");
    }
}