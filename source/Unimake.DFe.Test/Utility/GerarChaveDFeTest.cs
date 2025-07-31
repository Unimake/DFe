using System;
using System.Text.RegularExpressions;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.Utility
{
    /// <summary>
    /// Realizar testes com o gerador de chaves de DFe
    /// </summary>
    public class GerarChaveDFeTest
    {
        [Theory]
        [Trait("Utility", "ChaveDFe")]

        //CNPJ Alfanumérico - Matriz
        [InlineData("I3.AAD.IU2/0001-58", ModeloDFe.NFCe, 10, "00000001", 0)]
        [InlineData("RO.TG2.OQ2/0001-91", ModeloDFe.NFe, 22, "00000002", 7)]
        [InlineData("M5.65E.PLC/0001-74", ModeloDFe.NFe, 333, "00000003", 3)]

        //CNPJ Alfanumérico - Matriz e suas filiais
        [InlineData("15.UQ7.3HD/0001-49", ModeloDFe.NFe, 3444, "00000004", 8)]
        [InlineData("15.UQ7.3HD/0002-20", ModeloDFe.NFCe, 355, "00000005", 8)]
        [InlineData("15UQ73HD000300", ModeloDFe.NFCe, 366, "00000005", 2)]

        //CNPJ Numérico - Matriz
        [InlineData("42.275.886/0001-26", ModeloDFe.NFCe, 83, "00000008", 8)]
        [InlineData("86.480.621/0001-35", ModeloDFe.NFe, 773, "00000009", 2)]
        [InlineData("18188514000188", ModeloDFe.NFe, 9993, "00000010", 9)]

        //CNPJ Numérico - Matriz e suas filiais
        [InlineData("66.680.026/0001-36", ModeloDFe.NFe, 123, "00000011", 2)]
        [InlineData("66.680.026/0002-17", ModeloDFe.NFe, 1231233, "00000012", 8)]
        [InlineData("66.680.026/0003-06", ModeloDFe.NFCe, 1233, "90000013", 4)]

        //CPF
        [InlineData("81293612057", ModeloDFe.NFCe, 441, "00000014", 2)]
        [InlineData("596.552.450-12", ModeloDFe.NFe, 551, "10000015", 4)]
        [InlineData("372.841.240-67", ModeloDFe.NFe, 661, "80000016", 1)]
        public void TestGerarChaveDFe(string cnpj, ModeloDFe mod, int nNF, string cNF, int digitoCorreto)
        {
            var dhEmi = new DateTime(2025, 07, 31, 15, 30, 23);
            var cnpjLimpo = Regex.Replace(cnpj, @"[^\w]", "").PadLeft(14, '0');

            var conteudoChaveDFe = new XMLUtility.ConteudoChaveDFe
            {
                UFEmissor = UFBrasil.PR,
                AnoEmissao = dhEmi.ToString("yy"),
                MesEmissao = dhEmi.ToString("MM"),
                CNPJCPFEmissor = cnpjLimpo,
                Modelo = mod,
                Serie = 1,
                NumeroDoctoFiscal = nNF,
                TipoEmissao = TipoEmissao.Normal,
                CodigoNumerico = cNF
            };

            var chave = XMLUtility.MontarChaveNFe(ref conteudoChaveDFe);
            var cDV = conteudoChaveDFe.DigitoVerificador;

            Assert.True(cDV == digitoCorreto, $"O dígito verificador da chave '{chave}' deveria ser {digitoCorreto} mas foi {cDV}.");
            Assert.True(chave.Substring(6, 14) == cnpjLimpo, $"CNPJ/CPF composto na chave {chave.Substring(6, 14)} está diferente de {cnpjLimpo}.");
        }
    }
}
