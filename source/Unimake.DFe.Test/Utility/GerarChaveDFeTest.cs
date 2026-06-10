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
        [Fact]
        [Trait("Utility", "ChaveDFe")]
        public void TipoCNPJ_Numerico_RetornaN()
        {
            Assert.Equal("N", XMLUtility.TipoCNPJ("12345678000195"));
        }

        [Fact]
        [Trait("Utility", "ChaveDFe")]
        public void TipoCNPJ_Alfanumerico_RetornaA()
        {
            Assert.Equal("A", XMLUtility.TipoCNPJ("12ABC34501DE35"));
        }

        [Fact]
        [Trait("Utility", "ChaveDFe")]
        public void TipoCNPJ_ComMascaraAlfa_RetornaA()
        {
            Assert.Equal("A", XMLUtility.TipoCNPJ("12.ABC.345/01DE-35"));
        }

        [Fact]
        [Trait("Utility", "ChaveDFe")]
        public void TipoCNPJ_ComUnderscore_RetornaI()
        {
            Assert.Equal("I", XMLUtility.TipoCNPJ("12ABC34501D_35"));
        }

        [Fact]
        [Trait("Utility", "ChaveDFe")]
        public void CalcularDVChave_CNPJNumerico_MantemResultadoAtual()
        {
            var chaveSemDV = "4126061234567800019555001000000123187654321";

            Assert.Equal(4, XMLUtility.CalcularDVChave(chaveSemDV));
            Assert.Equal("41260612345678000195550010000001231876543214", chaveSemDV + XMLUtility.CalcularDVChave(chaveSemDV));
        }

        [Fact]
        [Trait("Utility", "ChaveDFe")]
        public void CalcularDVChave_CNPJAlfanumerico_CalculaDV()
        {
            var chaveSemDV = "41260612ABC34501DE3555001000000123187654321";

            Assert.Equal(4, XMLUtility.CalcularDVChave(chaveSemDV));
            Assert.Equal("41260612ABC34501DE35550010000001231876543214", chaveSemDV + XMLUtility.CalcularDVChave(chaveSemDV));
        }

        [Fact]
        [Trait("Utility", "ChaveDFe")]
        public void CalcularDVChave_ComPrefixoNFeAlfa_CalculaDV()
        {
            Assert.Equal(4, XMLUtility.CalcularDVChave("NFe41260612ABC34501DE3555001000000123187654321"));
        }

        [Fact]
        [Trait("Utility", "ChaveDFe")]
        public void MontarChaveNFe_CNPJNumerico_GeraChaveAtual()
        {
            var conteudo = CriarConteudoChaveNFe("12345678000195");

            var chave = XMLUtility.MontarChaveNFe(ref conteudo);

            Assert.Equal("41260612345678000195550010000001231876543214", chave);
        }

        [Fact]
        [Trait("Utility", "ChaveDFe")]
        public void MontarChaveNFe_CNPJAlfanumerico_GeraChaveAlfa()
        {
            var conteudo = CriarConteudoChaveNFe("12ABC34501DE35");

            var chave = XMLUtility.MontarChaveNFe(ref conteudo);

            Assert.Equal("41260612ABC34501DE35550010000001231876543214", chave);
        }

        [Fact]
        [Trait("Utility", "ChaveDFe")]
        public void ChecarChaveDFe_CNPJAlfanumerico_ValidaSemErro()
        {
            XMLUtility.ChecarChaveDFe("41260612ABC34501DE35550010000001231876543214");
        }

        [Fact]
        [Trait("Utility", "ChaveDFe")]
        public void ChecarChaveDFe_CNPJAlfanumericoDVInvalido_LancaErro()
        {
            var exception = Assert.Throws<Exception>(() => XMLUtility.ChecarChaveDFe("41260612ABC34501DE35550010000001231876543215"));

            Assert.Contains("Dígito verificador", exception.Message);
        }

        [Fact]
        [Trait("Utility", "ChaveDFe")]
        public void ExtrairConteudoChaveDFe_CNPJAlfanumerico_RetornaCNPJAlfa()
        {
            var conteudo = XMLUtility.ExtrairConteudoChaveDFe("41260612ABC34501DE35550010000001231876543214");

            Assert.Equal("12ABC34501DE35", conteudo.CNPJCPFEmissor);
            Assert.Equal(ModeloDFe.NFe, conteudo.Modelo);
            Assert.Equal(1, conteudo.Serie);
            Assert.Equal(123, conteudo.NumeroDoctoFiscal);
            Assert.Equal("87654321", conteudo.CodigoNumerico);
            Assert.Equal(4, conteudo.DigitoVerificador);
        }

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

        private static XMLUtility.ConteudoChaveDFe CriarConteudoChaveNFe(string cnpjcpfEmissor)
        {
            return new XMLUtility.ConteudoChaveDFe
            {
                UFEmissor = UFBrasil.PR,
                AnoEmissao = "26",
                MesEmissao = "06",
                CNPJCPFEmissor = cnpjcpfEmissor,
                Modelo = ModeloDFe.NFe,
                Serie = 1,
                NumeroDoctoFiscal = 123,
                TipoEmissao = TipoEmissao.Normal,
                CodigoNumerico = "87654321"
            };
        }
    }
}
