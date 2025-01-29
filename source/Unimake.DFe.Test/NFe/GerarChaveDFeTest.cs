using System;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.NFe
{
    /// <summary>
    /// Testa 
    /// </summary>
    public class GerarChaveDFeTest
    {
        /// <summary>
        /// Testar o método que monta a chave da NFe
        /// </summary>
        [Fact]
        [Trait("DFe", "NFe")]
        public void MontarChaveNFe()
        {
            var cUF = UFBrasil.PR;
            var dhEmi = DateTime.Now;
            var cnpjOrCpf = "06117473000150";
            var mod = ModeloDFe.NFe;
            var serie = 1;
            var nNF = 123;
            var tpEmis = TipoEmissao.Normal;
            var cNF = "12345678";

            var chaveNFe = Unimake.Business.DFe.Utility.XMLUtility.MontarChaveDFe(cUF, dhEmi, cnpjOrCpf, mod, serie, nNF, tpEmis, cNF);

            var chaveCorreta = "41250106117473000150550010000001231123456786";

            Assert.True(chaveNFe.Equals(chaveCorreta), "Chave da NFe gerada está diferente! Chave gerada: " + chaveNFe + "Como deveria ser a chave: " + chaveCorreta);
        }
    }
}