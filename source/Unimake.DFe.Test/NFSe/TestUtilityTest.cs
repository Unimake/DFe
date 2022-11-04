using System;
using System.Collections.Generic;
using Diag = System.Diagnostics;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe
{
    /// <summary>
    /// Testar classe e método que monta o cenário de testes da NFSe
    /// </summary>
    public class TesteUtilityTest
    {
        /// <summary>
        /// Testar classe e método que monta o cenário de testes da NFSe
        /// </summary>
        [Fact]
        [Trait("DFe", "NFSe")]
        public void TestarElaboracaoCenario()
        {
            var lista = TestUtility.PreparaDadosCenario("GerarNfse");
        }
    }
}