using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CTeOS;
using Unimake.Business.DFe.Xml.CTeOS;
using Xunit;

namespace Unimake.DFe.Test.CTeOS
{
    /// <summary>
    /// Testar o serviço de consulta cadastro da CTeOS
    /// </summary>
    public class ConsultaCadastroTest
    {
        

        [Theory]
        [Trait("DFe", "CTeOS")]
        [InlineData(UFBrasil.PR, "06117473000150")]
        public void ConsultaCadastroConstrutor(UFBrasil ufBrasil, string cnpj)
        {

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTeOS,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var consultaCadastro = new ConsultaCadastro(ufBrasil, cnpj, configuracao);
            consultaCadastro.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(consultaCadastro.Result.InfCons.CUF.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(consultaCadastro.Result.InfCons.CStat != 259, "CNPJ consultado não é foi localizado no webservice da UF " + ufBrasil.ToString() + ".");
        }


    }


}