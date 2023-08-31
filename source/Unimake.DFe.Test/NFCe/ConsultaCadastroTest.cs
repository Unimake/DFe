using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFCe;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFCe
{
    /// <summary>
    /// Testar o serviço de consulta cadastro da NFCe
    /// </summary>
    public class ConsultaCadastroTest
    {
        /// <summary>
        /// Consulta um CNPJ em cada estado somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua uma consulta por estado + ambiente e um CNPJ por estado para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a consulta</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a consulta</param>
        [Theory]
        [Trait("DFe", "NFCe")]
        [InlineData(UFBrasil.AC, "30805423000140")]
        //        [InlineData(UFBrasil.AL)]
        //        [InlineData(UFBrasil.AP)]
        //        [InlineData(UFBrasil.AM, "17965457000133")]
        [InlineData(UFBrasil.BA, "14365765000130")]
        [InlineData(UFBrasil.CE, "22809241000208")]
        //        [InlineData(UFBrasil.DF, "03185564000134")]
        [InlineData(UFBrasil.ES, "00205696000354")]
        [InlineData(UFBrasil.GO, "36856904000160")]
        //        [InlineData(UFBrasil.MA)]
        [InlineData(UFBrasil.MT, "18903380000130")]
        [InlineData(UFBrasil.MS, "10656587000145")]
        [InlineData(UFBrasil.MG, "25631151000179")]
        //        [InlineData(UFBrasil.PA)]
        [InlineData(UFBrasil.PB, "35437276000116")]
        [InlineData(UFBrasil.PR, "06117473000150")]
        [InlineData(UFBrasil.PE, "24028713000121")]
        //        [InlineData(UFBrasil.PI)]
        //        [InlineData(UFBrasil.RJ)]
        [InlineData(UFBrasil.RN, "10723930000127")]
        [InlineData(UFBrasil.RS, "91417329000108")]
        //        [InlineData(UFBrasil.RO)]
        //        [InlineData(UFBrasil.RR)]
        [InlineData(UFBrasil.SC, "35075869000180")]
        [InlineData(UFBrasil.SP, "06877949000150")]
        //        [InlineData(UFBrasil.SE)]
        //        [InlineData(UFBrasil.TO)]
        public void ConsultarCadastroContribuinteNFCe(UFBrasil ufBrasil, string cnpj)
        {
            var xml = new ConsCad
            {
                Versao = "2.00",
                InfCons = new InfCons()
                {
                    CNPJ = cnpj,
                    UF = ufBrasil
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var consultaCadastro = new ConsultaCadastro(xml, configuracao);
            consultaCadastro.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(consultaCadastro.Result.InfCons.CUF.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(consultaCadastro.Result.InfCons.CStat != 259, "CNPJ consultado não é foi localizado no webservice da UF " + ufBrasil.ToString() + ".");

            //Assert.True(consultaCadastro.Result.InfCons.CNPJ.Equals(xml.InfCons.CNPJ), "Webservice retornou uma chave da NFCe diferente da enviada na consulta.");
        }
    }
}