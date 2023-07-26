using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFe;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe
{
    /// <summary>
    /// Testar o serviço de Inutilização da NFe
    /// </summary>
    public class InutilizacaoTest
    {
        /// <summary>
        /// Tenta inutilizar números de NFe somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua uma inutilização por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a inutilizacao</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a inutilizacao</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(UFBrasil.AC, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AL, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AP, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AM, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.BA, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.CE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.DF, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.ES, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.GO, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MA, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Homologacao, "", "02111233456")]
        [InlineData(UFBrasil.MS, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MG, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PA, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PB, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PI, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RN, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RS, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RO, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RR, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.SC, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.SP, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.SE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.TO, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AC, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.AL, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.AP, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.AM, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.BA, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.CE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.DF, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.ES, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.GO, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MA, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Producao, "", "02111233456")]
        [InlineData(UFBrasil.MS, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MG, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PA, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PB, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PI, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RN, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RS, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RO, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RR, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SC, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SP, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.TO, TipoAmbiente.Producao)]
        public void InutilizarNumeroNFe(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente, string cnpj = "01111222333444", string cpf = "")
        {
            var xml = new InutNFe
            {
                Versao = "4.00",
                InfInut = new InutNFeInfInut
                {
                    Ano = "20",
                    CNPJ = cnpj,
                    CPF = cpf,
                    CUF = ufBrasil,
                    Mod = ModeloDFe.NFe,
                    NNFIni = 1,
                    NNFFin = 2,
                    Serie = 1,
                    TpAmb = tipoAmbiente,
                    XJust = "Justificativa da inutilização de teste"
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var inutilizacao = new Inutilizacao(xml, configuracao);
            inutilizacao.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            if (inutilizacao.Result != null)
            {
                Assert.True(inutilizacao.Result.InfInut.CUF.Equals(ufBrasil), "Web-service retornou uma UF e está diferente de " + ufBrasil.ToString());
                Assert.True(inutilizacao.Result.InfInut.TpAmb.Equals(tipoAmbiente), "Web-service retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
                if (inutilizacao.Result.InfInut.Id != null)
                {
                    Assert.True(inutilizacao.Result.InfInut.Id.Equals(xml.InfInut.Id), "Web-service retornou uma chave da NFe diferente da enviada na consulta.");
                }
            }
        }
    }
}