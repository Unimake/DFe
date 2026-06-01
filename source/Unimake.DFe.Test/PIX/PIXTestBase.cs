using System;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.PIX;
using Xunit;

namespace Unimake.DFe.Test.PIX
{
    public abstract class PIXTestBase
    {
        protected static bool TemConfiguracaoPIXValida(bool exigeTxId = false)
        {
            if(!ValorInformado(PropConfig.PIXAppId) ||
               !ValorInformado(PropConfig.PIXSecret) ||
               !ValorInformado(PropConfig.PIXConfigurationId) ||
               !ValorInformado(PropConfig.PIXBeneficiarioInscricao) ||
               !ValorInformado(PropConfig.PIXBeneficiarioNome) ||
               !ValorInformado(PropConfig.PIXBeneficiarioAgencia) ||
               !ValorInformado(PropConfig.PIXBeneficiarioConta) ||
               !ValorInformado(PropConfig.PIXBeneficiarioBanco))
            {
                return false;
            }

            return !exigeTxId || ValorInformado(PropConfig.PIXTxId);
        }

        protected static Configuracao CriarConfiguracao(Servico servico) => new Configuracao
        {
            TipoDFe = TipoDFe.PIX,
            TipoAmbiente = TipoAmbiente.Homologacao,
            Servico = servico,
            AppId = PropConfig.PIXAppId,
            Secret = PropConfig.PIXSecret
        };

        protected static void ExecutarTesteServico(Func<Business.DFe.Servicos.ServicoBase> criarServico, bool configuracaoCompleta)
        {
            Business.DFe.Servicos.ServicoBase servico = null;

            try
            {
                servico = criarServico();
                servico.Executar();
            }
            catch
            {
                if(!configuracaoCompleta)
                {
                    return;
                }

                throw;
            }

            if(!configuracaoCompleta)
            {
                if(!string.IsNullOrWhiteSpace(servico?.RetornoWSString))
                {
                    Assert.NotNull(servico.RetornoWSXML);
                }

                return;
            }

            Assert.NotNull(servico.RetornoWSXML);
            Assert.NotEmpty(servico.RetornoWSString);
        }

        protected static PixCobrancaCriar CriarPixCobrancaCriar()
        {
            return new PixCobrancaCriar
            {
                ConfigurationId = PropConfig.PIXConfigurationId,
                SolicitacaoPagador = "Teste de cobranca PIX",
                TipoCobranca = PixTipoCobranca.Cob,
                TipoCobrancaSpecified = true,
                Valor = 10.50m,
                Chave = PropConfig.PIXChave,
                TxId = GerarTxId(),
                GerarQRCode = true,
                GerarQRCodeSpecified = true,
                Testing = true,
                TestingSpecified = true,
                Beneficiario = CriarBeneficiario()
            };
        }

        protected static PixCobrancaConsultar CriarPixCobrancaConsultar()
        {
            return new PixCobrancaConsultar
            {
                ConfigurationId = PropConfig.PIXConfigurationId,
                StartDate = DateTimeOffset.Now.Date.AddDays(-1),
                EndDate = DateTimeOffset.Now.Date,
                Testing = true,
                TestingSpecified = true,
                Beneficiario = CriarBeneficiario()
            };
        }

        protected static PixConsultar CriarPixConsultar()
        {
            return new PixConsultar
            {
                ConfigurationId = PropConfig.PIXConfigurationId,
                StartDate = DateTimeOffset.Now.Date.AddDays(-1),
                EndDate = DateTimeOffset.Now.Date,
                TxId = PropConfig.PIXTxId,
                Testing = true,
                TestingSpecified = true,
                Beneficiario = CriarBeneficiario()
            };
        }

        protected static PixBeneficiario CriarBeneficiario()
        {
            return new PixBeneficiario
            {
                Inscricao = PropConfig.PIXBeneficiarioInscricao,
                Nome = PropConfig.PIXBeneficiarioNome,
                Conta = new PixContaCorrente
                {
                    Agencia = PropConfig.PIXBeneficiarioAgencia,
                    Numero = PropConfig.PIXBeneficiarioConta,
                    Banco = PropConfig.PIXBeneficiarioBanco
                }
            };
        }

        private static string GerarTxId()
        {
            var sufixo = DateTime.Now.ToString("yyyyMMddHHmmss");
            return ("UNIMAKEPIXTESTE" + sufixo).PadRight(26, '0');
        }

        private static bool ValorInformado(string valor) =>
            !string.IsNullOrWhiteSpace(valor) &&
            !valor.StartsWith("Seu_", StringComparison.OrdinalIgnoreCase) &&
            !valor.StartsWith("Sua_", StringComparison.OrdinalIgnoreCase);
    }
}
