using System;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.EBoleto;
using Xunit;

namespace Unimake.DFe.Test.EBoleto.Servicos
{
    public abstract class EBoletoTestBase
    {
        protected static bool TemConfiguracaoEBoletoValida(bool exigeNumeroNoBanco = false)
        {
            if (!ValorInformado(PropConfig.EBoletoAppId) ||
                !ValorInformado(PropConfig.EBoletoSecret) ||
                !ValorInformado(PropConfig.EBoletoConfigurationId) ||
                !ValorInformado(PropConfig.EBoletoPagadorInscricao) ||
                !ValorInformado(PropConfig.EBoletoPagadorNome))
            {
                return false;
            }

            return !exigeNumeroNoBanco || ValorInformado(PropConfig.EBoletoNumeroNoBancoTeste);
        }

        protected static Configuracao CriarConfiguracao(Servico servico) => new Configuracao
        {
            TipoDFe = TipoDFe.EBoleto,
            TipoAmbiente = TipoAmbiente.Homologacao,
            Servico = servico,
            AppId = PropConfig.EBoletoAppId,
            Secret = PropConfig.EBoletoSecret
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
                if (!configuracaoCompleta)
                {
                    return;
                }

                throw;
            }

            if (!configuracaoCompleta)
            {
                if (!string.IsNullOrWhiteSpace(servico?.RetornoWSString))
                {
                    Assert.NotNull(servico.RetornoWSXML);
                }

                return;
            }

            Assert.NotNull(servico.RetornoWSXML);
            Assert.NotEmpty(servico.RetornoWSString);
        }

        private static bool ValorInformado(string valor) =>
            !string.IsNullOrWhiteSpace(valor) &&
            !valor.StartsWith("Seu_", StringComparison.OrdinalIgnoreCase) &&
            !valor.StartsWith("Sua_", StringComparison.OrdinalIgnoreCase);

        protected static BoletoRegistrar CriarBoletoRegistrar()
        {
            return new BoletoRegistrar
            {
                ConfigurationId = PropConfig.EBoletoConfigurationId,
                Aceite = false,
                Emissao = DateTimeOffset.Now.Date,
                Especie = EBoletoEspecieTitulo.Codigo2,
                NumeroNaEmpresa = "TESTE" + DateTime.Now.ToString("yyyyMMddHHmmss"),
                NumeroParcela = 1,
                Pagador = CriarPagador(),
                PDFConfig = new EBoletoPDFConfig
                {
                    TryGeneratePDF = false
                },
                Testing = true,
                TestingSpecified = true,
                ValorNominal = 10.50m,
                Vencimento = DateTimeOffset.Now.Date.AddDays(5)
            };
        }

        protected static EBoletoPagador CriarPagador()
        {
            return new EBoletoPagador
            {
                Nome = PropConfig.EBoletoPagadorNome,
                Inscricao = PropConfig.EBoletoPagadorInscricao,
                TipoInscricao = PropConfig.EBoletoPagadorInscricao?.Length == 14 ? EBoletoTipoInscricao.CNPJ : EBoletoTipoInscricao.CPF,
                Endereco = new EBoletoEndereco
                {
                    Logradouro = "Rua Teste",
                    Numero = "100",
                    Complemento = "Sala 1",
                    Bairro = "Centro",
                    Cidade = "Paranavaí",
                    UF = "PR",
                    CEP = "87700-000"
                }
            };
        }
    }
}
