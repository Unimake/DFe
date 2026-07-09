using System;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.UMessenger.Servicos
{
    public abstract class UMessengerTestBase
    {
        protected static Configuracao CriarConfiguracao(Servico servico) => new Configuracao
        {
            TipoDFe = TipoDFe.UMessenger,
            TipoAmbiente = TipoAmbiente.Homologacao,
            Servico = servico,
            AppId = PropConfig.UMessengerAppId,
            Secret = PropConfig.UMessengerSecret,
            UMessengerInstanceName = PropConfig.UMessengerInstanceName
        };

        protected static bool TemConfiguracaoUMessengerValida() =>
            ValorInformado(PropConfig.UMessengerAppId) &&
            ValorInformado(PropConfig.UMessengerSecret) &&
            ValorInformado(PropConfig.UMessengerInstanceName) &&
            ValorInformado(PropConfig.UMessengerDestinoTeste) &&
            PropConfig.UMessengerDestinoTeste != "55DDNNNNNNNNN";

        protected static void ExecutarTesteServico(Func<Business.DFe.Servicos.UMessenger.PublishUMessenger> criarServico,
                                                   bool configuracaoCompleta,
                                                   Action<Business.DFe.Servicos.UMessenger.PublishUMessenger> assertCompleto)
        {
            Business.DFe.Servicos.UMessenger.PublishUMessenger servico = null;

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

            assertCompleto(servico);
        }

        private static bool ValorInformado(string valor) =>
            !string.IsNullOrWhiteSpace(valor) &&
            !valor.StartsWith("Seu_", StringComparison.OrdinalIgnoreCase) &&
            !valor.StartsWith("Sua_", StringComparison.OrdinalIgnoreCase);
    }
}
