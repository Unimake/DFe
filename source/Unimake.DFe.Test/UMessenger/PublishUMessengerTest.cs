using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.UMessenger;
using Xunit;

namespace Unimake.DFe.Test.UMessenger
{
    public class PublishUMessengerTest
    {
        private static Configuracao CriarConfiguracao() => new Configuracao
        {
            TipoDFe = TipoDFe.UMessenger,
            TipoAmbiente = TipoAmbiente.Homologacao,
            Servico = Servico.UMessengerPublish,
            AppId = PropConfig.UMessengerAppId,
            Secret = PropConfig.UMessengerSecret,
            UMessengerInstanceName = PropConfig.UMessengerInstanceName
        };

        /// <summary>
        /// Testar envio de mensagem de texto simples via uMessenger (sandbox)
        /// </summary>
        [Fact]
        [Trait("DFe", "UMessenger")]
        public void PublicarSendTextMessage()
        {
            var xml = new uMessengerSendTextMessage
            {
                SendTextMessage = new List<SendTextMessageContent>
                {
                    new SendTextMessageContent
                    {
                        InstanceName = PropConfig.UMessengerInstanceName,
                        To = PropConfig.UMessengerDestinoTeste,
                        Text = "Olá! Esta é uma mensagem de teste enviada via uMessenger.\\nAtenciosamente, Equipe Unimake.",
                        Testing = true
                    }
                }
            };

            var servico = new Business.DFe.Servicos.UMessenger.PublishUMessenger(xml, CriarConfiguracao());
            servico.Executar();

            Assert.NotNull(servico.Result);
            Assert.NotEmpty(servico.Result.RawResponse);
        }

        /// <summary>
        /// Testar envio de múltiplas mensagens de texto via uMessenger (sandbox)
        /// </summary>
        [Fact]
        [Trait("DFe", "UMessenger")]
        public void PublicarSendTextMessageMultiplo()
        {
            var xml = new uMessengerSendTextMessage
            {
                SendTextMessage = new List<SendTextMessageContent>
                {
                    new SendTextMessageContent
                    {
                        Id = "MSG001",
                        InstanceName = PropConfig.UMessengerInstanceName,
                        To = PropConfig.UMessengerDestinoTeste,
                        Text = "Primeira mensagem de teste.",
                        Testing = true
                    },
                    new SendTextMessageContent
                    {
                        Id = "MSG002",
                        InstanceName = PropConfig.UMessengerInstanceName,
                        To = PropConfig.UMessengerDestinoTeste,
                        Text = "Segunda mensagem de teste.",
                        Testing = true
                    }
                }
            };

            var servico = new Business.DFe.Servicos.UMessenger.PublishUMessenger(xml, CriarConfiguracao());
            servico.Executar();

            Assert.Equal(2, servico.Results.Count);
            Assert.All(servico.Results, r => Assert.NotEmpty(r.RawResponse));
        }
    }
}
