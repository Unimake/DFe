using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.UMessenger;
using Xunit;

namespace Unimake.DFe.Test.UMessenger
{
    public class PublishUMessengerTest : UMessengerTestBase
    {
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

            ExecutarTesteServico(
                () => new Business.DFe.Servicos.UMessenger.PublishUMessenger(xml, CriarConfiguracao(Servico.UMessengerPublish)),
                TemConfiguracaoUMessengerValida(),
                servico =>
                {
                    Assert.NotNull(servico.Result);
                    Assert.NotEmpty(servico.Result.RawResponse);
                });
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

            ExecutarTesteServico(
                () => new Business.DFe.Servicos.UMessenger.PublishUMessenger(xml, CriarConfiguracao(Servico.UMessengerPublish)),
                TemConfiguracaoUMessengerValida(),
                servico =>
                {
                    Assert.Equal(2, servico.Results.Count);
                    Assert.All(servico.Results, r => Assert.NotEmpty(r.RawResponse));
                });
        }
    }
}
