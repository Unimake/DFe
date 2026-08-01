using System;
using System.Collections.Generic;
using System.IO;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.UMessenger;
using Xunit;

namespace Unimake.DFe.Test.UMessenger.Servicos
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
                    Assert.Single(servico.Result.Mensagem);
                    Assert.True(servico.Result.Mensagem[0].Status == 0 || servico.Result.Mensagem[0].Status == 1);
                    Assert.NotEmpty(servico.Result.Mensagem[0].DLLVersao);
                    Assert.Equal(servico.Result.Mensagem[0].MessageID, servico.Result.MessageId);
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
                    Assert.Equal(2, servico.MessageResults.Count);
                    Assert.All(servico.Results, r =>
                    {
                        Assert.Single(r.Mensagem);
                        Assert.True(r.Mensagem[0].Status == 0 || r.Mensagem[0].Status == 1);
                        Assert.NotEmpty(r.Mensagem[0].DLLVersao);
                    });
                });
        }

        [Fact]
        [Trait("DFe", "UMessenger")]
        public void DeveManterMensagemDeErroParaArquivoInexistente()
        {
            var caminho = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N") + ".pdf");
            var servico = CriarServicoComArquivo(caminho, true);

            var exception = Assert.Throws<Exception>(() => servico.Executar());

            Assert.Equal($"O arquivo '{caminho}' não foi encontrado.", exception.Message);
        }

        [Fact]
        [Trait("DFe", "UMessenger")]
        public void DeveExigirTipoDeMidiaDoArquivo()
        {
            var caminho = Path.GetTempFileName();

            try
            {
                var servico = CriarServicoComArquivo(caminho, false);

                var exception = Assert.Throws<Exception>(() => servico.Executar());

                Assert.Equal($"O tipo de mídia do '{caminho}' arquivo não foi informado.", exception.Message);
            }
            finally
            {
                File.Delete(caminho);
            }
        }

        [Fact]
        [Trait("DFe", "UMessenger")]
        public void DeveManterMensagemParaServicoNaoIdentificado()
        {
            const string xml = "<uMessenger versao=\"1.00\"><OutroServico /></uMessenger>";

            var exception = Assert.Throws<Exception>(() =>
                new Business.DFe.Servicos.UMessenger.PublishUMessenger(xml, CriarConfiguracao(Servico.UMessengerPublish)));

            Assert.Equal("Não foi possível identificar qual o tipo de serviço de envio de mensagens via WhatsApp deve ser utilizado.", exception.Message);
        }

        private static Business.DFe.Servicos.UMessenger.PublishUMessenger CriarServicoComArquivo(string caminho, bool informarMediaType)
        {
            var arquivo = new SendTextMessageFile
            {
                FullPath = caminho,
                MediaType = 2,
                MediaTypeSpecified = informarMediaType
            };

            var xml = new uMessengerSendTextMessage
            {
                SendTextMessage = new List<SendTextMessageContent>
                {
                    new SendTextMessageContent
                    {
                        InstanceName = "INSTANCIA-TESTE",
                        To = "5544999999999",
                        Text = "Mensagem de teste.",
                        Testing = true,
                        Files = new SendTextMessageFiles
                        {
                            File = new List<SendTextMessageFile> { arquivo }
                        }
                    }
                }
            };

            return new Business.DFe.Servicos.UMessenger.PublishUMessenger(xml, CriarConfiguracao(Servico.UMessengerPublish));
        }
    }
}
