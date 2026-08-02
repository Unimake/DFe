using Newtonsoft.Json.Linq;
using System;
using System.IO;
using System.Reflection;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.PIX;
using Unimake.Exceptions;
using Xunit;

namespace Unimake.DFe.Test.PIX.Servicos
{
    public class PixCobrancaCriarTest : PIXTestBase
    {
        /// <summary>
        /// Testar criacao de cobranca PIX (sandbox)
        /// </summary>
        [Fact]
        [Trait("DFe", "PIX")]
        public void CriarCobrancaPIX()
        {
            var xml = CriarPixCobrancaCriar();

            ExecutarTesteServico(
                () => new Business.DFe.Servicos.PIX.PixCobrancaCriar(xml, CriarConfiguracao(Servico.PIXCobrancaCriar)),
                TemConfiguracaoPIXValida());
        }

        /// <summary>
        /// Testar a serialização JSON do valor da cobrança PIX
        /// </summary>
        [Fact]
        [Trait("DFe", "PIX")]
        public void DeveSerializarValorComoObjetoOriginalNoJson()
        {
            var json = new JObject
            {
                { "valor", 10.5m }
            };

            NormalizarJson(json);

            Assert.Equal(JTokenType.Object, json["valor"].Type);
            Assert.Equal(JTokenType.Float, json["valor"]["original"].Type);
            Assert.Equal(10.5m, json["valor"].Value<decimal>("original"));
        }

        /// <summary>
        /// Testar a compatibilidade dos formatos de imagem com o contrato do UniNFe
        /// </summary>
        [Theory]
        [InlineData(PixQrCodeImageFormat.GIF, 0)]
        [InlineData(PixQrCodeImageFormat.JPEG, 1)]
        [InlineData(PixQrCodeImageFormat.PNG, 2)]
        [InlineData(PixQrCodeImageFormat.WEBP, 3)]
        [Trait("DFe", "PIX")]
        public void DevePreservarCodigoDosFormatosDeImagem(PixQrCodeImageFormat imageFormat, int codigo) =>
            Assert.Equal(codigo, (int)imageFormat);

        /// <summary>
        /// Testar a gravação da imagem Base64 do QRCode e a atualização do XML de retorno
        /// </summary>
        [Fact]
        [Trait("DFe", "PIX")]
        public void DeveGravarQRCodeEAtualizarCaminhoNoRetorno()
        {
            var servico = CriarServicoSemAutenticacao(CriarPixCobrancaCriar());
            var retorno = new retPIXCobrancaCriar
            {
                Status = 0,
                ImageQRCode = "AQIDBA=="
            };
            var caminhoArquivo = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N") + ".png");

            try
            {
                servico.RetornoWSXML = retorno.GerarXML();
                servico.RetornoWSString = servico.RetornoWSXML.OuterXml;

                servico.GravarQRCode(caminhoArquivo);

                Assert.Equal(new byte[] { 1, 2, 3, 4 }, File.ReadAllBytes(caminhoArquivo));
                Assert.Equal(caminhoArquivo, servico.Result.ImageQRCode);
                Assert.Contains("<ImageQRCode>" + caminhoArquivo + "</ImageQRCode>", servico.RetornoWSString);
            }
            finally
            {
                servico.Dispose();

                if (File.Exists(caminhoArquivo))
                {
                    File.Delete(caminhoArquivo);
                }
            }
        }

        /// <summary>
        /// Testar as validações condicionais da cobrança com vencimento
        /// </summary>
        [Fact]
        [Trait("DFe", "PIX")]
        public void DeveExigirCalendarioEDevedorNaCobrancaComVencimento()
        {
            var xml = CriarPixCobrancaCriar();
            xml.TipoCobranca = PixTipoCobranca.CobV;

            using (var servico = CriarServicoSemAutenticacao(xml))
            {
                var exception = ExecutarValidacaoConteudo(servico);
                Assert.Equal("Se o conteudo da tag <TipoCobranca> for igual a 1 e obrigatorio informar o grupo de tag <Calendario>.", exception.Message);
            }

            xml.Calendario = new PixCalendario
            {
                Criacao = DateTime.Now
            };

            using (var servico = CriarServicoSemAutenticacao(xml))
            {
                var exception = ExecutarValidacaoConteudo(servico);
                Assert.Equal("Se o conteudo da tag <TipoCobranca> for igual a 1 e obrigatorio informar o grupo de tag <Devedor>.", exception.Message);
            }
        }

        private static void NormalizarJson(JObject json)
        {
            var method = typeof(Business.DFe.Servicos.PIX.PixCobrancaCriar)
                .BaseType
                .GetMethod("NormalizarJson", BindingFlags.NonPublic | BindingFlags.Static);

            method.Invoke(null, new object[] { json });
        }

        private static ValidarXMLException ExecutarValidacaoConteudo(Business.DFe.Servicos.PIX.PixCobrancaCriar servico)
        {
            var method = typeof(Business.DFe.Servicos.PIX.PixCobrancaCriar)
                .GetMethod("XmlValidarConteudo", BindingFlags.NonPublic | BindingFlags.Instance);
            var exception = Assert.Throws<TargetInvocationException>(() => method.Invoke(servico, null));
            return Assert.IsType<ValidarXMLException>(exception.InnerException);
        }

        private static Business.DFe.Servicos.PIX.PixCobrancaCriar CriarServicoSemAutenticacao(Business.DFe.Xml.PIX.PixCobrancaCriar xml)
        {
            var servico = new Business.DFe.Servicos.PIX.PixCobrancaCriar();
            var envioField = servico.GetType().BaseType.GetField("envio", BindingFlags.NonPublic | BindingFlags.Instance);
            envioField.SetValue(servico, xml);
            return servico;
        }
    }
}
