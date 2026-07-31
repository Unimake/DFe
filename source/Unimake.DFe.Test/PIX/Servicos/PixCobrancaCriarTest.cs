using Newtonsoft.Json.Linq;
using System.Reflection;
using Unimake.Business.DFe.Servicos;
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

        private static void NormalizarJson(JObject json)
        {
            var method = typeof(Business.DFe.Servicos.PIX.PixCobrancaCriar)
                .BaseType
                .GetMethod("NormalizarJson", BindingFlags.NonPublic | BindingFlags.Static);

            method.Invoke(null, new object[] { json });
        }
    }
}
