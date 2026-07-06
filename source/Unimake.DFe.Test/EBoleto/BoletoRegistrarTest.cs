using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using Newtonsoft.Json.Serialization;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.EBoleto
{
    public class BoletoRegistrarTest : EBoletoTestBase
    {
        /// <summary>
        /// Testar registro de boleto via eBoleto (sandbox)
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void RegistrarBoleto()
        {
            var xml = CriarBoletoRegistrar();

            ExecutarTesteServico(
                () => new Business.DFe.Servicos.EBoleto.BoletoRegistrar(xml, CriarConfiguracao(Servico.EBoletoRegistrar)),
                TemConfiguracaoEBoletoValida());
        }

        /// <summary>
        /// Testar a serializacao JSON dos campos S/N do registro de boleto
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DeveSerializarPermiteRecebimentoParcialComoLetraNoJson()
        {
            var xml = CriarBoletoRegistrar();
            xml.PermiteRecebimentoParcial = SimNaoLetra.Sim;
            xml.PermiteRecebimentoParcialSpecified = true;

            var json = CriarJsonEnvio(xml);

            Assert.Equal("S", json.Value<string>("permiteRecebimentoParcial"));
            Assert.Equal(JTokenType.String, json["permiteRecebimentoParcial"].Type);
        }

        /// <summary>
        /// Testar a serializacao JSON da agencia coletora como texto livre
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DeveSerializarAgenciaColetoraComoTextoNoJson()
        {
            var xml = CriarBoletoRegistrar();
            xml.AgenciaColetoraField = "AGENCIA-01";
            xml.AgenciaColetoraSpecified = true;

            var json = CriarJsonEnvio(xml);

            Assert.Equal("AGENCIA-01", json.Value<string>("agenciaColetora"));
            Assert.Equal(JTokenType.String, json["agenciaColetora"].Type);
        }

        /// <summary>
        /// Testar compatibilidade da propriedade antiga S/N da agencia coletora
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DeveManterCompatibilidadeDaAgenciaColetoraComoLetraNoJson()
        {
            var xml = CriarBoletoRegistrar();
            xml.AgenciaColetora = SimNaoLetra.Nao;
            xml.AgenciaColetoraSpecified = true;

            var json = CriarJsonEnvio(xml);

            Assert.Equal("N", json.Value<string>("agenciaColetora"));
            Assert.Equal(JTokenType.String, json["agenciaColetora"].Type);
        }

        private static JObject CriarJsonEnvio(Business.DFe.Xml.EBoleto.BoletoRegistrar xml)
        {
            var serializer = JsonSerializer.Create(new JsonSerializerSettings
            {
                NullValueHandling = NullValueHandling.Ignore,
                ContractResolver = new CamelCasePropertyNamesContractResolver()
            });

            return JObject.FromObject(xml, serializer);
        }
    }
}
