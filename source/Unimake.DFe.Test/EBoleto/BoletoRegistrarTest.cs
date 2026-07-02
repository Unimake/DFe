using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System;
using System.Reflection;
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
            xml.AgenciaColetora = SimNaoLetra.Nao;
            xml.AgenciaColetoraSpecified = true;

            var json = CriarJsonEnvio(xml);

            Assert.Equal("S", json.Value<string>("permiteRecebimentoParcial"));
            Assert.Equal("N", json.Value<string>("agenciaColetora"));
            Assert.Equal(JTokenType.String, json["permiteRecebimentoParcial"].Type);
            Assert.Equal(JTokenType.String, json["agenciaColetora"].Type);
        }

        private static JObject CriarJsonEnvio(Business.DFe.Xml.EBoleto.BoletoRegistrar xml)
        {
            var resolverType = typeof(Business.DFe.Servicos.EBoleto.ServicoBase<>)
                .GetNestedType("EBoletoContractResolver", BindingFlags.NonPublic)
                .MakeGenericType(typeof(Business.DFe.Xml.EBoleto.BoletoRegistrar));

            var serializer = JsonSerializer.Create(new JsonSerializerSettings
            {
                NullValueHandling = NullValueHandling.Ignore,
                ContractResolver = (Newtonsoft.Json.Serialization.IContractResolver)Activator.CreateInstance(resolverType, true)
            });

            return JObject.FromObject(xml, serializer);
        }
    }
}
