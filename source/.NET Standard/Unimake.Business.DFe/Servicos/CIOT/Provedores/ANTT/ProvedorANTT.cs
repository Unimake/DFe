using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using Newtonsoft.Json.Serialization;
using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Xml;

namespace Unimake.Business.DFe.Servicos.CIOT.Provedores.ANTT
{
    internal sealed class ProvedorANTT : IProvedorCIOT
    {
        public bool UsaValidacaoSchema => true;

        public bool EnviaConteudoEmRequisicaoGet => false;

        public bool RecriaConteudoAposPrepararExecucao => false;

        public void Configurar(Configuracao configuracao, string nomeServico, Servico servico)
        {
            if (!configuracao.Definida)
            {
                configuracao.Load(nomeServico);
                configuracao.Definida = true;
            }
        }

        public HttpContent CriarHttpContent(XMLBase xml, Servico servico, Configuracao configuracao)
        {
            var settings = new JsonSerializerSettings
            {
                NullValueHandling = NullValueHandling.Ignore,
                ContractResolver = new CIOTContractResolver()
            };
            var jsonObject = JObject.FromObject(xml, JsonSerializer.Create(settings));
            NormalizarCamposDateTime(jsonObject);
            return new StringContent(jsonObject.ToString(Newtonsoft.Json.Formatting.None), System.Text.Encoding.UTF8, configuracao.WebContentType);
        }

        public void Validar(XMLBase xml, Servico servico, Configuracao configuracao) { }

        public void PrepararExecucao(Configuracao configuracao) { }

        public XmlDocument NormalizarRetorno(string retorno, Servico servico) => null;

        private static void NormalizarCamposDateTime(JToken token)
        {
            if (token is JObject objeto)
            {
                var propriedades = new List<JProperty>(objeto.Properties());
                foreach (var propriedade in propriedades)
                {
                    NormalizarCamposDateTime(propriedade.Value);
                    if (!propriedade.Name.EndsWith("Field", StringComparison.Ordinal)) continue;
                    var nomeOriginal = propriedade.Name.Substring(0, propriedade.Name.Length - "Field".Length);
                    foreach (var existente in new List<JProperty>(objeto.Properties()))
                    {
                        if (existente.Name == nomeOriginal) existente.Remove();
                    }
                    objeto.Add(new JProperty(nomeOriginal, propriedade.Value));
                    propriedade.Remove();
                }
            }
            else if (token is JArray array)
            {
                foreach (var item in array) NormalizarCamposDateTime(item);
            }
        }

        private sealed class CIOTContractResolver : DefaultContractResolver
        {
            protected override JsonProperty CreateProperty(MemberInfo member, MemberSerialization memberSerialization)
            {
                var property = base.CreateProperty(member, memberSerialization);
                if (member.GetCustomAttributes(typeof(XmlIgnoreAttribute), true).Length > 0) property.Ignored = true;
                if (member.Name.EndsWith("Field", StringComparison.Ordinal)) property.PropertyName = member.Name;
                return property;
            }
        }
    }
}
