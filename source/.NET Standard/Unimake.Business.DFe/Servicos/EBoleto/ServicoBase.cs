#if INTEROP
using System.Runtime.InteropServices;
#endif
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using Newtonsoft.Json.Serialization;
using System;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Xml;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.EBoleto
{
    /// <summary>
    /// Classe base para os serviços do eBoleto
    /// </summary>
#if INTEROP
    [ComVisible(false)]
#endif
    public abstract class ServicoBase<TEnvio> : Servicos.ServicoBase
        where TEnvio : XMLBase, new()
    {
        private TEnvio envio;

        /// <summary>
        /// XML de envio do serviço
        /// </summary>
        public TEnvio Envio
        {
            get => envio ?? (envio = new TEnvio().LerXML<TEnvio>(ConteudoXML));
            protected set => envio = value;
        }

        /// <summary>
        /// Serviço executado
        /// </summary>
        protected abstract Servico ServicoEBoleto { get; }

        /// <summary>
        /// Arquivo de schema do XML de envio
        /// </summary>
        protected abstract string SchemaArquivoEBoleto { get; }

        /// <summary>
        /// Construtor
        /// </summary>
        protected ServicoBase() : base() { }

        /// <summary>
        /// Definir configurações
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            Configuracoes.Servico = ServicoEBoleto;
            Configuracoes.TipoDFe = TipoDFe.EBoleto;
            Configuracoes.CodigoUF = (int)UFBrasil.AN;
            Configuracoes.SchemaVersao = "1.00";
            Configuracoes.UsaCertificadoDigital = false;

            if (GetBoolTag("Testing") || GetBoolTag("UseHomologServer"))
            {
                Configuracoes.TipoAmbiente = TipoAmbiente.Homologacao;
            }

            Configuracoes.Load(GetType().Name);
            Configuracoes.SchemaArquivo = SchemaArquivoEBoleto;
            ConfigureAuth();
            Configuracoes.HttpContent = CriarHttpContentPadrao();
            Configuracoes.Definida = true;
        }

        /// <summary>
        /// Criar o conteúdo JSON para consumo da API eBoleto
        /// </summary>
        protected override HttpContent CriarHttpContentPadrao()
        {
            var settings = new JsonSerializerSettings
            {
                NullValueHandling = NullValueHandling.Ignore,
                ContractResolver = new EBoletoContractResolver()
            };

            var jsonObject = JObject.FromObject(Envio, JsonSerializer.Create(settings));
            NormalizarJson(jsonObject);
            var json = jsonObject.ToString(Newtonsoft.Json.Formatting.None);

            var content = new StringContent(json, Encoding.UTF8);
            content.Headers.ContentType = new MediaTypeHeaderValue(Configuracoes.WebContentType);

            return content;
        }

        /// <summary>
        /// Validar o XML contra o schema XSD
        /// </summary>
        protected override void XmlValidar()
        {
            XmlValidarConteudo();

            if (!string.IsNullOrWhiteSpace(Configuracoes.SchemaArquivo))
            {
                var validar = new ValidarSchema();
                validar.Validar(ConteudoXML, Configuracoes.TipoDFe + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

                if (!validar.Success)
                {
                    throw new ValidarXMLException(validar.ErrorMessage);
                }
            }
        }

        /// <summary>
        /// Validações manuais de conteúdo das tags do XML
        /// </summary>
        protected override void XmlValidarConteudo() { }

        /// <summary>
        /// Verificar assinatura (não aplicável ao eBoleto)
        /// </summary>
        protected override void VerificarAssinarXML(string tagAssinatura, string tagAtributoID) { }

        /// <summary>
        /// Inicializar serviço
        /// </summary>
        protected void InicializarServico(TEnvio xml, Configuracao configuracao)
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(xml?.GerarXML() ?? throw new ArgumentNullException(nameof(xml)), configuracao);
        }

        /// <summary>
        /// Inicializar serviço
        /// </summary>
        protected void InicializarServico(string conteudoXML, Configuracao configuracao)
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML ?? throw new ArgumentNullException(nameof(conteudoXML)));
            Inicializar(doc, configuracao);
        }

        /// <inheritdoc />
#if INTEROP
        [ComVisible(false)]
#endif
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            StreamWriter streamWriter = null;

            try
            {
                streamWriter = File.CreateText(Path.Combine(pasta, nomeArquivo));
                streamWriter.Write(conteudoXML);
            }
            finally
            {
                streamWriter?.Close();
            }
        }

        private void ConfigureAuth()
        {
            var token = UMessengerTokenCache.GetOrAcquireToken(
                Configuracoes.AppId,
                Configuracoes.Secret,
                Configuracoes.TipoAmbiente != TipoAmbiente.Producao,
                Configuracoes.RequestURILoginProducao,
                Configuracoes.RequestURILoginHomologacao);

            Configuracoes.MunicipioToken = "Bearer " + token;
        }

        private bool GetBoolTag(string tagName)
        {
            var nodes = ConteudoXML.GetElementsByTagName(tagName);
            return nodes.Count > 0 && bool.TryParse(nodes[0].InnerText, out var value) && value;
        }

        private static void NormalizarJson(JObject jsonObject)
        {
            RemoverPropriedade(jsonObject, "useHomologServer");

            if (jsonObject.TryGetValue("numerosNoBanco", out var numerosNoBanco) && numerosNoBanco is JObject numerosNoBancoObject)
            {
                if (numerosNoBancoObject.TryGetValue("numeroNoBanco", out var numeroNoBanco))
                {
                    jsonObject["numeroNoBanco"] = numeroNoBanco;
                }

                jsonObject.Remove("numerosNoBanco");
            }

            NormalizarArrays(jsonObject);
        }

        private static void NormalizarArrays(JToken token)
        {
            if (token is JObject obj)
            {
                foreach (var property in obj.Properties().ToList())
                {
                    if (property.Value is JObject childObject && childObject.Properties().Count() == 1)
                    {
                        var childProperty = childObject.Properties().First();
                        if (childProperty.Value is JArray && (childProperty.Name == "mensagem" || childProperty.Name == "numeroNoBanco"))
                        {
                            property.Value = childProperty.Value;
                            continue;
                        }
                    }

                    NormalizarArrays(property.Value);
                }
            }
            else if (token is JArray array)
            {
                foreach (var item in array)
                {
                    NormalizarArrays(item);
                }
            }
        }

        private static void RemoverPropriedade(JObject jsonObject, string propertyName)
        {
            if (jsonObject.Property(propertyName) != null)
            {
                jsonObject.Remove(propertyName);
            }
        }

        private sealed class EBoletoContractResolver : CamelCasePropertyNamesContractResolver
        {
            protected override JsonProperty CreateProperty(MemberInfo member, MemberSerialization memberSerialization)
            {
                var property = base.CreateProperty(member, memberSerialization);

                if (member.GetCustomAttributes(typeof(XmlIgnoreAttribute), true)?.Length > 0 ||
                    member.GetCustomAttributes(typeof(XmlAttributeAttribute), true)?.Length > 0)
                {
                    property.Ignored = true;
                    return property;
                }

                if (member.Name.EndsWith("Field", StringComparison.Ordinal))
                {
                    var originalName = member.Name.Substring(0, member.Name.Length - "Field".Length);
                    property.PropertyName = char.ToLowerInvariant(originalName[0]) + originalName.Substring(1);
                }

                if (property.PropertyType == typeof(SimNaoLetra))
                {
                    property.Converter = new XmlEnumJsonConverter();
                }

                var specifiedProperty = member.DeclaringType?.GetProperty(member.Name + "Specified", BindingFlags.Instance | BindingFlags.Public);
                if (specifiedProperty?.PropertyType == typeof(bool))
                {
                    property.ShouldSerialize = instance => (bool)specifiedProperty.GetValue(instance, null);
                }

                return property;
            }
        }

        private sealed class XmlEnumJsonConverter : JsonConverter
        {
            public override bool CanRead => false;

            public override bool CanConvert(Type objectType) => objectType.IsEnum;

            public override void WriteJson(JsonWriter writer, object value, JsonSerializer serializer)
            {
                if (value == null)
                {
                    writer.WriteNull();
                    return;
                }

                var field = value.GetType().GetField(value.ToString());
                var xmlEnum = field?.GetCustomAttribute<XmlEnumAttribute>();
                writer.WriteValue(xmlEnum?.Name ?? value.ToString());
            }

            public override object ReadJson(JsonReader reader, Type objectType, object existingValue, JsonSerializer serializer)
            {
                throw new NotSupportedException();
            }
        }
    }
}
