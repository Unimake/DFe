#if INTEROP
using System.Runtime.InteropServices;
#endif
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using Newtonsoft.Json.Serialization;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Validator;
using Unimake.Business.DFe.Xml;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.PIX
{
    /// <summary>
    /// Classe base para os serviços de PIX
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.PIX.ServicoBase")]
    [ComVisible(true)]
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
        protected abstract Servico ServicoPIX { get; }

        /// <summary>
        /// Arquivo de schema do XML de envio
        /// </summary>
        protected abstract string SchemaArquivoPIX { get; }

        /// <summary>
        /// Construtor
        /// </summary>
        protected ServicoBase() : base() { }

        /// <summary>
        /// Definir configurações
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            Configuracoes.Servico = ServicoPIX;
            Configuracoes.TipoDFe = TipoDFe.PIX;
            Configuracoes.CodigoUF = (int)UFBrasil.AN;
            Configuracoes.SchemaVersao = "1.00";
            Configuracoes.UsaCertificadoDigital = false;

            if(GetBoolTag("Testing") || GetBoolTag("UseHomologServer"))
            {
                Configuracoes.TipoAmbiente = TipoAmbiente.Homologacao;
            }

            Configuracoes.Load(GetType().Name);
            Configuracoes.SchemaArquivo = SchemaArquivoPIX;
            ConfigureAuth();
            ConfigurarRequestURI();

            if(Configuracoes.MetodoAPI != "get")
            {
                Configuracoes.HttpContent = CriarHttpContentPadrao();
            }

            Configuracoes.Definida = true;
        }

        /// <summary>
        /// Configurar URI da requisição
        /// </summary>
        protected virtual void ConfigurarRequestURI()
        {
            var configurationId = GetPropertyString("ConfigurationId");

            if(!string.IsNullOrWhiteSpace(configurationId))
            {
                AdicionarQueryString(new Dictionary<string, string>
                {
                    { "configurationId", configurationId }
                });
            }
        }

        /// <summary>
        /// Criar o conteúdo JSON para consumo da API PIX
        /// </summary>
        protected override HttpContent CriarHttpContentPadrao()
        {
            var settings = new JsonSerializerSettings
            {
                NullValueHandling = NullValueHandling.Ignore,
                ContractResolver = new PIXContractResolver()
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

            if(!string.IsNullOrWhiteSpace(Configuracoes.SchemaArquivo))
            {
                var validar = new ValidarSchema();
                validar.Validar(ConteudoXML, Configuracoes.TipoDFe + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

                if(!validar.Success)
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
        /// Verificar assinatura (não aplicável ao PIX)
        /// </summary>
        protected override void VerificarAssinarXML(string tagAssinatura, string tagAtributoID) { }

        /// <summary>
        /// Inicializar serviço
        /// </summary>
        protected void InicializarServico(TEnvio xml, Configuracao configuracao)
        {
            if(configuracao is null)
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
            if(configuracao is null)
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

        /// <summary>
        /// Adicionar parâmetros na query string das URIs configuradas
        /// </summary>
        protected void AdicionarQueryString(Dictionary<string, string> parametros)
        {
            var queryString = string.Join("&", parametros
                .Where(p => !string.IsNullOrWhiteSpace(p.Value))
                .Select(p => WebUtility.UrlEncode(p.Key) + "=" + WebUtility.UrlEncode(p.Value)));

            if(string.IsNullOrWhiteSpace(queryString))
            {
                return;
            }

            Configuracoes.RequestURIHomologacao = AdicionarQueryString(Configuracoes.RequestURIHomologacao, queryString);
            Configuracoes.RequestURIProducao = AdicionarQueryString(Configuracoes.RequestURIProducao, queryString);
            Configuracoes.RequestURI = AdicionarQueryString(Configuracoes.RequestURI, queryString);
        }

        /// <summary>
        /// Obter valor string de uma propriedade do XML de envio
        /// </summary>
        protected string GetPropertyString(string propertyName)
        {
            var property = Envio.GetType().GetProperty(propertyName, BindingFlags.Instance | BindingFlags.Public);
            var value = property?.GetValue(Envio, null);

            if(value is null)
            {
                return null;
            }

            if(value is DateTimeOffset dateTimeOffset)
            {
                return dateTimeOffset.ToString("yyyy-MM-ddTHH:mm:sszzz");
            }

            if(value is DateTime dateTime)
            {
                return dateTime.ToString("yyyy-MM-ddTHH:mm:sszzz");
            }

            return value.ToString();
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

        private static string AdicionarQueryString(string uri, string queryString)
        {
            if(string.IsNullOrWhiteSpace(uri))
            {
                return uri;
            }

            return uri + (uri.Contains("?") ? "&" : "?") + queryString;
        }

        private static void NormalizarJson(JObject jsonObject)
        {
            RemoverPropriedade(jsonObject, "useHomologServer");
        }

        private static void RemoverPropriedade(JObject jsonObject, string propertyName)
        {
            if(jsonObject.Property(propertyName) != null)
            {
                jsonObject.Remove(propertyName);
            }
        }

        private sealed class PIXContractResolver : CamelCasePropertyNamesContractResolver
        {
            protected override JsonProperty CreateProperty(MemberInfo member, MemberSerialization memberSerialization)
            {
                var property = base.CreateProperty(member, memberSerialization);

                if(member.GetCustomAttributes(typeof(XmlIgnoreAttribute), true)?.Length > 0 ||
                   member.GetCustomAttributes(typeof(XmlAttributeAttribute), true)?.Length > 0)
                {
                    property.Ignored = true;
                    return property;
                }

                if(member.Name.EndsWith("Field", StringComparison.Ordinal))
                {
                    var originalName = member.Name.Substring(0, member.Name.Length - "Field".Length);
                    property.PropertyName = char.ToLowerInvariant(originalName[0]) + originalName.Substring(1);
                }

                var specifiedProperty = member.DeclaringType?.GetProperty(member.Name + "Specified", BindingFlags.Instance | BindingFlags.Public);
                if(specifiedProperty?.PropertyType == typeof(bool))
                {
                    property.ShouldSerialize = instance => (bool)specifiedProperty.GetValue(instance, null);
                }

                return property;
            }
        }
    }
}
