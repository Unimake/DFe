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
using System.Net.Http;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Xml;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Classe base para os serviços do CIOT
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase : Servicos.ServicoBase
    {
        /// <summary>
        /// Serviço executado
        /// </summary>
        protected abstract Servico ServicoCIOT { get; }

        /// <summary>
        /// Nome da tag raiz do XML de retorno
        /// </summary>
        protected abstract string NomeRootRetorno { get; }

        /// <summary>
        /// Objeto do XML de envio
        /// </summary>
        protected abstract XMLBase XmlEnvio { get; }

        /// <summary>
        /// Construtor
        /// </summary>
        protected ServicoBase() : base() { }

        /// <summary>
        /// Obter o XML de envio tipado
        /// </summary>
        protected TEnvio ObterEnvio<TEnvio>(ref TEnvio envio)
            where TEnvio : XMLBase, new()
        {
            return envio ?? (envio = new TEnvio().LerXML<TEnvio>(ConteudoXML));
        }

        /// <summary>
        /// Obter o resultado tipado do serviço
        /// </summary>
        protected TRetorno ObterResult<TRetorno>()
            where TRetorno : XMLBase, new()
        {
            if (RetornoWSXML?.DocumentElement != null)
            {
                NormalizarRetorno();
                return new TRetorno().LerXML<TRetorno>(RetornoWSXML);
            }

            return new TRetorno();
        }

        /// <summary>
        /// Definir configurações
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                Configuracoes.TipoDFe = TipoDFe.CIOT;
                Configuracoes.Servico = ServicoCIOT;
                Configuracoes.CodigoUF = (int)UFBrasil.AN;
                Configuracoes.SchemaVersao = "1.00";
            }

            base.DefinirConfiguracao();
        }

        /// <summary>
        /// Criar o conteúdo JSON para consumo da API ANTT
        /// </summary>
        protected override HttpContent CriarHttpContentPadrao()
        {
            var settings = new JsonSerializerSettings
            {
                NullValueHandling = NullValueHandling.Ignore,
                ContractResolver = new CIOTContractResolver()
            };
            var jsonObject = JObject.FromObject(XmlEnvio, JsonSerializer.Create(settings));
            NormalizarCamposDateTime(jsonObject);
            var json = jsonObject.ToString(Newtonsoft.Json.Formatting.None);

            return new StringContent(json, Encoding.UTF8, Configuracoes.WebContentType);
        }

        /// <summary>
        /// Validar o XML
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
        /// Validar conteúdo do XML
        /// </summary>
        protected override void XmlValidarConteudo() { }

        /// <inheritdoc />
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            base.Executar();
            NormalizarRetorno();
        }

        /// <summary>
        /// Inicializar serviço
        /// </summary>
        protected void InicializarServico<TEnvio>(TEnvio xml, Configuracao configuracao)
            where TEnvio : XMLBase, new()
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
            doc.LoadXml(conteudoXML);

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
        /// Criar XML de retorno tipado
        /// </summary>
        protected virtual XmlDocument CriarXMLRetornoTipado()
        {
            var doc = new XmlDocument();
            var rootName = NomeRootRetorno;

            if (RetornoWSXML.DocumentElement.Name == rootName)
            {
                doc.LoadXml(RetornoWSXML.OuterXml);
                return doc;
            }

            var root = doc.CreateElement(rootName, "http://www.antt.gov.br/ciot");
            doc.AppendChild(root);

            if (RetornoWSXML.DocumentElement.LocalName == "temp" && RetornoWSXML.DocumentElement["error"] != null)
            {
                root.AppendChild(CopiarNodeComNamespace(doc, RetornoWSXML.DocumentElement, "http://www.antt.gov.br/ciot"));
                return doc;
            }

            foreach (XmlNode child in RetornoWSXML.DocumentElement.ChildNodes)
            {
                root.AppendChild(CopiarNodeComNamespace(doc, child, "http://www.antt.gov.br/ciot"));
            }

            return doc;
        }

        /// <summary>
        /// Normalizar XML de retorno
        /// </summary>
        protected void NormalizarRetorno()
        {
            if (RetornoWSXML?.DocumentElement == null)
            {
                return;
            }

            RetornoWSXML = CriarXMLRetornoTipado();
            RetornoWSString = RetornoWSXML.OuterXml;
        }

        private static XmlNode CopiarNodeComNamespace(XmlDocument doc, XmlNode origem, string ns)
        {
            if (origem.NodeType == XmlNodeType.Element)
            {
                var elemento = doc.CreateElement(origem.LocalName, ns);

                foreach (XmlAttribute atributo in origem.Attributes)
                {
                    elemento.SetAttribute(atributo.Name, atributo.Value);
                }

                foreach (XmlNode filho in origem.ChildNodes)
                {
                    elemento.AppendChild(CopiarNodeComNamespace(doc, filho, ns));
                }

                return elemento;
            }

            return doc.ImportNode(origem, true);
        }

        private static void NormalizarCamposDateTime(JToken token)
        {
            if (token is JObject objeto)
            {
                var propriedades = new List<JProperty>(objeto.Properties());

                foreach (var propriedade in propriedades)
                {
                    NormalizarCamposDateTime(propriedade.Value);

                    if (!propriedade.Name.EndsWith("Field", StringComparison.Ordinal))
                    {
                        continue;
                    }

                    var nomeOriginal = propriedade.Name.Substring(0, propriedade.Name.Length - "Field".Length);
                    foreach (var existente in new List<JProperty>(objeto.Properties().Where(p => p.Name == nomeOriginal)))
                    {
                        existente.Remove();
                    }

                    objeto.Add(new JProperty(nomeOriginal, propriedade.Value));
                    propriedade.Remove();
                }
            }
            else if (token is JArray array)
            {
                foreach (var item in array)
                {
                    NormalizarCamposDateTime(item);
                }
            }
        }

        private sealed class CIOTContractResolver : DefaultContractResolver
        {
            protected override JsonProperty CreateProperty(MemberInfo member, MemberSerialization memberSerialization)
            {
                var property = base.CreateProperty(member, memberSerialization);
                var xmlIgnore = member.GetCustomAttributes(typeof(XmlIgnoreAttribute), true);

                if (xmlIgnore?.Length > 0)
                {
                    property.Ignored = true;
                }

                if (member.Name.EndsWith("Field", StringComparison.Ordinal))
                {
                    property.PropertyName = member.Name;
                }

                return property;
            }
        }
    }
}
