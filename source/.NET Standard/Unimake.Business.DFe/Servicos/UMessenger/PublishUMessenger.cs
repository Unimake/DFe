#if INTEROP
using System.Runtime.InteropServices;
#endif
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;
using System;
using System.Collections.Generic;
using System.IO;
using System.Net.Http;
using System.Text;
using System.Threading;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.UMessenger;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.UMessenger
{
    /// <summary>
    /// Publicar mensagem de texto via uMessenger (WhatsApp)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.UMessenger.PublishUMessenger")]
    [ComVisible(true)]
#endif
    public class PublishUMessenger : ServicoBase
    {
        #region Private Fields

        private List<retUMessengerMensagem> _results;

        private static readonly JsonSerializerSettings JsonSettings = new JsonSerializerSettings
        {
            ContractResolver = new CamelCasePropertyNamesContractResolver(),
            NullValueHandling = NullValueHandling.Ignore
        };

        #endregion Private Fields

        #region Public Properties

        /// <summary>
        /// Lista de resultados do envio (uma entrada por mensagem enviada)
        /// </summary>
        public IReadOnlyList<retUMessengerMensagem> Results => _results ?? new List<retUMessengerMensagem>();

        /// <summary>
        /// Resultado do primeiro (ou único) envio, desserializado de RetornoWSXML
        /// </summary>
        public retUMessengerPublish Result => RetornoWSXML != null
            ? XMLUtility.Deserializar<retUMessengerPublish>(RetornoWSXML)
            : null;

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Enviar mensagem de texto via uMessenger
        /// </summary>
        public PublishUMessenger(uMessengerSendTextMessage xml, Configuracao configuracao)
        {
            if (configuracao is null) throw new ArgumentNullException(nameof(configuracao));
            Inicializar(xml?.GerarXML() ?? throw new ArgumentNullException(nameof(xml)), configuracao);
        }

        /// <summary>
        /// Enviar mensagem via uMessenger a partir de XML em string
        /// </summary>
        public PublishUMessenger(string conteudoXML, Configuracao configuracao)
        {
            if (configuracao is null) throw new ArgumentNullException(nameof(configuracao));
            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML ?? throw new ArgumentNullException(nameof(conteudoXML)));
            Inicializar(doc, configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executar o serviço via INTEROP
        /// </summary>
        [ComVisible(true)]
        public void ExecutarTextMessage([MarshalAs(UnmanagedType.IUnknown)] uMessengerSendTextMessage xml, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null) throw new ArgumentNullException(nameof(configuracao));
                Inicializar(xml?.GerarXML() ?? throw new ArgumentNullException(nameof(xml)), configuracao);
                Executar();
            }
            catch (ValidarXMLException ex) { Exceptions.ThrowHelper.Instance.Throw(ex); }
            catch (Exception ex) { ThrowHelper.Instance.Throw(ex); }
        }
#endif

        #endregion Public Constructors

        #region Protected Methods

        /// <inheritdoc />
        protected override void DefinirConfiguracao()
        {
            Configuracoes.Servico = Servico.UMessengerPublish;
            Configuracoes.TipoDFe = TipoDFe.UMessenger;
            base.DefinirConfiguracao();

            Configuracoes.SchemaArquivo = "uMessengerText_1_00.xsd";

            var firstMsg = ConteudoXML.GetElementsByTagName("SendTextMessage")[0] as XmlElement;
            if (GetBoolTag(firstMsg, "Testing") || GetBoolTag(firstMsg, "UseHomologServer"))
            {
                Configuracoes.TipoAmbiente = TipoAmbiente.Homologacao;
            }

            ConfigureAuth(ResolverInstanceName());
        }

        #endregion Protected Methods

        #region Public Methods

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            _results = new List<retUMessengerMensagem>();

            var sendTextXml = new uMessengerSendTextMessage().LerXML<uMessengerSendTextMessage>(ConteudoXML);
            var nodes = ConteudoXML.GetElementsByTagName("SendTextMessage");

            for (var i = 0; i < nodes.Count; i++)
            {
                var msgData = sendTextXml.SendTextMessage[i];

                Configuracoes.HttpContent = GerarJSONTextMessage(msgData);
                base.Executar();

                var retorno = XMLUtility.Deserializar<retUMessengerPublish>(RetornoWSXML);
                var mensagem = retorno?.Mensagem != null && retorno.Mensagem.Count > 0
                    ? retorno.Mensagem[0]
                    : new retUMessengerMensagem();

                mensagem.Id = !string.IsNullOrWhiteSpace(msgData.Id) ? msgData.Id : (i + 1).ToString("00");
                _results.Add(mensagem);

                if (i < nodes.Count - 1)
                {
                    // Rate limit da API uMessenger: aguarda 3s entre envios consecutivos na mesma instância
                    Thread.Sleep(3000);
                }
            }

            var retornoFinal = new retUMessengerPublish
            {
                Mensagem = _results
            };

            RetornoWSXML = retornoFinal.GerarXML();
            RetornoWSString = RetornoWSXML.OuterXml;
        }

        #endregion Public Methods

        #region Private Methods

        private string ResolverInstanceName()
        {
            var firstMsg = ConteudoXML.GetElementsByTagName("SendTextMessage")[0] as XmlElement;
            var instanceNodes = firstMsg?.GetElementsByTagName("InstanceName");
            if (instanceNodes != null && instanceNodes.Count > 0 && !string.IsNullOrWhiteSpace(instanceNodes[0].InnerText))
            {
                return instanceNodes[0].InnerText.Trim();
            }

            if (!string.IsNullOrWhiteSpace(Configuracoes.UMessengerInstanceName))
            {
                return Configuracoes.UMessengerInstanceName.Trim();
            }

            throw new Exception("InstanceName não informado. Informe no XML (tag InstanceName) ou em Configuracao.UMessengerInstanceName.");
        }

        private HttpContent GerarJSONTextMessage(SendTextMessageContent msg)
        {
            var hasFiles = msg.Files != null && msg.Files.File != null && msg.Files.File.Count > 0;

            object payload;

            if (hasFiles)
            {
                var files = new List<object>();
                foreach (var f in msg.Files.File)
                {
                    if (!File.Exists(f.FullPath))
                    {
                        throw new Exception($"Arquivo não encontrado: '{f.FullPath}'");
                    }

                    files.Add(new
                    {
                        Base64Content = Convert.ToBase64String(File.ReadAllBytes(f.FullPath)),
                        FileName = Path.GetFileName(f.FullPath),
                        Caption = f.Description,
                        MediaType = f.MediaTypeSpecified ? (int?)f.MediaType : null
                    });
                }

                payload = new
                {
                    MessagingService = "WhatsApp",
                    Testing = msg.Testing,
                    InstanceName = msg.InstanceName,
                    To = new { Destination = msg.To },
                    Text = msg.Text.Replace("\\r", "\r").Replace("\\n", "\n"),
                    Files = files
                };
            }
            else
            {
                payload = new
                {
                    MessagingService = "WhatsApp",
                    Testing = msg.Testing,
                    InstanceName = msg.InstanceName,
                    To = new { Destination = msg.To },
                    Text = msg.Text.Replace("\\r", "\r").Replace("\\n", "\n"),
                    Type = "Text"
                };
            }

            var json = JsonConvert.SerializeObject(payload, JsonSettings);
            return new StringContent(json, Encoding.UTF8, Configuracoes.WebContentType);
        }

        private static bool GetBoolTag(XmlElement parent, string tagName)
        {
            if (parent == null) return false;
            var nodes = parent.GetElementsByTagName(tagName);
            if (nodes.Count == 0) return false;
            return bool.TryParse(nodes[0].InnerText, out var val) && val;
        }

        #endregion Private Methods
    }
}
