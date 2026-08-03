#if INTEROP
using System.Runtime.InteropServices;
#endif
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
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

        private List<retUMessengerPublish> _results;
        private retUMessengerPublish _result;
        private string _requestURIHomologacao;
        private string _requestURIProducao;
        private TipoAmbiente _tipoAmbienteConfigurado;
        private bool _configuracaoOriginalCapturada;

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
        public IReadOnlyList<retUMessengerPublish> Results => _results ?? new List<retUMessengerPublish>();

        /// <summary>
        /// Lista de mensagens de retorno no formato novo do uMessenger
        /// </summary>
        public IReadOnlyList<retUMessengerMensagem> MessageResults => _result?.Mensagem ?? new List<retUMessengerMensagem>();

        /// <summary>
        /// Resultado do primeiro (ou único) envio, desserializado de RetornoWSXML
        /// </summary>
        public retUMessengerPublish Result => _result ?? (RetornoWSXML != null
            ? XMLUtility.Deserializar<retUMessengerPublish>(RetornoWSXML)
            : null);

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

            if (ConteudoXML.GetElementsByTagName("SendTextMessage").Count == 0)
            {
                throw new Exception("Não foi possível identificar qual o tipo de serviço de envio de mensagens via WhatsApp deve ser utilizado.");
            }

            CapturarConfiguracaoOriginal();
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
            _results = new List<retUMessengerPublish>();
            _result = null;

            var sendTextXml = new uMessengerSendTextMessage().LerXML<uMessengerSendTextMessage>(ConteudoXML);
            var nodes = ConteudoXML.GetElementsByTagName("SendTextMessage");
            var mensagensRetorno = new List<retUMessengerMensagem>();

            for (var i = 0; i < nodes.Count; i++)
            {
                var msgData = sendTextXml.SendTextMessage[i];

                TimeoutEmMilissegundos = PossuiArquivos(msgData) ? 180000 : 0;
                Configuracoes.HttpContent = GerarJSONTextMessage(msgData);
                PrepararExecucao(msgData);
                base.Executar();

                var retorno = CriarRetornoCompativel(RetornoWSXML, RetornoWSRawString);
                var mensagem = retorno?.Mensagem != null && retorno.Mensagem.Count > 0
                    ? retorno.Mensagem[0]
                    : new retUMessengerMensagem();

                mensagem.Id = !string.IsNullOrWhiteSpace(msgData.Id) ? msgData.Id : (i + 1).ToString("00");
                mensagensRetorno.Add(mensagem);

                var retornoIndividual = new retUMessengerPublish();
                retornoIndividual.Mensagem.Add(mensagem);
                _results.Add(retornoIndividual);

                if (i < nodes.Count - 1)
                {
                    // Rate limit da API uMessenger: aguarda 3s entre envios consecutivos na mesma instância
                    Thread.Sleep(3000);
                }
            }

            _result = new retUMessengerPublish
            {
                Mensagem = mensagensRetorno
            };

            RetornoWSXML = _result.GerarXML();
            RetornoWSString = RetornoWSXML.OuterXml;
        }

        #endregion Public Methods

        #region Private Methods

        private void PrepararExecucao(SendTextMessageContent mensagem)
        {
            CapturarConfiguracaoOriginal();

            Configuracoes.RequestURIHomologacao = _requestURIHomologacao;
            Configuracoes.RequestURIProducao = _requestURIProducao;
            Configuracoes.TipoAmbiente = mensagem.Testing || mensagem.UseHomologServer
                ? TipoAmbiente.Homologacao
                : _tipoAmbienteConfigurado;

            ConfigureAuth(ResolverInstanceName(mensagem));
        }

        private void CapturarConfiguracaoOriginal()
        {
            if (_configuracaoOriginalCapturada)
            {
                return;
            }

            _requestURIHomologacao = Configuracoes.RequestURIHomologacao;
            _requestURIProducao = Configuracoes.RequestURIProducao;
            _tipoAmbienteConfigurado = Configuracoes.TipoAmbiente;
            _configuracaoOriginalCapturada = true;
        }

        private string ResolverInstanceName(SendTextMessageContent mensagem)
        {
            if (!string.IsNullOrWhiteSpace(mensagem.InstanceName))
            {
                return mensagem.InstanceName.Trim();
            }

            if (!string.IsNullOrWhiteSpace(Configuracoes.UMessengerInstanceName))
            {
                return Configuracoes.UMessengerInstanceName.Trim();
            }

            throw new Exception("InstanceName não informado. Informe no XML (tag InstanceName) ou em Configuracao.UMessengerInstanceName.");
        }

        private static bool PossuiArquivos(SendTextMessageContent mensagem) =>
            mensagem.Files?.File != null && mensagem.Files.File.Count > 0;

        private static retUMessengerPublish CriarRetornoCompativel(XmlDocument retornoXml, string rawResponse)
        {
            retUMessengerPublish retorno;

            if (retornoXml?.DocumentElement != null &&
                string.Equals(retornoXml.DocumentElement.LocalName, "string", StringComparison.OrdinalIgnoreCase))
            {
                retorno = new retUMessengerPublish();
                retorno.Mensagem.Add(new retUMessengerMensagem
                {
                    Status = 1,
                    Motivo = "Mensagem enviada com sucesso.",
                    MessageID = retornoXml.DocumentElement.InnerText?.Trim(),
                    DLLVersao = Info.VersaoDLL
                });
            }
            else
            {
                retorno = retornoXml != null
                    ? XMLUtility.Deserializar<retUMessengerPublish>(retornoXml)
                    : new retUMessengerPublish();
            }

            retorno.RawResponse = rawResponse;

            if (!string.IsNullOrWhiteSpace(rawResponse))
            {
                try
                {
                    var root = JObject.Parse(rawResponse);
                    retorno.LocalId = root.Value<string>("localId");

                    if (string.IsNullOrWhiteSpace(retorno.MessageId))
                    {
                        retorno.MessageId = root.Value<string>("messageId");
                    }
                }
                catch (JsonException)
                {
                }
            }

            return retorno;
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
                        throw new Exception($"O arquivo '{f.FullPath}' não foi encontrado.");
                    }

                    if (!f.MediaTypeSpecified)
                    {
                        throw new Exception($"O tipo de mídia do '{f.FullPath}' arquivo não foi informado.");
                    }

                    files.Add(new
                    {
                        Base64Content = Convert.ToBase64String(File.ReadAllBytes(f.FullPath)),
                        FileName = Path.GetFileName(f.FullPath),
                        Caption = f.Description,
                        MediaType = (int?)f.MediaType
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

        #endregion Private Methods
    }
}
