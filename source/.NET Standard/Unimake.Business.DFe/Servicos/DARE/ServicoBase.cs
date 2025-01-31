using System;
using System.IO;
using System.Net.Http;
using System.Runtime.InteropServices;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Validator;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.DARE
{
    /// <summary>
    /// Classe base para os serviços da DARE
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.DARE.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase : Servicos.ServicoBase
    {
        #region Protected Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        protected ServicoBase() : base() { }

        #endregion Protected Constructors

        #region Protected Methods

        /// <summary>
        /// Definir configurações
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            Configuracoes.CodigoUF = (int)UFBrasil.AN;

            //Esta linha tem que ficar fora do if acima, pois tem que carregar esta parte, independente, pois o que é carregado sempre é automático. Mudar isso, vai gerar falha no UNINFE, principalmente no envio dos eventos, onde eu defino as configurações manualmente. Wandrey 07/12/2020
            Configuracoes.Load(GetType().Name);
        }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            XmlValidarConteudo(); // Efetuar a validação antes de validar schema para evitar alguns erros que não ficam claros para o desenvolvedor.

            if (!string.IsNullOrWhiteSpace(Configuracoes.SchemaArquivo))
            {
                var validar = new ValidarSchema();
                validar.Validar(ConteudoXML, Configuracoes.TipoDFe.ToString() + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

                if (!validar.Success)
                {
                    throw new ValidarXMLException(validar.ErrorMessage);
                }
            }
        }

        /// <summary>
        /// Validar, o conteúdo das tags do XML, alguns validações manuais que o schema não faz. Vamos implementando novas regras na medida da necessidade de cada serviço.
        /// </summary>
        protected override void XmlValidarConteudo() { }

        /// <summary>
        /// Verifica se o XML está assinado, se não estiver assina. Só faz isso para XMLs que tem tag de assinatura, demais ele mantem como está, sem assinar.
        /// </summary>
        /// <param name="tagAssinatura">Tag de assinatura</param>
        /// <param name="tagAtributoID">Tag que detêm o atributo ID</param>
        protected override void VerificarAssinarXML(string tagAssinatura, string tagAtributoID)
        {
            if (!string.IsNullOrWhiteSpace(Configuracoes.TagAssinatura) && !AssinaturaDigital.EstaAssinado(ConteudoXML, Configuracoes.TagAssinatura))
            {
                var eventoEspecifico = string.Empty;
                var listEventos = ConteudoXML.GetElementsByTagName("evento");

                foreach (XmlNode nodeEvento in listEventos)
                {
                    var elementEvento = (XmlElement)nodeEvento;
                    var DAREEvento = elementEvento.GetElementsByTagName("DARE")[0];

                    var xmlEventoEspecifico = new XmlDocument();
                    xmlEventoEspecifico.LoadXml(DAREEvento.OuterXml);

                    eventoEspecifico = DAREEvento.FirstChild.Name;

                    if (!AssinaturaDigital.EstaAssinado(xmlEventoEspecifico, Configuracoes.TiposEventosEspecificos[eventoEspecifico.ToString()].TagAssinatura))
                    {
                        AssinaturaDigital.Assinar(xmlEventoEspecifico, Configuracoes.TiposEventosEspecificos[eventoEspecifico.ToString()].TagAssinatura,
                            Configuracoes.TiposEventosEspecificos[eventoEspecifico.ToString()].TagAtributoID,
                            Configuracoes.CertificadoDigital, AlgorithmType.Sha256, false, "Id");

                        nodeEvento.RemoveChild(DAREEvento);
                        nodeEvento.AppendChild(ConteudoXML.ImportNode(xmlEventoEspecifico.DocumentElement, true));
                    }
                }
            }
        }

        /// <summary>
        /// </summary>
        /// <param name="conteudoXML"></param>
        /// <param name="configuracao"></param>
        /// <exception cref="ArgumentNullException"></exception>
        protected override void Inicializar(XmlDocument conteudoXML, Configuracao configuracao)
        {
            Configuracoes = configuracao ?? throw new ArgumentNullException(nameof(configuracao));
            ConteudoXML = conteudoXML ?? throw new ArgumentNullException(nameof(conteudoXML));

            if (!Configuracoes.Definida)
            {
                DefinirConfiguracao();
            }

            // 
            /* Retirado a linha especial do Wandrey; ID #170137

                ¯\_(ツ)_/¯

                 There is always a solution

                         ,;~;,
                            /\_
                           (  /
                           ((),     ;,;
                           |  \\  ,;;'(
                       __ _(  )'~;;'   \
                     /'  '\'()/~' \ /'\.)
                  ,;(      )||     |
                 ,;' \    /-(.;,   )
                      ) /       ) /
                     //         ||
                    (_\         (_\

                 go horse <3

            */

            System.Diagnostics.Trace.WriteLine(ConteudoXML?.InnerXml, "Unimake.DFe");

            //Forçar criar a tag QrCode bem como assinatura para que o usuário possa acessar o conteúdo no objeto do XML antes de enviar
            _ = ConteudoXMLAssinado;

            XmlValidar();

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
            AjustarXMLAposAssinado();

            XmlValidar();

            if (!(ValidatorFactory.BuidValidator(ConteudoXML.InnerXml)?.Validate() ?? true))
            {
                return;
            }

            if (!string.IsNullOrWhiteSpace(Configuracoes.TagAssinatura) && Configuracoes.NaoAssina != null && Configuracoes.NaoAssina != Configuracoes.TipoAmbiente)
            {
                if (!AssinaturaDigital.EstaAssinado(ConteudoXML, Configuracoes.TagAssinatura))
                {
                    AssinaturaDigital.Assinar(ConteudoXML, Configuracoes.TagAssinatura, Configuracoes.TagAtributoID, Configuracoes.CertificadoDigital, AlgorithmType.Sha1, true, "Id");
                    AjustarXMLAposAssinado();
                }
            }

            var apiConfig = new APIConfig
            {
                ContentType = Configuracoes.WebContentType,
                RequestURI = (Configuracoes.TipoAmbiente == TipoAmbiente.Producao ? Configuracoes.RequestURIProducao : Configuracoes.RequestURIHomologacao),
                TagRetorno = Configuracoes.WebTagRetorno,
                WebSoapString = Configuracoes.WebSoapString,
                MetodoAPI = Configuracoes.MetodoAPI,
                Token = Configuracoes.MunicipioToken,
                WebAction = Configuracoes.WebActionProducao,
                MunicipioSenha = Configuracoes.MunicipioSenha,
                MunicipioUsuario = Configuracoes.MunicipioUsuario,
                ResponseMediaType = Configuracoes.ResponseMediaType,
                Servico = Configuracoes.Servico,
                UsaCertificadoDigital = Configuracoes.UsaCertificadoDigital,
                Host = (Configuracoes.TipoAmbiente == TipoAmbiente.Producao ? Configuracoes.HostProducao : Configuracoes.HostHomologacao),
                ApiKey = Configuracoes.ApiKey,
                HttpContent = Configuracoes.HttpContent,
            };

            var consumirAPI = new ConsumirAPI();
            consumirAPI.ExecutarServico(ConteudoXML, apiConfig, Configuracoes.CertificadoDigital);

            RetornoWSString = consumirAPI.RetornoServicoString;
            RetornoWSXML = consumirAPI.RetornoServicoXML;
            RetornoStream = consumirAPI.RetornoStream;  //Retorno específico para criação de .pdf para os casos em que a String corrompe o conteúdo. Mauricio 27/09/2023 #157859
            //HttpStatusCode = consumirAPI.HttpStatusCode;

            //base.Executar();
        }

        /// <summary>
        /// Gravar o XML de distribuição em uma pasta no HD
        /// </summary>
        /// <param name="pasta">Pasta onde deve ser gravado o XML no HD</param>
        /// <param name="nomeArquivo">Nome do arquivo a ser gravado no HD</param>
        /// <param name="conteudoXML">String contendo o conteúdo do XML a ser gravado no HD</param>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            StreamWriter streamWriter = null;

            try
            {
                var conteudoXmlDistribuicao = conteudoXML;

                streamWriter = File.CreateText(Path.Combine(pasta, nomeArquivo));
                streamWriter.Write(conteudoXmlDistribuicao);
            }
            finally
            {
                if (streamWriter != null)
                {
                    streamWriter.Close();
                }
            }
        }

        /// <summary>
        /// Gravar o XML de distribuição em um stream
        /// </summary>
        /// <param name="value">Conteúdo a ser gravado no stream</param>
        /// <param name="stream">Stream que vai receber o conteúdo do XML</param>
        /// <param name="encoding">Define o encoding do stream, caso não informado ,será usado o UTF8</param>
#if INTEROP
        [ComVisible(false)]
#endif
        public virtual void GravarXmlDistribuicao(Stream stream, string value, Encoding encoding = null)
        {
            if (stream is null)
            {
                throw new ArgumentNullException(nameof(stream));
            }

            if (string.IsNullOrEmpty(value))
            {
                throw new ArgumentNullException(nameof(value));
            }

            if (encoding == null)
            {
                encoding = Encoding.UTF8;
            }

            var byteData = encoding.GetBytes(value);
            stream.Write(byteData, 0, byteData.Length);
            stream.Close();
        }

        /// <summary>
        /// Refatorar a classe ConsumirAPI ID #170137 - Apenas o envio de DARE está utilizando no momento (30/01/2025)
        /// Antigo EnveloparJSON(), foi implementado na classe de EnvioDARE e EnvioDARELote
        /// </summary>
        protected abstract HttpContent GerarJSON();

        #endregion Public Methods
    }
}
