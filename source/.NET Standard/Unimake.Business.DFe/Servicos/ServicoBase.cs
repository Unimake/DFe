#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml;
using Unimake.Exceptions;
using Unimake.Business.DFe.Validator;
using Unimake.Business.Security;
using System.IO;

namespace Unimake.Business.DFe.Servicos
{
    /// <summary>
    /// Classe base abstrata para elaboração dos serviços dos documentos fiscais eletrônicos (NFe, NFCe, MDFe, NFSe, CTe, GNRE, etc...)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase
    {
        #region Private Fields

        private XmlDocument _conteudoXML;

        #endregion Private Fields

        #region Private Methods

        /// <summary>
        /// Verifica se o XML está assinado, se não estiver assina. Só faz isso para XMLs que tem tag de assinatura, demais ele mantem como está, sem assinar.
        /// </summary>
        /// <param name="tagAssinatura">Tag de assinatura</param>
        /// <param name="tagAtributoID">Tag que detêm o atributo ID</param>
        private void VerificarAssinarXML(string tagAssinatura, string tagAtributoID)
        {
            
                if (!string.IsNullOrWhiteSpace(tagAssinatura) && Configuracoes.NaoAssina == null && Configuracoes.NaoAssina != Configuracoes.TipoAmbiente)
            {
                if (AssinaturaDigital.EstaAssinado(ConteudoXML, tagAssinatura))
                {
                    AjustarXMLAposAssinado();
                }
                else
                {
                    AssinaturaDigital.Assinar(ConteudoXML, tagAssinatura, tagAtributoID, Configuracoes.CertificadoDigital, AlgorithmType.Sha1, true, "Id", true);

                    AjustarXMLAposAssinado();
                }
            }
        }

        #endregion Private Methods

        #region Protected Properties

        /// <summary>
        /// Conteúdo do XML, pode ou não estar assinado. Esta propriedade é utilizada em tempo de processamento.
        /// Utilize as propriedades ConteudoXMLOriginal ou ConteudoXMLAssinado para recuperar o que você deseja fora da classe.
        /// </summary>
        protected XmlDocument ConteudoXML
        {
            get => _conteudoXML;
            set
            {
                if (ConteudoXMLOriginal == null)
                {
                    ConteudoXMLOriginal = new XmlDocument();
                    ConteudoXMLOriginal.LoadXml(value?.OuterXml);
                }

                _conteudoXML = value;
            }
        }

        #endregion Protected Properties

        #region Protected Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        protected ServicoBase() { }

        #endregion Protected Constructors

        #region Protected Methods

        /// <summary>
        /// Este método é uma possibilidade de fazer ajustes no XML depois de assinado, pois ele é executado assim que a assinatura é feita. Basta implementar ele nas heranças.
        /// </summary>
        protected virtual void AjustarXMLAposAssinado() { }

        /// <summary>
        /// Defini o valor das propriedades do objeto "Configuracoes"
        /// </summary>
        protected abstract void DefinirConfiguracao();

        /// <summary>
        /// Validar o schema do XML
        /// </summary>
        protected abstract void XmlValidar();

        /// <summary>
        /// Validar o conteúdo das tags do XML, alguns validações manuais que o schema não faz. Vamos implementando novas regras na medida da necessidade de cada serviço.
        /// </summary>
        protected abstract void XmlValidarConteudo();

        #endregion Protected Methods

        #region Protected Internal Methods

        /// <summary>
        /// Inicializa configurações, parâmetros e propriedades para execução do serviço.
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML a ser enviado para o web-service</param>
        /// <param name="configuracao">Configurações a serem utilizadas para conexão e envio do XML para o web-service</param>
#if INTEROP
        [ComVisible(false)]
#endif
        protected virtual void Inicializar(XmlDocument conteudoXML, Configuracao configuracao)
        {
            Configuracoes = configuracao ?? throw new ArgumentNullException(nameof(configuracao));
            ConteudoXML = conteudoXML ?? throw new ArgumentNullException(nameof(conteudoXML));

            if (!Configuracoes.Definida)
            {
                DefinirConfiguracao();
            }

            //Esta linha tem que ficar fora do if acima, pois tem que carregar esta parte, independente, pois o que é carregado sempre é automático. Mudar isso, vai gerar falha no UNINFE, principalmente no envio dos eventos, onde eu defino as configurações manualmente. Wandrey 07/12/2020
           Configuracoes.Load(GetType().Name);

            System.Diagnostics.Trace.WriteLine(ConteudoXML?.InnerXml, "Unimake.DFe");

            //Forçar criar a tag QrCode bem como assinatura para que o usuário possa acessar o conteúdo no objeto do XML antes de enviar
                _ = ConteudoXMLAssinado;
            }
            
        #endregion Protected Internal Methods

        #region Public Properties

        /// <summary>
        /// Configurações diversas para consumir os serviços
        /// </summary>
        public Configuracao Configuracoes { get; set; }

        /// <summary>
        /// Conteúdo do XML assinado.
        /// </summary>
        public XmlDocument ConteudoXMLAssinado
        {
            get
            {
                VerificarAssinarXML(Configuracoes.TagAssinatura, Configuracoes.TagAtributoID);
                VerificarAssinarXML(Configuracoes.TagLoteAssinatura, Configuracoes.TagLoteAtributoID);

                return ConteudoXML;
            }
        }

#if INTEROP

        /// <summary>
        /// Recupera o conteúdo do XML assinado.
        /// </summary>
        /// <returns>Retorna conteúdo do XML assinado</returns>
        public string GetConteudoXMLAssinado() => (ConteudoXMLAssinado != null ? ConteudoXMLAssinado.OuterXml : "");

        /// <summary>
        /// Recupera o conteúdo do XML original.
        /// </summary>
        /// <returns>Retorna conteúdo do XML original</returns>
        public string GetConteudoXMLOriginal() => (ConteudoXMLOriginal != null ? ConteudoXMLOriginal.OuterXml : "");


#endif

        /// <summary>
        /// Conteúdo do XML original, para os que tem assinatura este está sem. Original conforme foi criado.
        /// </summary>
        public XmlDocument ConteudoXMLOriginal { get; private set; }

        /// <summary>
        /// String do XML retornado pelo WebService
        /// </summary>
        public string RetornoWSString { get; set; }

        /// <summary>
        /// XML retornado pelo Web-service
        /// </summary>
        public XmlDocument RetornoWSXML { get; set; }

        /// <summary>
        /// Stream retornada pelo Webservice. Para consumo de serviços que retornam .pdf
        /// </summary>
        public Stream RetornoStream { get; set; }

        #endregion Public Properties

        #region Public Constructors

        static ServicoBase() => AppDomain.CurrentDomain.AssemblyResolve += AssemblyResolver.AssemblyResolve;

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Executar o serviço para consumir o web-service
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public virtual void Executar()
        {
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

            if (Configuracoes.IsAPI)
            {
                var apiConfig = new APIConfig
                {
                    ContentType = Configuracoes.WebContentType,
                    RequestURI = (Configuracoes.TipoAmbiente == TipoAmbiente.Producao ? Configuracoes.RequestURIProducao : Configuracoes.RequestURIHomologacao),
                    TagRetorno = Configuracoes.WebTagRetorno,
                    GZipCompress = Configuracoes.GZIPCompress,
                    WebSoapString = Configuracoes.WebSoapString,
                    MetodoAPI = Configuracoes.MetodoAPI,
                    Token = Configuracoes.MunicipioToken,
                    WebAction = Configuracoes.WebActionProducao,
                    MunicipioSenha = Configuracoes.MunicipioSenha,
                    MunicipioUsuario = Configuracoes.MunicipioUsuario,
                    PadraoNFSe = Configuracoes.PadraoNFSe,
                    LoginConexao = Configuracoes.LoginConexao,
                    ResponseMediaType = Configuracoes.ResponseMediaType,
                    CodigoTom = Configuracoes.CodigoTom,
                    UsaCertificadoDigital = Configuracoes.UsaCertificadoDigital
                };

                var consumirAPI = new ConsumirAPI();
                consumirAPI.ExecutarServico(ConteudoXML, apiConfig, Configuracoes.CertificadoDigital);

                RetornoWSString = consumirAPI.RetornoServicoString;
                RetornoWSXML = consumirAPI.RetornoServicoXML;
                RetornoStream = consumirAPI.RetornoStream;  //Retorno específico para criação de .pdf para os casos em que a String corrompe o conteúdo. Mauricio 27/09/2023 #157859
            }
            else
            {
                var soap = new WSSoap
                {
                    EnderecoWeb = (Configuracoes.TipoAmbiente == TipoAmbiente.Producao ? Configuracoes.WebEnderecoProducao : Configuracoes.WebEnderecoHomologacao),
                    ActionWeb = (Configuracoes.TipoAmbiente == TipoAmbiente.Producao ? Configuracoes.WebActionProducao : Configuracoes.WebActionHomologacao),
                    TagRetorno = Configuracoes.WebTagRetorno,
                    TagRetornoHomologacao = Configuracoes.WebTagRetornoHomologacao,
                    EncodingRetorno = Configuracoes.WebEncodingRetorno,
                    GZIPCompress = Configuracoes.GZIPCompress,
                    VersaoSoap = Configuracoes.WebSoapVersion,
                    SoapString = Configuracoes.WebSoapString,
                    ContentType = Configuracoes.WebContentType,
                    TimeOutWebServiceConnect = Configuracoes.TimeOutWebServiceConnect,
                    PadraoNFSe = Configuracoes.PadraoNFSe,
                    UsaCertificadoDigital = Configuracoes.UsaCertificadoDigital,
                    TipoAmbiente = Configuracoes.TipoAmbiente,
                    ConverteSenhaBase64 = Configuracoes.ConverteSenhaBase64,
                    MunicipioSenha = Configuracoes.ConverteSenhaBase64 ? Configuracoes.MunicipioSenha.Base64Encode() : Configuracoes.MunicipioSenha,
                    MunicipioUsuario = Configuracoes.MunicipioUsuario,
                    Token = Configuracoes.MunicipioToken,
                    EncriptaTagAssinatura = Configuracoes.EncriptaTagAssinatura,
                    Proxy = (Configuracoes.HasProxy ? Proxy.DefinirServidor(Configuracoes.ProxyAutoDetect,
                                                                            Configuracoes.ProxyUser,
                                                                            Configuracoes.ProxyPassword) : null)
                };

                var consumirWS = new ConsumirWS();
                consumirWS.ExecutarServico(ConteudoXML, soap, Configuracoes.CertificadoDigital);

                RetornoWSString = consumirWS.RetornoServicoString;
                RetornoWSXML = consumirWS.RetornoServicoXML;
            }
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
        public abstract void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML);

        #endregion Public Methods
    }
}