using System;
using System.Runtime.InteropServices;
using System.Xml;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml;

namespace Unimake.Business.DFe.Servicos
{
    /// <summary>
    /// Classe base abstrata para elaboração dos serviços dos documentos fiscais eletrônicos (NFe, NFCe, MDFe, NFSe, CTe, GNRE, etc...)
    /// </summary>
    [ComVisible(true)]
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
        private void VerificarAssinarXML(string tagAssinatura)
        {
            if(!string.IsNullOrWhiteSpace(tagAssinatura))
            {
                if(AssinaturaDigital.EstaAssinado(ConteudoXML, tagAssinatura))
                {
                    AjustarXMLAposAssinado();
                }
                else
                {
                    AssinaturaDigital.Assinar(ConteudoXML, tagAssinatura, Configuracoes.TagAtributoID, Configuracoes.CertificadoDigital, AlgorithmType.Sha1, true, "Id", true);

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
                if(ConteudoXMLOriginal == null)
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
        protected ServicoBase()
        {
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML a ser enviado para o webservice</param>
        /// <param name="configuracao">Configurações a serem utilizadas para conexão e envio do XML para o webservice</param>
        protected ServicoBase(XmlDocument conteudoXML, Configuracao configuracao)
                    : this() => PrepararServico(conteudoXML, configuracao);

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
        /// Preparar o ambiente para consumir o serviço
        /// </summary>
        /// <param name="conteudoXML">XML que será enviado para o webservice</param>
        /// <param name="configuracao">Configurações que serão utilizadas para conexão e envio do XML para o webservice</param>
        protected void PrepararServico(XmlDocument conteudoXML, Configuracao configuracao)
        {
            Configuracoes = configuracao ?? throw new ArgumentNullException(nameof(configuracao));
            ConteudoXML = conteudoXML ?? throw new ArgumentNullException(nameof(conteudoXML));
            Inicializar();
            System.Diagnostics.Trace.WriteLine(ConteudoXML?.InnerXml, "Unimake.DFe");
        }

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
        /// Inicializa configurações, parmâtros e propriedades para execução do serviço.
        /// </summary>
        [ComVisible(false)]
        protected internal void Inicializar()
        {
            if(!Configuracoes.Definida)
            {
                DefinirConfiguracao();
            }

            //Esta linha tem que ficar fora do if acima, pois tem que carregar esta parte, independente, pois o que é carregado sempre é automático. Mudar isso, vai gerar falha no UNINFE, principalmente no envio dos eventos, onde eu defino as configurações manualmente. Wandrey 07/12/2020
            Configuracoes.Load(GetType().Name);
        }

        #endregion Protected Internal Methods

        #region Public Properties

        /// <summary>
        /// Configurações diversas para consumir os serviços
        /// </summary>
        public Configuracao Configuracoes { get; set; }

        /// <summary>
        /// Conteúdo do XML assinado, quando o mesmo possui assinatura, caso contrário, o conteúdo será o mesmo da propriedade ConteudoXMLOriginal.
        /// </summary>
        public XmlDocument ConteudoXMLAssinado
        {
            get
            {
                VerificarAssinarXML(Configuracoes.TagAssinatura);
                VerificarAssinarXML(Configuracoes.TagLoteAssinatura);

                return ConteudoXML;
            }
        }

        /// <summary>
        /// Conteúdo do XML original, para os que tem assinatura este está sem. Original conforme foi criado.
        /// </summary>
        public XmlDocument ConteudoXMLOriginal { get; private set; }

        /// <summary>
        /// String do XML retornado pelo WebService
        /// </summary>
        public string RetornoWSString { get; set; }

        /// <summary>
        /// XML retornado pelo Webservice
        /// </summary>
        public XmlDocument RetornoWSXML { get; set; }

        #endregion Public Properties

        #region Public Constructors

        static ServicoBase() => AppDomain.CurrentDomain.AssemblyResolve += AssemblyResolver.AssemblyResolve;

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Executar o serviço para consumir o webservice
        /// </summary>
        [ComVisible(false)]
        public virtual void Executar()
        {
            if(!string.IsNullOrWhiteSpace(Configuracoes.TagAssinatura))
            {
                if(!AssinaturaDigital.EstaAssinado(ConteudoXML, Configuracoes.TagAssinatura))
                {
                    AssinaturaDigital.Assinar(ConteudoXML, Configuracoes.TagAssinatura, Configuracoes.TagAtributoID, Configuracoes.CertificadoDigital, AlgorithmType.Sha1, true, "Id");
                    AjustarXMLAposAssinado();
                }
            }

            var soap = new WSSoap
            {
                EnderecoWeb = (Configuracoes.TipoAmbiente == TipoAmbiente.Producao ? Configuracoes.WebEnderecoProducao : Configuracoes.WebEnderecoHomologacao),
                ActionWeb = (Configuracoes.TipoAmbiente == TipoAmbiente.Producao ? Configuracoes.WebActionProducao : Configuracoes.WebActionHomologacao),
                TagRetorno = Configuracoes.WebTagRetorno,
                EncodingRetorno = Configuracoes.WebEncodingRetorno,
                VersaoSoap = Configuracoes.WebSoapVersion,
                SoapString = Configuracoes.WebSoapString,
                ContentType = Configuracoes.WebContentType,
                Proxy = (Configuracoes.HasProxy ? Proxy.DefinirServidor(Configuracoes.ProxyAutoDetect,
                                                                        Configuracoes.ProxyUser,
                                                                        Configuracoes.ProxyPassword) : null)
            };

            var consumirWS = new ConsumirWS();
            consumirWS.ExecutarServico(ConteudoXML, soap, Configuracoes.CertificadoDigital);

            RetornoWSString = consumirWS.RetornoServicoString;
            RetornoWSXML = consumirWS.RetornoServicoXML;
        }

        /// <summary>
        /// Gravar o XML de distribuição em uma pasta no HD
        /// </summary>
        /// <param name="pasta">Pasta onde deve ser gravado o XML no HD</param>
        /// <param name="nomeArquivo">Nome do arquivo a ser gravado no HD</param>
        /// <param name="conteudoXML">String contendo o conteúdo do XML a ser gravado no HD</param>
        [ComVisible(false)]
        public abstract void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML);

        #endregion Public Methods
    }
}