using System;
using System.IO;
using System.Reflection;
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
        private Assembly _assembly = Assembly.GetExecutingAssembly();

        #endregion Private Fields

        #region Private Methods

        /// <summary>
        /// Definir o nome da tag que contem as propriedades de acordo com o serviço que está sendo executado
        /// </summary>
        /// <returns>Nome da tag</returns>
        private string DefinirNomeTag() => GetType().Name;

        /// <summary>
        /// Namespace onde estão contidos os XMLs de configurações embutidos na DLL por tipo de documento (NFe, NFCe, CTe, etc...)
        /// </summary>
        private string NamespaceConfig => Configuration.NamespaceConfig + Configuracoes.TipoDFe.ToString() + ".";

        /// <summary>
        /// Ler conteúdo do arquivo de configurações contido nos recursos da DLL
        /// </summary>
        /// <param name="arquivo">Nome do arquivo que é para ler o conteúdo</param>
        /// <returns>Stream do arquivo de configuração contido nos recursos da DLL</returns>
        private Stream LoadXmlConfig(string arquivo) => _assembly.GetManifestResourceStream(arquivo);

        /// <summary>
        /// Retorna o nome do arquivo de configurações específicas do estado, município, etc...
        /// </summary>
        /// <param name="arqConfig">Arquivo de configuração</param>
        /// <returns>Retorna Namespace + Nome do arquivo de configuração de serviços</returns>
        private string GetConfigFile(string arqConfig) => NamespaceConfig + arqConfig;

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

        /// <summary>
        /// Ler as configurações do XML
        /// </summary>
        /// <param name="doc">Documento XML</param>
        /// <param name="arqConfig">Nome do arquivo de configuração</param>
        private void LerConfig(XmlDocument doc, string arqConfig)
        {
            if(doc.GetElementsByTagName("Servicos")[0] != null)
            {
                LerConfigPadrao();
            }

            var achouConfigVersao = false;

            var listServicos = doc.GetElementsByTagName("Servicos");
            foreach(var nodeServicos in listServicos)
            {
                var elementServicos = (XmlElement)nodeServicos;

                if(elementServicos.GetAttribute("ID") == Configuracoes.TipoDFe.ToString())
                {
                    var nomeTagServico = DefinirNomeTag();

                    var listPropriedades = elementServicos.GetElementsByTagName(nomeTagServico);

                    foreach(var nodePropridades in listPropriedades)
                    {
                        var elementPropriedades = (XmlElement)nodePropridades;
                        if(elementPropriedades.GetAttribute("versao") == Configuracoes.SchemaVersao)
                        {
                            achouConfigVersao = true;

                            if(XMLUtility.TagExist(elementPropriedades, "Descricao"))
                            {
                                Configuracoes.Descricao = XMLUtility.TagRead(elementPropriedades, "Descricao");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "WebActionHomologacao"))
                            {
                                Configuracoes.WebActionHomologacao = XMLUtility.TagRead(elementPropriedades, "WebActionHomologacao");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "WebActionProducao"))
                            {
                                Configuracoes.WebActionProducao = XMLUtility.TagRead(elementPropriedades, "WebActionProducao");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "WebEnderecoHomologacao"))
                            {
                                Configuracoes.WebEnderecoHomologacao = XMLUtility.TagRead(elementPropriedades, "WebEnderecoHomologacao");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "WebEnderecoProducao"))
                            {
                                Configuracoes.WebEnderecoProducao = XMLUtility.TagRead(elementPropriedades, "WebEnderecoProducao");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "WebContentType"))
                            {
                                Configuracoes.WebContentType = XMLUtility.TagRead(elementPropriedades, "WebContentType");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "WebSoapString"))
                            {
                                Configuracoes.WebSoapString = XMLUtility.TagRead(elementPropriedades, "WebSoapString");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "WebSoapVersion"))
                            {
                                Configuracoes.WebSoapVersion = XMLUtility.TagRead(elementPropriedades, "WebSoapVersion");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "WebTagRetorno"))
                            {
                                Configuracoes.WebTagRetorno = XMLUtility.TagRead(elementPropriedades, "WebTagRetorno");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "WebEncodingRetorno"))
                            {
                                Configuracoes.WebEncodingRetorno = XMLUtility.TagRead(elementPropriedades, "WebEncodingRetorno");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "TargetNS"))
                            {
                                Configuracoes.TargetNS = XMLUtility.TagRead(elementPropriedades, "TargetNS");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "SchemaVersao"))
                            {
                                Configuracoes.SchemaVersao = XMLUtility.TagRead(elementPropriedades, "SchemaVersao");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "SchemaArquivo"))
                            {
                                Configuracoes.SchemaArquivo = XMLUtility.TagRead(elementPropriedades, "SchemaArquivo").Replace("{0}", Configuracoes.SchemaVersao);
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "TagAssinatura"))
                            {
                                Configuracoes.TagAssinatura = XMLUtility.TagRead(elementPropriedades, "TagAssinatura");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "TagAtributoID"))
                            {
                                Configuracoes.TagAtributoID = XMLUtility.TagRead(elementPropriedades, "TagAtributoID");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "TagExtraAssinatura"))
                            {
                                Configuracoes.TagExtraAssinatura = XMLUtility.TagRead(elementPropriedades, "TagExtraAssinatura");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "TagExtraAtributoID"))
                            {
                                Configuracoes.TagExtraAtributoID = XMLUtility.TagRead(elementPropriedades, "TagExtraAtributoID");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "TagLoteAssinatura"))
                            {
                                Configuracoes.TagLoteAssinatura = XMLUtility.TagRead(elementPropriedades, "TagLoteAssinatura");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "TagLoteAtributoID"))
                            {
                                Configuracoes.TagLoteAtributoID = XMLUtility.TagRead(elementPropriedades, "TagLoteAtributoID");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "UrlQrCodeHomologacao"))
                            {
                                Configuracoes.UrlQrCodeHomologacao = XMLUtility.TagRead(elementPropriedades, "UrlQrCodeHomologacao");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "UrlQrCodeProducao"))
                            {
                                Configuracoes.UrlQrCodeProducao = XMLUtility.TagRead(elementPropriedades, "UrlQrCodeProducao");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "UrlChaveHomologacao"))
                            {
                                Configuracoes.UrlChaveHomologacao = XMLUtility.TagRead(elementPropriedades, "UrlChaveHomologacao");
                            }

                            if(XMLUtility.TagExist(elementPropriedades, "UrlChaveProducao"))
                            {
                                Configuracoes.UrlChaveProducao = XMLUtility.TagRead(elementPropriedades, "UrlChaveProducao");
                            }

                            //Verificar se existem schemas específicos de validação
                            if(XMLUtility.TagExist(elementPropriedades, "SchemasEspecificos"))
                            {
                                var listSchemasEspecificios = elementPropriedades.GetElementsByTagName("SchemasEspecificos");

                                foreach(var nodeSchemasEspecificos in listSchemasEspecificios)
                                {
                                    var elemenSchemasEspecificos = (XmlElement)nodeSchemasEspecificos;

                                    var listTipo = elemenSchemasEspecificos.GetElementsByTagName("Tipo");

                                    foreach(var nodeTipo in listTipo)
                                    {
                                        var elementTipo = (XmlElement)nodeTipo;
                                        var idSchemaEspecifico = elementTipo.GetElementsByTagName("ID")[0].InnerText;

                                        Configuracoes.SchemasEspecificos[idSchemaEspecifico] = new SchemaEspecifico
                                        {
                                            Id = idSchemaEspecifico,
                                            SchemaArquivo = elementTipo.GetElementsByTagName("SchemaArquivo")[0].InnerText.Replace("{0}", Configuracoes.SchemaVersao),
                                            SchemaArquivoEspecifico = elementTipo.GetElementsByTagName("SchemaArquivoEspecifico")[0].InnerText.Replace("{0}", Configuracoes.SchemaVersao)
                                        };
                                    }
                                }
                            }
                        }
                    }

                    break;
                }
            }

            if (Configuracoes.Servico == Servico.NFeConsultaCadastro)
            {
                //Estados que não disponibilizam a coinsulta cadastro e que usam SVRS, como SVRS tem a consulta mas não para estes estados, tenho que tratar a exceção manualmente, conforma baixo.
                if (Configuracoes.CodigoUF.Equals(14) || Configuracoes.CodigoUF.Equals(16) ||
                    Configuracoes.CodigoUF.Equals(33) || Configuracoes.CodigoUF.Equals(11) ||
                    Configuracoes.CodigoUF.Equals(15) || Configuracoes.CodigoUF.Equals(22) ||
                    Configuracoes.CodigoUF.Equals(27) || Configuracoes.CodigoUF.Equals(18) ||
                    Configuracoes.CodigoUF.Equals(17))
                {
                    throw new Exception(Configuracoes.Nome + " não disponibiliza o serviço de " + Configuracoes.Servico.GetAttributeDescription() + " para o ambiente de "+(Configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? "homologação." : "produção."));
                }
            }

            if(Configuracoes.TipoAmbiente == TipoAmbiente.Homologacao && string.IsNullOrWhiteSpace(Configuracoes.WebEnderecoHomologacao))
            {
                throw new Exception(Configuracoes.Nome + " não disponibiliza o serviço de " + Configuracoes.Servico.GetAttributeDescription() + " para o ambiente de homologação.");
            }
            else if(Configuracoes.TipoAmbiente == TipoAmbiente.Producao && string.IsNullOrWhiteSpace(Configuracoes.WebEnderecoProducao))
            {
                throw new Exception(Configuracoes.Nome + " não disponibiliza o serviço de " + Configuracoes.Servico.GetAttributeDescription() + " para o ambiente de produção.");
            }
            else if(!achouConfigVersao)
            {
                throw new Exception("Não foi localizado as configurações para a versão de schema " + Configuracoes.SchemaVersao + " no arquivo de configuração do serviço de " + Configuracoes.TipoDFe.ToString() + ".\r\n\r\n" + arqConfig);
            }
        }

        private void LerConfigPadrao()
        {
            var arqConfig = NamespaceConfig + Configuration.ArquivoConfigPadrao;

            var xmlDoc = new XmlDocument();

            var stream = LoadXmlConfig(arqConfig);
            if(stream != null)
            {
                xmlDoc.Load(stream);
            }
            else
            {
                throw new System.Exception("Não foi localizado o arquivo de configuração padrão do serviço de " + arqConfig);
            }

            var achouConfigVersao = false;
            var listConfigPadrao = xmlDoc.GetElementsByTagName("ConfigPadrao");

            foreach(var nodeConfigPadrao in listConfigPadrao)
            {
                var elementConfigPadrao = (XmlElement)nodeConfigPadrao;

                var listVersao = xmlDoc.GetElementsByTagName("Versao");

                foreach(var nodeVersao in listVersao)
                {
                    var elementVersao = (XmlElement)nodeVersao;

                    if(elementVersao.GetAttribute("ID") == Configuracoes.SchemaVersao)
                    {
                        achouConfigVersao = true;
                        if(XMLUtility.TagExist(elementVersao, "WebContentType"))
                        {
                            Configuracoes.WebContentType = XMLUtility.TagRead(elementVersao, "WebContentType");
                        }

                        if(XMLUtility.TagExist(elementVersao, "WebSoapString"))
                        {
                            Configuracoes.WebSoapString = XMLUtility.TagRead(elementVersao, "WebSoapString");
                        }

                        if(XMLUtility.TagExist(elementVersao, "WebTagRetorno"))
                        {
                            Configuracoes.WebTagRetorno = XMLUtility.TagRead(elementVersao, "WebTagRetorno");
                        }

                        if(XMLUtility.TagExist(elementVersao, "WebEncodingRetorno"))
                        {
                            Configuracoes.WebEncodingRetorno = XMLUtility.TagRead(elementVersao, "WebEncodingRetorno");
                        }

                        if(XMLUtility.TagExist(elementVersao, "WebSoapVersion"))
                        {
                            Configuracoes.WebSoapVersion = XMLUtility.TagRead(elementVersao, "WebSoapVersion");
                        }

                        if(XMLUtility.TagExist(elementVersao, "SchemaVersao"))
                        {
                            Configuracoes.SchemaVersao = XMLUtility.TagRead(elementVersao, "SchemaVersao");
                        }

                        if(XMLUtility.TagExist(elementVersao, "TargetNS"))
                        {
                            Configuracoes.TargetNS = XMLUtility.TagRead(elementVersao, "TargetNS");
                        }

                        break;
                    }
                }
            }

            if(!achouConfigVersao)
            {
                throw new Exception("Não foi localizado as configurações para a versão de schema " + Configuracoes.SchemaVersao + " no arquivo de configuração padrão do serviço de " + Configuracoes.TipoDFe.ToString() + ".\r\n\r\n" + arqConfig);
            }
        }

        /// <summary>
        /// Efetua a leitura do XML que contem configurações específicas de cada webservice e atribui o conteúdo nas propriedades do objeto "Configuracoes"
        /// </summary>
        private void LerXmlConfigEspecifico(string xmlConfigEspecifico)
        {
            var doc = new XmlDocument();
            doc.Load(LoadXmlConfig(xmlConfigEspecifico));

            #region Leitura do XML do SVC - Sistema Virtual de Contingência

            var svc = false;
            var arqConfigSVC = string.Empty;

            switch(Configuracoes.TipoEmissao)
            {
                case TipoEmissao.ContingenciaSVCRS:
                    svc = true;
                    arqConfigSVC = NamespaceConfig + (Configuracoes.TipoDFe == TipoDFe.NFe ? "SVCRS.xml" : "SVRS.xml");
                    goto default;

                case TipoEmissao.ContingenciaSVCAN:
                    svc = true;
                    arqConfigSVC = NamespaceConfig + "SVCAN.xml";
                    goto default;

                case TipoEmissao.ContingenciaSVCSP:
                    svc = true;
                    arqConfigSVC = NamespaceConfig + "SVSP.xml";
                    goto default;

                default:
                    if(svc)
                    {
                        doc.Load(LoadXmlConfig(arqConfigSVC));
                        LerConfig(doc, arqConfigSVC);
                    }
                    break;
            }

            #endregion

            if(!svc)
            {
                #region Leitura do XML herdado, quando tem herança.

                var temHeranca = false;

                if(doc.GetElementsByTagName("Heranca")[0] != null)
                {
                    var arqConfigHeranca = NamespaceConfig + doc.GetElementsByTagName("Heranca")[0].InnerText;

                    temHeranca = true;

                    doc.Load(LoadXmlConfig(arqConfigHeranca));
                    LerConfig(doc, arqConfigHeranca);

                    doc.Load(LoadXmlConfig(xmlConfigEspecifico));
                }

                #endregion Leitura do XML herdado, quando tem herança.

                try
                {
                    LerConfig(doc, xmlConfigEspecifico);
                }
                catch
                {
                    if(!temHeranca) //Se tem herança pode ser que não encontre configuração específica, então não pode retornar a exceção lançada neste ponto.
                    {
                        throw;
                    }
                }
            }

            SubstituirValorSoapString();
        }

        /// <summary>
        /// Substituir alguns valores do da configuração do SoapString (Configuracao.WebSoapString)
        /// </summary>
        private void SubstituirValorSoapString()
        {
            Configuracoes.WebSoapString = Configuracoes.WebSoapString.Replace("{ActionWeb}", (Configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? Configuracoes.WebActionHomologacao : Configuracoes.WebActionProducao));
            Configuracoes.WebSoapString = Configuracoes.WebSoapString.Replace("{cUF}", Configuracoes.CodigoUF.ToString());
            Configuracoes.WebSoapString = Configuracoes.WebSoapString.Replace("{versaoDados}", Configuracoes.SchemaVersao);
        }

        #endregion Private Methods

        #region Protected Properties

        /// <summary>
        /// Conteúdo do XML, pode ou não estar assinado. Esta propriedade é utilizada em tempo de processamento.
        /// Utilize as propriedades ConteudoXMLOriginal ou ConteudoXMLAssinado para recuperar o que você deseja fora da classe.
        /// </summary>
        protected XmlDocument
            ConteudoXML
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
            LerXmlConfigGeral();
        }

        /// <summary>
        /// Efetua a leitura do XML que contem configurações gerais e atribui o conteúdo nas propriedades do objeto "Configuracoes"
        /// </summary>
        protected internal void LerXmlConfigGeral()
        {
            if(Configuracoes.CodigoConfig != 0)
            {
                var doc = new XmlDocument();
                doc.Load(LoadXmlConfig(Configuration.ArquivoConfigGeral));

                var listConfiguracoes = doc.GetElementsByTagName("Configuracoes");

                foreach(XmlNode nodeConfiguracoes in listConfiguracoes)
                {
                    var elementConfiguracoes = (XmlElement)nodeConfiguracoes;
                    var listArquivos = elementConfiguracoes.GetElementsByTagName("Arquivo");

                    foreach(var nodeArquivos in listArquivos)
                    {
                        var elementArquivos = (XmlElement)nodeArquivos;

                        if(elementArquivos.GetAttribute("ID") != Configuracoes.CodigoConfig.ToString())
                        {
                            continue;
                        }

                        Configuracoes.Nome = elementArquivos.GetElementsByTagName("Nome")[0].InnerText;
                        Configuracoes.NomeUF = elementArquivos.GetElementsByTagName("UF")[0].InnerText;
                        Configuracoes.PadraoNFSe = PadraoNFSe.None;
                        
                        if(XMLUtility.TagExist(elementArquivos, "PadraoNFSe"))
                        {
                            try
                            {
                                Configuracoes.PadraoNFSe = (PadraoNFSe)Enum.Parse(typeof(PadraoNFSe), XMLUtility.TagRead(elementArquivos, "PadraoNFSe"));
                            }
                            catch(Exception)
                            {
                                throw new Exception("Caro desenvolvedor, você esqueceu de definir no enumerador \"PadraoNFSe\" o tipo "+XMLUtility.TagRead(elementArquivos, "PadraoNFSe")+" e eu não tenho como resolver esta encrenca. Por favor, va lá e defina.");
                            }
                        }

                        LerXmlConfigEspecifico(GetConfigFile(elementArquivos.GetElementsByTagName("ArqConfig")[0].InnerText));

                        break;
                    }
                }
            }
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