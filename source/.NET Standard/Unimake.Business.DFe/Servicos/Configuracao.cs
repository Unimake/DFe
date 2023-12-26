#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Servicos
{
    /// <summary>
    /// Classe das configurações para conexão e envio dos XMLs para os webservices
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.Configuracao")]
    [ComVisible(true)]
#endif
    public class Configuracao
    {
        #region Private Fields

        private X509Certificate2 _certificadoDigital;
        private readonly Assembly _assembly = Assembly.GetExecutingAssembly();

        #endregion Private Fields

        #region Private Properties

        /// <summary>
        /// Namespace onde estão contidos os XMLs de configurações embutidos na DLL por tipo de documento (NFe, NFCe, CTe, etc...)
        /// </summary>
        private string NamespaceConfig => Configuration.NamespaceConfig + TipoDFe.ToString() + ".";

        /// <summary>
        /// Nome da tag que contem as propriedades de acordo com o serviço que está sendo executado
        /// </summary>
        private string NomeTagServico { get; set; }

        #endregion 

        #region Private Methods

        /// <summary>
        /// Carregar certificado digital direto do arquivo .PFX
        /// </summary>
        /// <returns>Objeto do certificado digital</returns>
        private X509Certificate2 GetX509Certificate()
        {
            //Se já tem certificado digital informado
            if (_certificadoDigital != null)
            {
                return _certificadoDigital;
            }

            #region Tenta carregar certificado A1 apontando direto o caminho do .PFX

            if (!string.IsNullOrWhiteSpace(CertificadoArquivo) && !string.IsNullOrWhiteSpace(CertificadoSenha))
            {
                var fi = new FileInfo(CertificadoArquivo);
                _certificadoDigital = new X509Certificate2();

                using (var fs = fi.OpenRead())
                {
                    var buffer = new byte[fs.Length];
                    fs.Read(buffer, 0, buffer.Length);
                    _certificadoDigital = new X509Certificate2(buffer, CertificadoSenha);
                }
            }

            #endregion

            #region Tenta carregar o certificado A1 de um base64

            else if (!string.IsNullOrWhiteSpace(CertificadoBase64) && !string.IsNullOrWhiteSpace(CertificadoSenha))
            {
                var buffer = Convert.FromBase64String(CertificadoBase64);

                _certificadoDigital = new X509Certificate2(buffer, CertificadoSenha);
            }

            #endregion

            #region Tenta encontrar o certificado digital no repositório do windows via SerialNumber ou Thumbprint

            else if (!string.IsNullOrWhiteSpace(CertificadoSerialNumberOrThumbPrint))
            {
                var store = new X509Store("MY", StoreLocation.CurrentUser);
                store.Open(OpenFlags.ReadOnly | OpenFlags.OpenExistingOnly);
                var collection = store.Certificates;
                var collection1 = collection.Find(X509FindType.FindByTimeValid, DateTime.Now, false);
                var collection2 = collection1.Find(X509FindType.FindByKeyUsage, X509KeyUsageFlags.DigitalSignature, false);

                //Primeiro tento encontrar pelo thumbprint
                var collection3 = collection2.Find(X509FindType.FindByThumbprint, CertificadoSerialNumberOrThumbPrint, false);
                if (collection3.Count > 0)
                {
                    _certificadoDigital = collection3[0];
                }
                else
                {
                    //Se não encontrou pelo thumbprint tento pelo SerialNumber pegando o mesmo thumbprint que veio no arquivo de configurações para ver se não encontro.
                    collection3 = collection2.Find(X509FindType.FindBySerialNumber, CertificadoSerialNumberOrThumbPrint, false);

                    if (collection3.Count > 0)
                    {
                        _certificadoDigital = collection3[0];
                    }
                }
            }

            #endregion

            return _certificadoDigital;
        }

        /// <summary>
        /// Retorna o nome do arquivo de configurações específicas do estado, município, etc...
        /// </summary>
        /// <param name="arqConfig">Arquivo de configuração</param>
        /// <returns>Retorna Namespace + Nome do arquivo de configuração de serviços</returns>
        private string GetConfigFile(string arqConfig) => NamespaceConfig + arqConfig;

        /// <summary>
        /// Ler conteúdo do arquivo de configurações contido nos recursos da DLL
        /// </summary>
        /// <param name="arquivo">Nome do arquivo que é para ler o conteúdo</param>
        /// <returns>Stream do arquivo de configuração contido nos recursos da DLL</returns>
        private Stream LoadXmlConfig(string arquivo) => _assembly.GetManifestResourceStream(arquivo);

        /// <summary>
        /// Ler as configurações do XML
        /// </summary>
        /// <param name="doc">Documento XML</param>
        /// <param name="arqConfig">Nome do arquivo de configuração</param>
        /// <param name="lerConfigPadrao">Efetua a leitura do XML que contem as configurações padrões?</param>
        private void LerConfig(XmlDocument doc, string arqConfig, bool lerConfigPadrao)
        {
            if (lerConfigPadrao && doc.GetElementsByTagName("Servicos")[0] != null)
            {
                LerConfigPadrao();
            }

            var achouConfigVersao = false;

            var listServicos = doc.GetElementsByTagName("Servicos");
            foreach (var nodeServicos in listServicos)
            {
                var elementServicos = (XmlElement)nodeServicos;

                if (elementServicos.GetAttribute("ID") == TipoDFe.ToString())
                {
                    var listPropriedades = elementServicos.GetElementsByTagName(NomeTagServico);

                    foreach (var nodePropridades in listPropriedades)
                    {
                        var elementPropriedades = (XmlElement)nodePropridades;
                        if (elementPropriedades.GetAttribute("versao") == SchemaVersao)
                        {
                            achouConfigVersao = true;

                            if (XMLUtility.TagExist(elementPropriedades, "Descricao"))
                            {
                                Descricao = XMLUtility.TagRead(elementPropriedades, "Descricao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "WebActionHomologacao"))
                            {
                                WebActionHomologacao = XMLUtility.TagRead(elementPropriedades, "WebActionHomologacao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "WebActionProducao"))
                            {
                                WebActionProducao = XMLUtility.TagRead(elementPropriedades, "WebActionProducao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "WebEnderecoHomologacao"))
                            {
                                WebEnderecoHomologacao = XMLUtility.TagRead(elementPropriedades, "WebEnderecoHomologacao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "WebEnderecoProducao"))
                            {
                                WebEnderecoProducao = XMLUtility.TagRead(elementPropriedades, "WebEnderecoProducao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "WebContentType"))
                            {
                                WebContentType = XMLUtility.TagRead(elementPropriedades, "WebContentType");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "WebSoapString"))
                            {
                                WebSoapString = XMLUtility.TagRead(elementPropriedades, "WebSoapString");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "WebSoapStringProducao"))
                            {
                                WebSoapStringProducao = XMLUtility.TagRead(elementPropriedades, "WebSoapStringProducao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "WebSoapStringHomologacao"))
                            {
                                WebSoapStringHomologacao = XMLUtility.TagRead(elementPropriedades, "WebSoapStringHomologacao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "GZIPCompress"))
                            {
                                GZIPCompress = (XMLUtility.TagRead(elementPropriedades, "GZIPCompress").ToLower() == "true" ? true : false);
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "WebSoapVersion"))
                            {
                                WebSoapVersion = XMLUtility.TagRead(elementPropriedades, "WebSoapVersion");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "WebTagRetorno"))
                            {
                                WebTagRetorno = XMLUtility.TagRead(elementPropriedades, "WebTagRetorno");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "WebTagRetornoHomologacao"))
                            {
                                WebTagRetornoHomologacao = XMLUtility.TagRead(elementPropriedades, "WebTagRetornoHomologacao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "WebEncodingRetorno"))
                            {
                                WebEncodingRetorno = XMLUtility.TagRead(elementPropriedades, "WebEncodingRetorno");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "TargetNS"))
                            {
                                TargetNS = XMLUtility.TagRead(elementPropriedades, "TargetNS");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "SchemaVersao"))
                            {
                                SchemaVersao = XMLUtility.TagRead(elementPropriedades, "SchemaVersao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "SchemaArquivo"))
                            {
                                SchemaArquivo = XMLUtility.TagRead(elementPropriedades, "SchemaArquivo").Replace("{0}", SchemaVersao);
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "TagAssinatura"))
                            {
                                TagAssinatura = XMLUtility.TagRead(elementPropriedades, "TagAssinatura");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "TagAtributoID"))
                            {
                                TagAtributoID = XMLUtility.TagRead(elementPropriedades, "TagAtributoID");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "TagExtraAssinatura"))
                            {
                                TagExtraAssinatura = XMLUtility.TagRead(elementPropriedades, "TagExtraAssinatura");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "TagExtraAtributoID"))
                            {
                                TagExtraAtributoID = XMLUtility.TagRead(elementPropriedades, "TagExtraAtributoID");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "TagLoteAssinatura"))
                            {
                                TagLoteAssinatura = XMLUtility.TagRead(elementPropriedades, "TagLoteAssinatura");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "TagLoteAtributoID"))
                            {
                                TagLoteAtributoID = XMLUtility.TagRead(elementPropriedades, "TagLoteAtributoID");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "UrlQrCodeHomologacao"))
                            {
                                UrlQrCodeHomologacao = XMLUtility.TagRead(elementPropriedades, "UrlQrCodeHomologacao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "UrlQrCodeProducao"))
                            {
                                UrlQrCodeProducao = XMLUtility.TagRead(elementPropriedades, "UrlQrCodeProducao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "UrlChaveHomologacao"))
                            {
                                UrlChaveHomologacao = XMLUtility.TagRead(elementPropriedades, "UrlChaveHomologacao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "UrlChaveProducao"))
                            {
                                UrlChaveProducao = XMLUtility.TagRead(elementPropriedades, "UrlChaveProducao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "RequestURIProducao"))
                            {
                                RequestURIProducao = XMLUtility.TagRead(elementPropriedades, "RequestURIProducao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "RequestURIHomologacao"))
                            {
                                RequestURIHomologacao = XMLUtility.TagRead(elementPropriedades, "RequestURIHomologacao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "MetodoAPI"))
                            {
                                MetodoAPI = XMLUtility.TagRead(elementPropriedades, "MetodoAPI");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "LoginConexao"))
                            {
                                LoginConexao = XMLUtility.TagRead(elementPropriedades, "LoginConexao").ToLower() == "true" ? true : false;
                            }
                            if (XMLUtility.TagExist(elementPropriedades, "ResponseMediaType"))
                            {
                                ResponseMediaType = XMLUtility.TagRead(elementPropriedades, "ResponseMediaType");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "CodigoTom"))
                            {
                                CodigoTom = XMLUtility.TagRead(elementPropriedades, "CodigoTom");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "UsaCertificadoDigital"))
                            {
                                UsaCertificadoDigital = XMLUtility.TagRead(elementPropriedades, "UsaCertificadoDigital").ToLower() == "true" ? true : false;
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "ConverteSenhaBase64"))
                            {
                                ConverteSenhaBase64 = XMLUtility.TagRead(elementPropriedades, "ConverteSenhaBase64").ToLower() == "true" ? true : false;
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "NaoAssina"))
                            {
                                NaoAssina = XMLUtility.TagRead(elementPropriedades, "NaoAssina").ToLower() == "homologação" ? TipoAmbiente.Homologacao : TipoAmbiente.Producao;
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "EncriptaTagAssinatura"))
                            {
                                EncriptaTagAssinatura = XMLUtility.TagRead(elementPropriedades, "EncriptaTagAssinatura").ToLower() == "true" ? true : false;
                            }

                            //Verificar se existem schemas específicos de validação
                            if (XMLUtility.TagExist(elementPropriedades, "SchemasEspecificos"))
                            {
                                var listSchemasEspecificios = elementPropriedades.GetElementsByTagName("SchemasEspecificos");

                                foreach (var nodeSchemasEspecificos in listSchemasEspecificios)
                                {
                                    var elemenSchemasEspecificos = (XmlElement)nodeSchemasEspecificos;

                                    var listTipo = elemenSchemasEspecificos.GetElementsByTagName("Tipo");

                                    foreach (var nodeTipo in listTipo)
                                    {
                                        var elementTipo = (XmlElement)nodeTipo;
                                        var idSchemaEspecifico = elementTipo.GetElementsByTagName("ID")[0].InnerText;

                                        SchemasEspecificos[idSchemaEspecifico] = new SchemaEspecifico
                                        {
                                            Id = idSchemaEspecifico,
                                            SchemaArquivo = elementTipo.GetElementsByTagName("SchemaArquivo")[0].InnerText.Replace("{0}", SchemaVersao),
                                            SchemaArquivoEspecifico = elementTipo.GetElementsByTagName("SchemaArquivoEspecifico")[0].InnerText.Replace("{0}", SchemaVersao)
                                        };
                                    }
                                }
                            }
                        }
                    }

                    break;
                }
            }

            if (!achouConfigVersao)
            {
                throw new Exception("Não foi localizado as configurações para a versão de schema " + SchemaVersao + " no arquivo de configuração do serviço de " + TipoDFe.ToString() + ".\r\n\r\n" + arqConfig);
            }
            else
            {
                if (Servico == Servico.NFeConsultaCadastro)
                {
                    //Estados que não disponibilizam a coinsulta cadastro e que usam SVRS, como SVRS tem a consulta mas não para estes estados, tenho que tratar a exceção manualmente, conforma baixo.
                    if (CodigoUF.Equals(14) || CodigoUF.Equals(16) ||
                        CodigoUF.Equals(33) || CodigoUF.Equals(11) ||
                        CodigoUF.Equals(15) || CodigoUF.Equals(22) ||
                        CodigoUF.Equals(27) || CodigoUF.Equals(18) ||
                        CodigoUF.Equals(17))
                    {
                        throw new Exception(Nome + " não disponibiliza o serviço de " + Servico.GetAttributeDescription() + " para o ambiente de " + (TipoAmbiente == TipoAmbiente.Homologacao ? "homologação." : "produção."));
                    }
                }
                else if (TipoAmbiente == TipoAmbiente.Homologacao && (string.IsNullOrWhiteSpace(WebEnderecoHomologacao) && string.IsNullOrWhiteSpace(RequestURIHomologacao)))
                {
                    throw new Exception(Nome + " não disponibiliza o serviço de " + Servico.GetAttributeDescription() + " para o ambiente de homologação.");
                }
                else if (TipoAmbiente == TipoAmbiente.Producao && (string.IsNullOrWhiteSpace(WebEnderecoProducao) && string.IsNullOrWhiteSpace(RequestURIProducao)))
                {
                    throw new Exception(Nome + " não disponibiliza o serviço de " + Servico.GetAttributeDescription() + " para o ambiente de produção.");
                }
            }
        }

        /// <summary>
        /// Ler configurações específicas para sobrepor algumas no caso de estar emitindo em contingência. Exemplo: Carrega a contingência SVCRS e tenho que sobrepor as URLs de QRCode com as do estado, para os que tem.
        /// </summary>
        /// <param name="doc">Documento XML</param>
        private void LerConfigEspecifica(XmlDocument doc)
        {
            var listServicos = doc.GetElementsByTagName("Servicos");
            foreach (var nodeServicos in listServicos)
            {
                var elementServicos = (XmlElement)nodeServicos;

                if (elementServicos.GetAttribute("ID") == TipoDFe.ToString())
                {
                    var listPropriedades = elementServicos.GetElementsByTagName(NomeTagServico);

                    foreach (var nodePropridades in listPropriedades)
                    {
                        var elementPropriedades = (XmlElement)nodePropridades;
                        if (elementPropriedades.GetAttribute("versao") == SchemaVersao)
                        {
                            if (XMLUtility.TagExist(elementPropriedades, "UrlQrCodeHomologacao"))
                            {
                                UrlQrCodeHomologacao = XMLUtility.TagRead(elementPropriedades, "UrlQrCodeHomologacao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "UrlQrCodeProducao"))
                            {
                                UrlQrCodeProducao = XMLUtility.TagRead(elementPropriedades, "UrlQrCodeProducao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "UrlChaveHomologacao"))
                            {
                                UrlChaveHomologacao = XMLUtility.TagRead(elementPropriedades, "UrlChaveHomologacao");
                            }

                            if (XMLUtility.TagExist(elementPropriedades, "UrlChaveProducao"))
                            {
                                UrlChaveProducao = XMLUtility.TagRead(elementPropriedades, "UrlChaveProducao");
                            }
                        }
                    }

                    break;
                }
            }
        }

        /// <summary>
        /// Busca o arquivo de configuração específica da SEFAZ, Receita Federal ou Prefeitura. Analisa se é para pegar o arquivo de configuração embutido na DLL ou um que esteja em uma pasta.
        /// </summary>
        /// <param name="xmlConfigEspecifico">Nome do XML de configuração específica embutido na DLL</param>
        /// <returns>XML de configuração</returns>
        private XmlDocument BuscarArquivoConfiguracao(string xmlConfigEspecifico)
        {
            var doc = new XmlDocument();

            switch (BuscarConfiguracaoPastaBase)
            {
                case true:
                    var arqConfig = Path.Combine(PastaArquivoConfiguracao, xmlConfigEspecifico.Substring(NamespaceConfig.Length));

                    if (File.Exists(arqConfig))
                    {
                        doc.Load(arqConfig);

                        if (doc.GetElementsByTagName("VersaoConfiguracao").Count > 0)
                        {
                            VersaoConfiguracao = doc.GetElementsByTagName("VersaoConfiguracao")[0].InnerText;
                        }

                        //Versão da DLL é maior que a versão do arquivo de configuração, então o que tem na DLL é mais atual do que o arquivo que está gravado na pasta, vou ignorar.
                        if (Convert.ToDecimal(Info.VersaoDLL) >= Convert.ToDecimal(VersaoConfiguracao))
                        {
                            goto default;
                        }
                    }
                    else
                    {
                        goto default;
                    }

                    break;

                default:
                    doc.Load(LoadXmlConfig(xmlConfigEspecifico));
                    break;
            }

            return doc;
        }

        /// <summary>
        /// Efetua a leitura do XML que contem configurações específicas de cada web-service e atribui o conteúdo nas propriedades do objeto "Configuracoes"
        /// </summary>
        /// <param name="xmlConfigEspecifico">Nome do XML de configuração específica embutido na DLL</param>
        private void LerXmlConfigEspecifico(string xmlConfigEspecifico)
        {
            var doc = BuscarArquivoConfiguracao(xmlConfigEspecifico);

            #region Leitura do XML do SVC - Sistema Virtual de Contingência

            var svc = false;
            var arqConfigSVC = string.Empty;

            switch (TipoEmissao)
            {
                case TipoEmissao.ContingenciaSVCRS:
                    svc = true;
                    arqConfigSVC = NamespaceConfig + (TipoDFe == TipoDFe.NFe ? "SVCRS.xml" : "SVRS.xml");
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
                    if (svc)
                    {
                        doc = BuscarArquivoConfiguracao(arqConfigSVC);
                        LerConfig(doc, arqConfigSVC, true);

                        //Sobrepor algumas configurações do SVC com os do estado, tais como a URL de QRCode. Isso para os estados que tem.
                        doc = BuscarArquivoConfiguracao(xmlConfigEspecifico);
                        LerConfigEspecifica(doc);
                    }
                    break;
            }

            #endregion

            if (!svc)
            {
                #region Leitura do XML herdado, quando tem herança.

                var temHeranca = false;

                if (doc.GetElementsByTagName("Heranca")[0] != null)
                {
                    var arqConfigHeranca = NamespaceConfig + doc.GetElementsByTagName("Heranca")[0].InnerText;

                    temHeranca = true;

                    doc = BuscarArquivoConfiguracao(arqConfigHeranca);
                    LerConfig(doc, arqConfigHeranca, true);

                    doc = BuscarArquivoConfiguracao(xmlConfigEspecifico);
                }

                #endregion Leitura do XML herdado, quando tem herança.

                try
                {
                    LerConfig(doc, xmlConfigEspecifico, !temHeranca);
                }
                catch
                {
                    if (!temHeranca) //Se tem herança pode ser que não encontre configuração específica, então não pode retornar a exceção lançada neste ponto.
                    {
                        throw;
                    }
                }
            }

            SubstituirValorPropriedadeVariavel();
        }

        /// <summary>
        /// Substituir alguns valores de algumas propriedades que possuem variáveis
        /// </summary>
        private void SubstituirValorPropriedadeVariavel()
        {
            if (!string.IsNullOrWhiteSpace(MunicipioToken) && !string.IsNullOrEmpty(WebEnderecoHomologacao))
            {
                WebEnderecoHomologacao = WebEnderecoHomologacao.Replace("{MunicipioToken}", MunicipioToken);
            }
            else if (!string.IsNullOrWhiteSpace(MunicipioToken) && !string.IsNullOrEmpty(RequestURIHomologacao))
            {
                RequestURIHomologacao = RequestURIHomologacao.Replace("{MunicipioToken}", MunicipioToken);
            }

            if (!string.IsNullOrWhiteSpace(MunicipioToken) && !string.IsNullOrEmpty(WebEnderecoProducao))
            {
                WebEnderecoProducao = WebEnderecoProducao.Replace("{MunicipioToken}", MunicipioToken);
            }
            else if (!string.IsNullOrWhiteSpace(MunicipioToken) && !string.IsNullOrEmpty(RequestURIProducao))
            {
                RequestURIProducao = RequestURIProducao.Replace("{MunicipioToken}", MunicipioToken);
            }

            if (!string.IsNullOrWhiteSpace(TokenSoap))
            {
                WebSoapString = WebSoapString.Replace("{TokenSoap}", TokenSoap);
            }

            if (!string.IsNullOrWhiteSpace(CodigoMunicipio.ToString()))
            {
                WebSoapString = WebSoapString.Replace("{CodigoMunicipio}", CodigoMunicipio.ToString());
            }

            if (!string.IsNullOrWhiteSpace(MunicipioUsuario))
            {
                WebSoapString = WebSoapString.Replace("{MunicipioUsuario}", MunicipioUsuario);
            }

            if (!string.IsNullOrWhiteSpace(MunicipioSenha))
            {
                if (ConverteSenhaBase64)
                {
                    WebSoapString = WebSoapString.Replace("{MunicipioSenha}", MunicipioSenha.Base64Encode());
                }
                else
                {
                    WebSoapString = WebSoapString.Replace("{MunicipioSenha}", MunicipioSenha);
                }
            }

            if (TipoAmbiente == TipoAmbiente.Homologacao)
            {
                WebSoapString = WebSoapStringHomologacao;
            }
            else
            {
                WebSoapString = WebSoapStringProducao;
            }

            //Antiga implementação para uso do padrão AGILI (Rondonópolis) que acabou não sendo necessária no momento da implementação do município. Ticket ID #159383. Mauricio 05/12/2023
            //if (!string.IsNullOrEmpty(ClientSecret) && !string.IsNullOrEmpty(ClientID))
            //{
            //    var proxy = Proxy.DefinirServidor(ProxyAutoDetect, ProxyUser, ProxyPassword);
            //    var token =  Token.GerarToken(proxy, MunicipioUsuario, MunicipioSenha, ClientID, ClientSecret);
            //    MunicipioToken = token.AccessToken;
            //}

            WebSoapString = WebSoapString.Replace("{ActionWeb}", (TipoAmbiente == TipoAmbiente.Homologacao ? WebActionHomologacao : WebActionProducao));
            WebSoapString = WebSoapString.Replace("{cUF}", CodigoUF.ToString());
            WebSoapString = WebSoapString.Replace("{versaoDados}", SchemaVersao);
        }

        /// <summary>
        /// Ler as informações do XML de configurações padrões
        /// </summary>
        private void LerConfigPadrao()
        {
            var arqConfig = NamespaceConfig + Configuration.ArquivoConfigPadrao;

            var xmlDoc = new XmlDocument();

            var stream = LoadXmlConfig(arqConfig);
            if (stream != null)
            {
                xmlDoc.Load(stream);
            }
            else
            {
                throw new System.Exception("Não foi localizado o arquivo de configuração padrão do serviço de " + arqConfig);
            }

            var achouConfigVersao = false;
            var listConfigPadrao = xmlDoc.GetElementsByTagName("ConfigPadrao");

            foreach (var nodeConfigPadrao in listConfigPadrao)
            {
                var elementConfigPadrao = (XmlElement)nodeConfigPadrao;

                var listVersao = xmlDoc.GetElementsByTagName("Versao");

                foreach (var nodeVersao in listVersao)
                {
                    var elementVersao = (XmlElement)nodeVersao;

                    if (elementVersao.GetAttribute("ID") == SchemaVersao)
                    {
                        achouConfigVersao = true;
                        if (XMLUtility.TagExist(elementVersao, "WebContentType"))
                        {
                            WebContentType = XMLUtility.TagRead(elementVersao, "WebContentType");
                        }

                        if (XMLUtility.TagExist(elementVersao, "WebSoapString"))
                        {
                            WebSoapString = XMLUtility.TagRead(elementVersao, "WebSoapString");
                        }

                        if (XMLUtility.TagExist(elementVersao, "WebSoapStringProducao"))
                        {
                            WebSoapStringProducao = XMLUtility.TagRead(elementVersao, "WebSoapStringProducao");
                        }

                        if (XMLUtility.TagExist(elementVersao, "WebSoapStringHomologacao"))
                        {
                            WebSoapStringHomologacao = XMLUtility.TagRead(elementVersao, "WebSoapStringHomologacao");
                        }

                        if (XMLUtility.TagExist(elementVersao, "GZIPCompress"))
                        {
                            GZIPCompress = (XMLUtility.TagRead(elementVersao, "GZIPCompress").ToLower() == "true" ? true : false);
                        }

                        if (XMLUtility.TagExist(elementVersao, "WebTagRetorno"))
                        {
                            WebTagRetorno = XMLUtility.TagRead(elementVersao, "WebTagRetorno");
                        }

                        if (XMLUtility.TagExist(elementVersao, "WebEncodingRetorno"))
                        {
                            WebEncodingRetorno = XMLUtility.TagRead(elementVersao, "WebEncodingRetorno");
                        }

                        if (XMLUtility.TagExist(elementVersao, "WebSoapVersion"))
                        {
                            WebSoapVersion = XMLUtility.TagRead(elementVersao, "WebSoapVersion");
                        }

                        if (XMLUtility.TagExist(elementVersao, "SchemaVersao"))
                        {
                            SchemaVersao = XMLUtility.TagRead(elementVersao, "SchemaVersao");
                        }

                        if (XMLUtility.TagExist(elementVersao, "TargetNS"))
                        {
                            TargetNS = XMLUtility.TagRead(elementVersao, "TargetNS");
                        }

                        if (XMLUtility.TagExist(elementVersao, "RequestURIProducao"))
                        {
                            RequestURIProducao = XMLUtility.TagRead(elementVersao, "RequestURIProducao");
                        }

                        if (XMLUtility.TagExist(elementVersao, "RequestURIHomologacao"))
                        {
                            RequestURIHomologacao = XMLUtility.TagRead(elementVersao, "RequestURIHomologacao");
                        }

                        if (XMLUtility.TagExist(elementVersao, "MetodoAPI"))
                        {
                            MetodoAPI = XMLUtility.TagRead(elementVersao, "MetodoAPI");
                        }

                        //Vou limpar a versão da configuração, caso alguém informe algo no arquivo padrão, tem que zerar, pois só pode existir valor na configuração específica de cada UF ou Município
                        VersaoConfiguracao = "";

                        break;
                    }
                }
            }

            if (!achouConfigVersao)
            {
                throw new Exception("Não foi localizado as configurações para a versão de schema " + SchemaVersao + " no arquivo de configuração padrão do serviço de " + TipoDFe.ToString() + ".\r\n\r\n" + arqConfig);
            }
        }

        #endregion Private Methods

        #region Public Fields

        /// <summary>
        /// Schemas específicos de um mesmo serviço (Tipos de Evento, Modal CTe ou Modal MDFe)
        /// </summary>
        public Dictionary<string, SchemaEspecifico> SchemasEspecificos = new Dictionary<string, SchemaEspecifico>();

        #endregion Public Fields

        #region Public Properties

        /// <summary>
        /// SerialNumber ou ThumbPrint do Certificado Digital
        /// </summary>
        public string CertificadoSerialNumberOrThumbPrint { get; set; }

        /// <summary>
        /// BASE64 do certificado digital A1
        /// </summary>
        public string CertificadoBase64 { get; set; }

        /// <summary>
        /// Caminho completo do certificado digital A1
        /// </summary>
        public string CertificadoArquivo { get; set; }

        /// <summary>
        /// Senha de instalação/uso do certificado digital A1
        /// </summary>
        public string CertificadoSenha { get; set; }

        /// <summary>
        /// Certificado digital
        /// </summary>
        public X509Certificate2 CertificadoDigital
        {
            get => GetX509Certificate();
            set => _certificadoDigital = value;
        }

        /// <summary>
        /// Código da configuração
        /// </summary>
        public int CodigoConfig
        {
            get
            {
                var codigo = (CodigoUF != 0 ? CodigoUF : CodigoMunicipio);
                return codigo;
            }
        }

        /// <summary>
        /// Código da Unidade Federativa (UF)
        /// </summary>
        public int CodigoUF { get; set; }

        /// <summary>
        /// Código do IBGE do Município (Utilizando no envio de NFSe)
        /// </summary>
        public int CodigoMunicipio { get; set; }

        /// <summary>
        /// Padrão da NFSe
        /// </summary>
        public PadraoNFSe PadraoNFSe { get; set; }

        /// <summary>
        /// CSC = Código de segurança do contribuinte. Utilizado para criar o QRCode da NFCe
        /// </summary>
        public string CSC { get; set; }

        /// <summary>
        /// IDToken do CSC (Código de segurança do contribuinte). Utilizado para criar o QRCode da NFCe
        /// </summary>
        public int CSCIDToken { get; set; }

        /// <summary>
        /// Configuração já foi definida anteriormente, não precisa carregar de acordo com os dados do XML
        /// </summary>
        public bool Definida { get; set; }

        /// <summary>
        /// Descrição do serviço
        /// </summary>
        public string Descricao { get; set; }

        /// <summary>
        /// Tem servidor de proxy?
        /// </summary>
        public bool HasProxy { get; set; } = false;

        /// <summary>
        /// Modelo do documento fiscal que é para consultar o status do serviço
        /// </summary>
        public ModeloDFe Modelo { get; set; }

        /// <summary>
        /// Nome do estado ou município
        /// </summary>
        public string Nome { get; set; }

        /// <summary>
        /// Nome da Unidade Federativa (UF)
        /// </summary>
        public string NomeUF { get; set; }

        /// <summary>
        /// True = Detectar o servidor de proxy automaticamente
        /// False = Utiliza os dados de Proxy Default
        /// </summary>
        public bool ProxyAutoDetect { get; set; } = false;

        /// <summary>
        /// Senha do usuário para conexão do servidor de proxy
        /// </summary>
        public string ProxyPassword { get; set; }

        /// <summary>
        /// Usuário para conexão do servidor de proxy
        /// </summary>
        public string ProxyUser { get; set; }

        /// <summary>
        /// Nome do arquivo de schema para validação do XML
        /// </summary>
        public string SchemaArquivo { get; set; }

        /// <summary>
        /// Versão do schema do XML
        /// </summary>
        public string SchemaVersao { get; set; }

        /// <summary>
        /// Serviço que será executado
        /// </summary>
        public Servico Servico { get; set; }

        /// <summary>
        /// Nome da tag de Assinatura do XML
        /// </summary>
        public string TagAssinatura { get; set; }

        /// <summary>
        /// Nome da tag que tem o atributo de identificador único a ser utilizado no Reference.URI da assinatura
        /// </summary>
        public string TagAtributoID { get; set; }

        /// <summary>
        /// Nome da tag de Assinatura do XML, quando tem lote (Exemplo: Uma lote com várias NFe ou NFSe)
        /// </summary>
        public string TagLoteAssinatura { get; set; }

        /// <summary>
        /// Nome da tag que tem o atributo de identificador único a ser utilizado no Reference.URI da assinatura, quando tem lote (Exemplo: Uma lote com várias NFe ou NFSe)
        /// </summary>
        public string TagLoteAtributoID { get; set; }

        /// <summary>
        /// Nome da tag de Assinatura do XML, quando tiver uma terceira tag para assinar (É o caso da Substituição da NFSe)
        /// </summary>
        public string TagExtraAssinatura { get; set; }

        /// <summary>
        /// Nome da tag que tem o atributo de identificador único a ser utilizado no Reference.URI da assinatura
        /// </summary>
        public string TagExtraAtributoID { get; set; }


        /// <summary>
        /// Nome da tag onde você escolhe como deseja que seja tratado o retorno.
        /// </summary>
        public string ResponseMediaType { get; set; }

        /// <summary>
        /// Nome da tag onde é definido o CodigoTOM da cidade
        /// </summary>
        public string CodigoTom { get; set; }

        /// <summary>
        /// Namespace do XML para validação de schema
        /// </summary>
        public string TargetNS { get; set; }

        /// <summary>
        /// Ambiente (2-Homologação ou 1-Produção)
        /// </summary>
        public TipoAmbiente TipoAmbiente { get; set; }

        /// <summary>
        /// Tipo do Documento Fiscal Eletrônico (DF-e)
        /// </summary>
        public TipoDFe TipoDFe { get; set; }

        /// <summary>
        /// Tipo de Emissao (1-Normal, 2-Contingencia, 6/7/8-SVC/AN/RS/SP, ...
        /// </summary>
        public TipoEmissao TipoEmissao { get; set; }

        /// <summary>
        /// URL para consulta do DFe (NFCe e CTe) manualmente no ambiente de homologação
        /// </summary>
        public string UrlChaveHomologacao { get; set; }

        /// <summary>
        /// URL para consulta do DFe (NFCe e CTe) manualmente no ambiente de produção
        /// </summary>
        public string UrlChaveProducao { get; set; }

        /// <summary>
        /// URL para consulta do DFe (NFCe e CTe) via QRCode no ambiente de homologação
        /// </summary>
        public string UrlQrCodeHomologacao { get; set; }

        /// <summary>
        /// URL para consulta do DFe (NFCe e CTe) via QRCode no ambiente de produção
        /// </summary>
        public string UrlQrCodeProducao { get; set; }

        /// <summary>
        /// Ação, do webservice, a ser executada no ambiente de homologação
        /// </summary>
        public string WebActionHomologacao { get; set; }

        /// <summary>
        /// Ação, do webservice, a ser executada no ambiente de produção
        /// </summary>
        public string WebActionProducao { get; set; }

        /// <summary>
        /// ContentType para conexão via classe HttpWebRequest
        /// </summary>
        /// <example>
        /// Exemplos de conteúdo:
        ///
        ///    application/soap+xml; charset=utf-8;
        ///    text/xml; charset=utf-8;
        ///
        /// Deixe o conteúdo em brando para utilizar um valor padrão.
        /// </example>
        public string WebContentType { get; set; }

        /// <summary>
        /// Endereço WebService do ambiente de homologação
        /// </summary>
        public string WebEnderecoHomologacao { get; set; }

        /// <summary>
        /// Endereço WebService do ambiente de produção
        /// </summary>
        public string WebEnderecoProducao { get; set; }

        /// <summary>
        /// Endereco para consumo de API - no ambiente de homologação
        /// </summary>
        public string RequestURIHomologacao { get; set; }

        /// <summary>
        /// Endereco para consumo de API - no ambiente de producao
        /// </summary>
        public string RequestURIProducao { get; set; }

        /// <summary>
        /// Chave de acesso utilizada nos URLs do padrão NACIONAL *(quando se necessita fazer o replace na url)
        /// </summary>
        public string ChaveAcesso { get; set; }

        /// <summary>
        /// Propriedade criada para atender o padrão FIORILLI, Monte Alto - SP; O município não precisa de assinatura em produção mas precisa em homologação.
        /// </summary>
        public TipoAmbiente? NaoAssina;

        /// <summary>
        /// Propriedade criada para atender o padrão DSF Versão 1.0.
        /// </summary>
        public bool EncriptaTagAssinatura {  get; set; }

        /// <summary>
        /// O serviço consome API? true ou false
        /// </summary>
        public bool IsAPI
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RequestURIProducao) || !string.IsNullOrWhiteSpace(RequestURIHomologacao))
                {
                    return true;
                }

                return false;
            }
        }

        /// <summary>
        /// Propriedade para habilitar o uso de usuário e senha para consumo pela API
        /// </summary>
        public bool LoginConexao { get; set; }

        /// <summary>
        /// Propriedade para habilitar o uso de certificado digital (default == True)
        /// </summary>
        public bool UsaCertificadoDigital { get; set; } = true;

        /// <summary>
        /// Propriedade para habilitar a conversao de alguma configuração para Base64 antes do envio (default == True)
        /// </summary>
        public bool ConverteSenhaBase64 { get; set; } = false;

        /// <summary>
        /// Método de solicitação da API
        /// </summary>
        public string MetodoAPI { get; set; }

        /// <summary>
        /// String do Soap para envio para o webservice;
        /// </summary>
        /// <example>
        /// Exemplo de conteúdo que deve ser inserido nesta propriedade:
        ///
        ///    <![CDATA[<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"><soap:Header>{xmlHeader}</soap:Header><soap:Body><nfeDadosMsg xmlns="{ActionWeb}">{xmlBody}</nfeDadosMsg></soap:Body></soap:Envelope>]]>
        ///
        ///    Onde estiver {xmlHeader} o conteúdo será substituído pelo XML do header em tempo de execução
        ///    Onde estiver {ActionWeb} o conteúdo será substituído pelo WebAction em tempo de execução
        ///    Onde estiver {xmlBody} o conteúdo será substituído pelo XML do Body em tempo de execução
        ///
        ///    Deixe o conteúdo em branco para utilizar um soap padrão.
        /// </example>
        public string WebSoapString { get; set; }

        private string _webSoapStringHomologacao;

        /// <summary>
        /// String do Soap para envio para o webservice;
        /// </summary>
        /// <example>
        /// Exemplo de conteúdo que deve ser inserido nesta propriedade:
        ///
        ///    <![CDATA[<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"><soap:Header>{xmlHeader}</soap:Header><soap:Body><nfeDadosMsg xmlns="{ActionWeb}">{xmlBody}</nfeDadosMsg></soap:Body></soap:Envelope>]]>
        ///
        ///    Onde estiver {xmlHeader} o conteúdo será substituído pelo XML do header em tempo de execução
        ///    Onde estiver {ActionWeb} o conteúdo será substituído pelo WebAction em tempo de execução
        ///    Onde estiver {xmlBody} o conteúdo será substituído pelo XML do Body em tempo de execução
        ///
        ///    Deixe o conteúdo em branco para utilizar um soap padrão.
        /// </example>
        public string WebSoapStringHomologacao
        {
            get
            {
                if (string.IsNullOrWhiteSpace(_webSoapStringHomologacao))
                {
                    return WebSoapString;
                }
                else
                {
                    return _webSoapStringHomologacao;
                }
            }
            set => _webSoapStringHomologacao = value;
        }

        private string _webSoapStringProducao;

        /// <summary>
        /// String do Soap para envio para o webservice;
        /// </summary>
        /// <example>
        /// Exemplo de conteúdo que deve ser inserido nesta propriedade:
        ///
        ///    <![CDATA[<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"><soap:Header>{xmlHeader}</soap:Header><soap:Body><nfeDadosMsg xmlns="{ActionWeb}">{xmlBody}</nfeDadosMsg></soap:Body></soap:Envelope>]]>
        ///
        ///    Onde estiver {xmlHeader} o conteúdo será substituído pelo XML do header em tempo de execução
        ///    Onde estiver {ActionWeb} o conteúdo será substituído pelo WebAction em tempo de execução
        ///    Onde estiver {xmlBody} o conteúdo será substituído pelo XML do Body em tempo de execução
        ///
        ///    Deixe o conteúdo em branco para utilizar um soap padrão.
        /// </example>
        public string WebSoapStringProducao
        {
            get
            {
                if (string.IsNullOrWhiteSpace(_webSoapStringProducao))
                {
                    return WebSoapString;
                }
                else
                {
                    return _webSoapStringProducao;
                }
            }
            set => _webSoapStringProducao = value;
        }

        /// <summary>
        /// Compactar a mensagem (XML) com GZIP para ser enviado ao webservice?
        /// </summary>
        public bool GZIPCompress { get; set; } = false;

        /// <summary>
        /// Versão do SOAP utilizada pelo webservice
        /// </summary>
        public string WebSoapVersion { get; set; }

        /// <summary>
        /// Nome da tag de retorno do serviço
        /// </summary>
        public string WebTagRetorno { get; set; }

        /// <summary>
        /// Nome da tag de retorno do serviço
        /// </summary>
        public string WebTagRetornoHomologacao { get; set; }

        /// <summary>
        /// Encoding do XML retornado pelo webservice (Padrão é UTF-8, mas tem webservices que retornam em encodings diferentes, para estes tem que definir para que os caracteres fiquem corretos.)
        /// </summary>
        public string WebEncodingRetorno { get; set; }

        /// <summary>
        /// Token de acesso ao webservice/api do município
        /// </summary>
        public string MunicipioToken { get; set; }

        /// <summary>
        /// Token de acesso ao soap do município
        /// </summary>
        public string TokenSoap { get; set; }

        /// <summary>
        /// Usuário de acesso ao webservice/api do município
        /// </summary>
        public string MunicipioUsuario { get; set; }

        /// <summary>
        /// Senha de acesso ao webservice/api do município
        /// </summary>
        public string MunicipioSenha { get; set; }

        /// <summary>
        /// ClientID para gerar token (no momento, apenas Padrão AGILI)
        /// </summary>
        public string ClientID { get; set; }

        /// <summary>
        /// ClientSecret para gerar token (no momento, apenas Padrão AGILI) 
        /// </summary>
        public string ClientSecret { get; set; }

        private int _TimeOutWebServiceConnect;

        /// <summary>
        /// Tempo, em milissegundos, para aguardar resposta dos WebServices. (default = 60000)
        /// </summary>
        public int TimeOutWebServiceConnect
        {
            get => _TimeOutWebServiceConnect <= 0 ? (_TimeOutWebServiceConnect = 60000) : _TimeOutWebServiceConnect;
            set => _TimeOutWebServiceConnect = value;
        }

        /// <summary>
        /// Buscar o arquivo de configuração na pasta base definida na propriedade "PastaDLL"? (true or false)
        /// Se existir o arquivo de configuração, da SEFAZ ou Prefeitura, na pasta definida na propriedade "PastaDLL", a DLL vai priorizar buscar as informações deste arquivo. 
        /// Muito útil quando a SEFAZ ou Prefeitura muda as configurações dos web-services sem aviso prévio, com isso podemos fazer os ajustes no XML de configuração rapidamente e 
        /// colocar o arquivo na pasta em questão que a DLL assuma as novas orientações sem precisar atualizar a DLL.
        /// Ativar esta configuração pode deixar a rotina, fração de segundos, mais lenta, pois vai fazer um acesso ao HD em busca do arquivo.
        /// Quando não ativamos esta propriedade a DLL continua buscando o arquivo de configuração somente nos recursos embutidos da mesma, e isso é muito rápido pois trabalha unicamente na memória.
        /// </summary>
        public bool BuscarConfiguracaoPastaBase { get; set; }

        private string _PastaDLL = string.Empty;

        /// <summary>
        /// Por padrão é a pasta base onde a DLL está instalada de onde está sendo executada, mas o desenvolvedor pode mudar esta pasta definindo a que lhe for melhor.
        /// </summary>
        public string PastaArquivoConfiguracao
        {
            get
            {
                if (string.IsNullOrWhiteSpace(_PastaDLL))
                {
                    _PastaDLL = System.IO.Path.GetDirectoryName(System.AppDomain.CurrentDomain.BaseDirectory);
                }

                return _PastaDLL;
            }

            set => _PastaDLL = value;
        }

        /// <summary>
        /// Versão da configuração definido no XML de configurações da UF ou Município
        /// Esta versão só vai existir se a configuração foi alterada e o ERP deseja usar ela na pasta sem precisar atualizar a DLL.
        /// A DLL vai verificar se a versão da configuração que está na pasta é maior que a versão da DLL, se for, vai priorizar o que está na pasta, caso contrário vai pegar a configuração embutida na DLL, pois é mais atual.
        /// Tag que deve ser informada no arquivo de configuração: Exemplo:
        /// 
        /// AC.XML
        /// 
        /// <?xml version="1.0" encoding="utf-8"?>
        /// <Configuracoes>
        /// 	<VersaoConfiguracao>202312061103</VersaoConfiguracao>
        /// 	<Heranca>SVRS.xml</Heranca>
        /// </Configuracoes>
        /// 
        /// Formato deve ser conforme acima: ano com 4 dígitos + mês com 2 dígitos (zeros a esquerda) + dia com 2 dígitos (zeros a esquerda) + horas com 2 dígitos (zeros a esquerda) + minutos com 2 dígitos (zeros a esquerda)
        /// </summary>
        public string VersaoConfiguracao { get; set; }

        #endregion Public Properties

        #region Public Methods

        /// <summary>
        /// Carregar do XML que contem configurações e atribuir o conteúdo nas propriedades da classe
        /// </summary>
        /// <param name="nomeTagServico">Nome da tag do serviço que será consumido</param>
        public void Load(string nomeTagServico)
        {
            NomeTagServico = nomeTagServico;

            if (CodigoConfig != 0)
            {
                var doc = new XmlDocument();
                doc.Load(LoadXmlConfig(Configuration.ArquivoConfigGeral));

                var listConfiguracoes = doc.GetElementsByTagName("Configuracoes");

                foreach (XmlNode nodeConfiguracoes in listConfiguracoes)
                {
                    var elementConfiguracoes = (XmlElement)nodeConfiguracoes;
                    var listArquivos = elementConfiguracoes.GetElementsByTagName("Arquivo");

                    foreach (var nodeArquivos in listArquivos)
                    {
                        var elementArquivos = (XmlElement)nodeArquivos;

                        if (elementArquivos.GetAttribute("ID") != CodigoConfig.ToString())
                        {
                            continue;
                        }

                        Nome = elementArquivos.GetElementsByTagName("Nome")[0].InnerText;
                        NomeUF = elementArquivos.GetElementsByTagName("UF")[0].InnerText;
                        PadraoNFSe = PadraoNFSe.None;

                        if (XMLUtility.TagExist(elementArquivos, "PadraoNFSe"))
                        {
                            try
                            {
                                PadraoNFSe = (PadraoNFSe)Enum.Parse(typeof(PadraoNFSe), XMLUtility.TagRead(elementArquivos, "PadraoNFSe"));
                            }
                            catch (Exception)
                            {
                                throw new Exception("Caro desenvolvedor, você esqueceu de definir no enumerador \"PadraoNFSe\" o tipo " + XMLUtility.TagRead(elementArquivos, "PadraoNFSe") + " e eu não tenho como resolver esta encrenca. Por favor, va lá e defina.");
                            }
                        }

                        LerXmlConfigEspecifico(GetConfigFile(elementArquivos.GetElementsByTagName("ArqConfig")[0].InnerText));

                        break;
                    }
                }

                if (string.IsNullOrWhiteSpace(Nome))
                {
                    throw new Exception((CodigoConfig.ToString().Length <= 2 ? "Unidade Federativa" : "Município") + " (" + CodigoConfig + ") não está implementado na DLL Unimake.DFe. Entre em contato com o suporte para solicitar a implementação.");
                }
            }
        }

        #endregion
    }

    /// <summary>
    /// Arquivos de schema específicos. Um mesmo serviço mais com vários arquivos de schema para validação, varia de acordo com uma determinada informação de tag, exemplo: CTe tem o Modal, Evento tem o tipo de evento, MDFe tem o modal, etc...
    /// </summary>
    public class SchemaEspecifico
    {
        #region Public Properties

        /// <summary>
        /// ID da parte específica do XML. Pode ser o TipoEvento para eventos ou o Modal para CTe e MDFe.
        /// </summary>
        public string Id { get; set; }

        /// <summary>
        /// Arquivo de schema principal
        /// </summary>
        public string SchemaArquivo { get; set; }

        /// <summary>
        /// Arquivo de schema da parte específica do XML
        /// </summary>
        public string SchemaArquivoEspecifico { get; set; }

        #endregion Public Properties
    }
}