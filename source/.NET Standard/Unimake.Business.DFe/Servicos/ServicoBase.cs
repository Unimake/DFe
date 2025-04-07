#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.IO;
using System.Net;
using System.Xml;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Validator;
using Unimake.Business.DFe.Xml;
using Newtonsoft.Json;
using System.Collections.Generic;
using System.Net.Http.Headers;
using System.Net.Http;
using System.Text;

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
        private XmlDocument _ConteudoXML;

        /// <summary>
        /// Verifica se o XML está assinado, se não estiver assina. Só faz isso para XMLs que tem tag de assinatura, demais ele mantem como está, sem assinar.
        /// </summary>
        /// <param name="tagAssinatura">Tag de assinatura</param>
        /// <param name="tagAtributoID">Tag que detêm o atributo ID</param>
        protected virtual void VerificarAssinarXML(string tagAssinatura, string tagAtributoID)
        {
            if (Configuracoes.UsaCertificadoDigital)
            {
                if (!string.IsNullOrWhiteSpace(tagAssinatura) && Configuracoes.NaoAssina == null && Configuracoes.NaoAssina != Configuracoes.TipoAmbiente)
                {
                    if (AssinaturaDigital.EstaAssinado(ConteudoXML, tagAssinatura))
                    {
                        AjustarXMLAposAssinado();
                    }
                    else
                    {
                        AssinaturaDigital.Assinar(ConteudoXML, tagAssinatura, tagAtributoID, Configuracoes.CertificadoDigital, AlgorithmType.Sha1, true, "", true);

                        AjustarXMLAposAssinado();
                    }
                }
            }
        }

        /// <summary>
        /// Conteúdo do XML, pode ou não estar assinado. Esta propriedade é utilizada em tempo de processamento.
        /// Utilize as propriedades ConteudoXMLOriginal ou ConteudoXMLAssinado para recuperar o que você deseja fora da classe.
        /// </summary>
        protected XmlDocument ConteudoXML
        {
            get => _ConteudoXML;
            set
            {
                if (ConteudoXMLOriginal == null)
                {
                    ConteudoXMLOriginal = new XmlDocument();
                    ConteudoXMLOriginal.LoadXml(value?.OuterXml);
                }

                _ConteudoXML = value;
            }
        }

        /// <summary>
        /// Construtor
        /// </summary>
        protected ServicoBase() { }

        /// <summary>
        /// Este método é uma possibilidade de fazer ajustes no XML depois de assinado, pois ele é executado assim que a assinatura é feita. Basta implementar ele nas heranças.
        /// </summary>
        protected virtual void AjustarXMLAposAssinado() { }

        /// <summary>
        /// Defini o valor das propriedades do objeto "Configuracoes"
        /// </summary>
        protected virtual void DefinirConfiguracao()
        {
            /* 
             As alterações a seguir foram necessárias para impedir uma leitura errada das configurações:
            Ao passar a primeira vez pela função "Load(GetType().Name)", ela carregava o arquivo Config.xml onde contém as configurações individuais dos municípios e carrega as informações deste
            Ao ser chamada uma segunda vez (independente do motivo), ela procurava o arquivo de configuração do município dentro do próprio alvo da busca e isto gerava erro.
            
            Atualmente, a propriedade Configuracoes.Definida está sendo atribuído o valor true apenas para NFSe;

             "Ok, mas por que não definir todas as configurações dentro do ServiçoBaseNFSe?"
                Por que há configurações dentro do EnveloparXML() que estão sendo compartilhadas em mais de um DFe
             
             Nesta refatoração, estou colocando oque é específico de um serviço, dentro da classe do serviço (Ex: GerarJSON() para as classes EnvioDARE e EnvioDARELote);
                ....                            oque é específico entre um tipo de DFe, dentro da classe ServicoBaseDFe..

             */
            if (!Configuracoes.Definida)                                    
            {                                                               
                Configuracoes.Load(GetType().Name);
            }

            if (Configuracoes.HttpContent == null && Configuracoes.RequestURI != null && Configuracoes.MetodoAPI != "get")  //Esta verificação é para API. WebSOAP ainda não foi alterado e está percocorrendo o caminho original dentro do ConsumirBase.cs
            {
                Configuracoes.HttpContent = EnveloparXML();
            }

            Configuracoes.Definida = true;
        }

        private HttpContent EnveloparXML()
        {
            var xmlBody = ConteudoXMLAssinado.OuterXml;

            if (Configuracoes.GZIPCompress)
            {
                xmlBody = Convert.ToBase64String(Encoding.UTF8.GetBytes(xmlBody));
                xmlBody = Compress.GZIPCompress(ConteudoXML);
            }

            //No momento, somente IPM 2.04 está utilizando WebSoapString em comunicação API, ele precisa o login acima
            if (!string.IsNullOrWhiteSpace(Configuracoes.WebSoapString))
            {
                Configuracoes.WebSoapString = Configuracoes.WebSoapString.Replace("{xml}", xmlBody);
                HttpContent temp = new StringContent(Configuracoes.WebSoapString, Encoding.UTF8, Configuracoes.WebContentType);
                return temp;
            }

            if (Configuracoes.WebContentType == "application/json")
            {
                var dicionario = new Dictionary<string, string>();

                if (Configuracoes.LoginConexao)
                {
                    dicionario.Add("usuario", Configuracoes.MunicipioUsuario);
                    dicionario.Add("senha", Configuracoes.MunicipioSenha);
                }

                dicionario.Add((string.IsNullOrWhiteSpace(Configuracoes.WebActionProducao) ? "xml" : Configuracoes.WebActionProducao), xmlBody);

                var Json = JsonConvert.SerializeObject(dicionario);

                HttpContent temp = new StringContent(Json, Encoding.UTF8, Configuracoes.WebContentType);

                return temp;
            }
            else if (Configuracoes.WebContentType == "multipart/form-data")
            {
                var path = string.Empty;

                if (string.IsNullOrWhiteSpace(ConteudoXML.BaseURI))
                {
                    path = "arquivo.xml";
                }
                else
                {
                    path = ConteudoXML.BaseURI.Substring(8, ConteudoXML.BaseURI.Length - 8);
                }

                var boundary = "----------------------------" + DateTime.Now.Ticks.ToString("x");

                #region ENVIO EM BYTES
                var xmlBytes = Encoding.UTF8.GetBytes(xmlBody);
                var xmlContent = new ByteArrayContent(xmlBytes);
                xmlContent.Headers.ContentType = MediaTypeHeaderValue.Parse("text/xml");
                xmlContent.Headers.ContentEncoding.Add("ISO-8859-1");
                xmlContent.Headers.ContentDisposition = new ContentDispositionHeaderValue("form-data")
                {
                    Name = "f1",
                    FileName = path,

                };
                #endregion ENVIO EM BYTES

                HttpContent MultiPartContent = new MultipartContent("form-data", boundary)
                {
                    xmlContent,

                };

                if (!string.IsNullOrWhiteSpace(Configuracoes.CodigoTom))               //SERÁ USADO PARA IPM 1.00 / Campo Mourão - PR 
                {
                    var usuario = new StringContent(Configuracoes.MunicipioUsuario);
                    usuario.Headers.ContentType = MediaTypeHeaderValue.Parse("text/xml");
                    usuario.Headers.ContentEncoding.Add("UTF-8");
                    usuario.Headers.ContentDisposition = new ContentDispositionHeaderValue("form-data")
                    {
                        Name = "login",
                    };
                    var senha = new StringContent(Configuracoes.MunicipioSenha);
                    senha.Headers.ContentType = MediaTypeHeaderValue.Parse("text/xml");
                    senha.Headers.ContentEncoding.Add("UTF-8");
                    senha.Headers.ContentDisposition = new ContentDispositionHeaderValue("form-data")
                    {
                        Name = "senha",
                    };
                    var codigoTom = new StringContent(Configuracoes.CodigoTom);
                    codigoTom.Headers.ContentType = MediaTypeHeaderValue.Parse("text/xml");
                    codigoTom.Headers.ContentEncoding.Add("UTF-8");
                    codigoTom.Headers.ContentDisposition = new ContentDispositionHeaderValue("form-data")
                    {
                        Name = "cidade",
                    };
                    var f1 = new StringContent(path);
                    f1.Headers.ContentType = MediaTypeHeaderValue.Parse("text/xml");
                    f1.Headers.ContentEncoding.Add("ISO-8859-1");
                    f1.Headers.ContentDisposition = new ContentDispositionHeaderValue("form-data")
                    {
                        Name = "f1",
                    };
                    HttpContent MultiPartContent2 = new MultipartContent("form-data", boundary)
                    {
                        usuario,
                        senha,
                        codigoTom,
                        f1,
                        xmlContent
                    };

                    return MultiPartContent2;
                }

                return MultiPartContent;
            }

            return new StringContent(xmlBody, Encoding.UTF8, Configuracoes.WebContentType);
        }

        /// <summary>
        /// Validar o schema do XML
        /// </summary>
        protected abstract void XmlValidar();

        /// <summary>
        /// Validar o conteúdo das tags do XML, alguns validações manuais que o schema não faz. Vamos implementando novas regras na medida da necessidade de cada serviço.
        /// </summary>
        protected abstract void XmlValidarConteudo();

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

            System.Diagnostics.Trace.WriteLine(ConteudoXML?.InnerXml, "Unimake.DFe");

            //Forçar criar a tag QrCode bem como assinatura para que o usuário possa acessar o conteúdo no objeto do XML antes de enviar
            _ = ConteudoXMLAssinado;

            XmlValidar();
        }


        /// <summary>
        /// Configurações diversas para consumir os serviços
        /// </summary>
        public Configuracao Configuracoes { get; set; }

        /// <summary>
        /// Conteúdo do XML assinado.
        /// </summary>
        public virtual XmlDocument ConteudoXMLAssinado
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
        /// Propriedade para uso interno nos testes unitários. 
        /// </summary>
        public HttpStatusCode HttpStatusCode { get; private set; }

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

        static ServicoBase() => AppDomain.CurrentDomain.AssemblyResolve += AssemblyResolver.AssemblyResolve;

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
                    RequestURI = (Configuracoes.RequestURI),
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
                    Servico = Configuracoes.Servico,
                    UsaCertificadoDigital = Configuracoes.UsaCertificadoDigital,
                    Host = (Configuracoes.TipoAmbiente == TipoAmbiente.Producao ? Configuracoes.HostProducao : Configuracoes.HostHomologacao),
                    ApiKey = Configuracoes.ApiKey,
                    HttpContent = Configuracoes.HttpContent,
                };

                var consumirAPI = new ConsumirAPI();
                consumirAPI.ExecutarServico(apiConfig, Configuracoes.CertificadoDigital);

                RetornoWSString = consumirAPI.RetornoServicoString;
                RetornoWSXML = consumirAPI.RetornoServicoXML;
                RetornoStream = consumirAPI.RetornoStream;  //Retorno específico para criação de .pdf para os casos em que a String corrompe o conteúdo. Mauricio 27/09/2023 #157859
                HttpStatusCode = consumirAPI.HttpStatusCode;
            }
            else
            {
                var soap = new WSSoap
                {
                    EnderecoWeb = (Configuracoes.TipoAmbiente == TipoAmbiente.Producao ? Configuracoes.WebEnderecoProducao : Configuracoes.WebEnderecoHomologacao),
                    ActionWeb = (Configuracoes.TipoAmbiente == TipoAmbiente.Producao ? Configuracoes.WebActionProducao : Configuracoes.WebActionHomologacao),
                    TagRetorno = (Configuracoes.TipoAmbiente == TipoAmbiente.Homologacao && !string.IsNullOrEmpty(Configuracoes.WebTagRetornoHomologacao)) ? Configuracoes.WebTagRetornoHomologacao : Configuracoes.WebTagRetorno,
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
                    Servico = Configuracoes.Servico,
                    TemCDATA = Configuracoes.TemCDATA,
                    Proxy = (Configuracoes.HasProxy ? Proxy.DefinirServidor(Configuracoes.ProxyAutoDetect,
                                                                            Configuracoes.ProxyUser,
                                                                            Configuracoes.ProxyPassword) : null)
                };

                var consumirWS = new ConsumirWS();
                consumirWS.ExecutarServico(ConteudoXML, soap, Configuracoes.CertificadoDigital);

                RetornoWSString = consumirWS.RetornoServicoString;
                RetornoWSXML = consumirWS.RetornoServicoXML;
                HttpStatusCode = consumirWS.HttpStatusCode;
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
    }
}