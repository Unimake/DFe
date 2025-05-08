#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.IO;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Security;
using Unimake.Exceptions;
using Newtonsoft.Json;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Collections.Generic;
using System.Xml.Linq;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Classe base para os serviços da NFSe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase : Servicos.ServicoBase
    {
        /// <summary>
        /// Construtor
        /// </summary>
        protected ServicoBase() : base() { }


        /// <summary>
        /// Definir configurações específicas da NFSe
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            //Esta linha irá carregar as informações referêntes ao município.
            Configuracoes.Load(GetType().Name);


            switch (Configuracoes.PadraoNFSe)
            {
                case PadraoNFSe.MEMORY:
                    MEMORY();
                    break;

                case PadraoNFSe.BAUHAUS:
                    BAUHAUS();
                    break;

                case PadraoNFSe.ABASE:
                case PadraoNFSe.BETHA:
                case PadraoNFSe.GINFES:
                case PadraoNFSe.EQUIPLANO:
                case PadraoNFSe.WEBFISCO:
                    PadroesConfigUnica();
                    break;

                case PadraoNFSe.IPM:
                    IPM();
                    break;

                case PadraoNFSe.NACIONAL:
                    NACIONAL();
                    break;

                case PadraoNFSe.HM2SOLUCOES:
                    HM2SOLUCOES();
                    AuthorizationBasic();
                    break;

                case PadraoNFSe.EL:

                    if (Configuracoes.SchemaVersao == "1.00")
                    {
                        EL();
                    }

                    break;
            }
            Configuracoes.Definida = true;
            base.DefinirConfiguracao();
        }

        private void HM2SOLUCOES()
        {
            if (!ConteudoXML.GetElementsByTagName("EnviarLoteRpsEnvio").IsNullOrEmpty())
            {
                _ = ConteudoXMLAssinado;

                var parameters = new Dictionary<string, string>
                {
                    { "xml", ConteudoXMLAssinado.OuterXml }
                };

                Configuracoes.HttpContent = new FormUrlEncodedContent(parameters);
            }
            else
            {
                XDocument document = XDocument.Parse(ConteudoXML.OuterXml);
                var dictionary = new Dictionary<string, string>();

                foreach (var parameters in document.Descendants())
                {
                    dictionary.Add(parameters.Name.ToString(), parameters.Value);
                }

                Configuracoes.HttpContent = new FormUrlEncodedContent(dictionary);
            }
        }

        #region Configurações separadas por PadrãoNFSe

        #region MEMORY

        private void MEMORY()
        {
            var numeroRPS = GetXMLElementInnertext("numeroRPS");
            var numeroNFSE = GetXMLElementInnertext("numeroNFSE");
            var protocolo = GetXMLElementInnertext("protocolo");
            var codMunicipio = GetXMLElementInnertext("codMunicipio");

            if (codMunicipio == null && Configuracoes.Servico == Servico.NFSeRecepcionarLoteRps)
            {
                var nodeloteRps = ConteudoXML.GetElementsByTagName("LoteRps")?[0];
                codMunicipio = nodeloteRps.Attributes.GetNamedItem("codMunicipio").Value;
            }

            // Replaces necessários para a comunicação com o webservice, deve estar antes da linha que altera o Codigo do Municipio
            Configuracoes.WebSoapString = Configuracoes.WebSoapString.Replace("{numeroRPS}", numeroRPS)
                                                                     .Replace("{numeroNFSE}", numeroNFSE)
                                                                     .Replace("{protocolo}", protocolo)
                                                                     .Replace("{codMunicipio}", codMunicipio)
                                                                     .Replace("{cnpjPrestador}", Configuracoes.MunicipioUsuario)
                                                                     .Replace("{hashValidador}", Configuracoes.MunicipioSenha);
            PadroesConfigUnica();
        }

        #endregion MEMORY

        #region IPM

        private void IPM()
        {
            AuthorizationBasic();

            if (Configuracoes.SchemaVersao == "2.80")
            {
                CriarHttpContentIPM();
            }

        }

        private void CriarHttpContentIPM()
        {
            var path = string.Empty;

            var xml = ConteudoXML.OuterXml.Replace("<", "&lt;")
                                          .Replace(">", "&gt;")
                                          .Replace("/", "")
                                          .Replace("&", "&amp;")
                                          .Replace("'", "&apos;")
                                          .Replace("\"", "&quot;");

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
            var xmlBytes = Encoding.UTF8.GetBytes(xml);
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
                HttpContent content = new MultipartContent("form-data", boundary)
                    {
                        usuario,
                        senha,
                        codigoTom,
                        f1,
                        xmlContent
                    };

                Configuracoes.HttpContent = content;
            }
        }

        #endregion IPM

        #region Bauhaus

        private void BAUHAUS()  //Authorization Homologação: apiConfig.Token = "9f16d93554dc1d93656e23bd4fc9d4566a4d76848517634d7bcabd5dasdasde4948f";
        {
            GerarContentBauhaus();
            AjusteLinkBauhaus();
        }

        private void GerarContentBauhaus()
        {
            var json = JsonConvert.SerializeObject(ConteudoXML);
            Configuracoes.HttpContent = new StringContent(json, Encoding.UTF8, Configuracoes.WebContentType);
        }

        private void AjusteLinkBauhaus()
        {
            Configuracoes.RequestURI = (Configuracoes.TipoAmbiente == TipoAmbiente.Producao ? Configuracoes.RequestURIProducao : Configuracoes.RequestURIHomologacao);

            var chave = default(string);
            if (Configuracoes.RequestURI.IndexOf("NumeroRps") > 0)
            {
                chave = ConteudoXML.GetElementsByTagName("NumeroRps")[0].InnerText;
                Configuracoes.RequestURI = Configuracoes.RequestURI.Replace("{Chave}", chave);
            }
            else if (Configuracoes.RequestURI.IndexOf("NumeroNfse") > 0)
            {
                chave = ConteudoXML.GetElementsByTagName("NumeroNfse")[0].InnerText;
                Configuracoes.RequestURI = Configuracoes.RequestURI.Replace("{Chave}", chave);
            }
        }

        #endregion Bauhaus

        #region NACIONAL

        private void NACIONAL()
        {
            var URI = Configuracoes.RequestURI;// = (Configuracoes.TipoAmbiente == TipoAmbiente.Producao ? Configuracoes.RequestURIProducao : Configuracoes.RequestURIHomologacao);

            var startIndex = ConteudoXML.OuterXml.IndexOf("Id=\"") + 7;
            var endIndex = ConteudoXML.OuterXml.IndexOf("\"", startIndex);
            var chave = ConteudoXML.OuterXml.Substring(startIndex, (endIndex - startIndex));
            Configuracoes.RequestURI = Configuracoes.RequestURI.Replace("{Chave}", chave);
        }

        #endregion NACIONAL

        #region EL

        private void EL()
        {
            var protocolo = GetXMLElementInnertext("Protocolo");
            var numeroNFSeRps = GetXMLElementInnertext("Numero");
            var cnpjCpfTomador = string.Empty;
            var cnpjCpfIntermediario = string.Empty;
            var dataInicial = string.Empty;
            var dataFinal = string.Empty;

            if (Configuracoes.Servico == Servico.NFSeConsultarNfse)
            {
                dataInicial = GetXMLElementInnertext("DataInicial");
                dataFinal = GetXMLElementInnertext("DataFinal");

                var tomador = ConteudoXML.GetElementsByTagName("Tomador");
                var intermediario = ConteudoXML.GetElementsByTagName("IntermediarioServico");

                if (tomador.Count > 0)
                {
                    foreach (XmlNode nodeTomador in tomador)
                    {
                        cnpjCpfTomador = nodeTomador.FirstChild.InnerText;                        
                    }
                }

                if (intermediario.Count > 0)
                {
                    foreach (XmlNode nodeIntermediario in intermediario)
                    {
                        cnpjCpfIntermediario = nodeIntermediario.FirstChild.InnerText;
                    }
                }
            }


            Configuracoes.WebSoapString = Configuracoes.WebSoapString.Replace("{MunicipioUsuario}", Configuracoes.MunicipioUsuario)
                                                                     .Replace("{MunicipioSenha}", Configuracoes.MunicipioSenha)
                                                                     .Replace("{numeroProtocolo}", protocolo)
                                                                     .Replace("{numeroNFSeRps}", numeroNFSeRps)
                                                                     .Replace("{dataInicial}", dataInicial)
                                                                     .Replace("{dataFinal}", dataFinal)
                                                                     .Replace("{cnpjCpfTomador}", cnpjCpfTomador)
                                                                     .Replace("{cnpjCpfIntermediario}", cnpjCpfIntermediario);

        }

        #endregion EL

        #endregion Configurações separadas por PadrãoNFSe

        private void PadroesConfigUnica()
        {
            //Municípios pontuais com configuração diferente:
            //São José dos Pinhais - PR     |GINFES
            //Varginha - MG                 |BETHA
            //Fortaleza - CE                |GINFES
            if (Configuracoes.CodigoMunicipio != 4125506 || Configuracoes.CodigoMunicipio != 3170701 || Configuracoes.CodigoMunicipio != 2304400)
            {
                Configuracoes.CodigoMunicipio = (int)(CodigoPadraoNFSe)Enum.Parse(typeof(CodigoPadraoNFSe), Configuracoes.PadraoNFSe.ToString());
            }
        }

        private void AuthorizationBasic()
        {
            // HM2SOLUCOES homologação:  11222333000181:S3nh@
            Configuracoes.MunicipioToken = "Basic " + Convert.ToBase64String(Encoding.UTF8.GetBytes($"{Configuracoes.MunicipioUsuario}:{Configuracoes.MunicipioSenha}"));
        }

        private string GetXMLElementInnertext(string tag) => ConteudoXML.GetElementsByTagName(tag)[0]?.InnerText;

        /// <summary>
        /// Ajustes no XMLs, depois de assinado.
        /// </summary>
        protected override void AjustarXMLAposAssinado()
        {
            #region Resolver problema da assinatura de Uberlândia-MG, que fugiu padrão mundial

            if (Configuracoes.CodigoMunicipio == 3170206) //Uberlândia (Tem esta zica na assinatura, pensa em merda.)
            {
                if (Configuracoes.Servico == Servico.NFSeRecepcionarLoteRps ||
                    Configuracoes.Servico == Servico.NFSeRecepcionarLoteRpsSincrono ||
                    Configuracoes.Servico == Servico.NFSeGerarNfse ||
                    Configuracoes.Servico == Servico.NFSeSubstituirNfse ||
                    Configuracoes.Servico == Servico.NFSeCancelarNfse)
                {

                    var xmlDoc = new XmlDocument();
                    xmlDoc.LoadXml(ConteudoXML.OuterXml);
                    var mudouXml = false;
                    if (Configuracoes.TagAssinatura.Equals("Rps"))
                    {
                        if (Configuracoes.Servico == Servico.NFSeGerarNfse)
                        {
                            var nodeRps = xmlDoc.GetElementsByTagName("Rps")[0];
                            var elementNodeRps = (XmlElement)nodeRps;
                            var elementInfDeclaracao = (XmlElement)elementNodeRps.GetElementsByTagName("InfDeclaracaoPrestacaoServico")[0];
                            var id = elementInfDeclaracao.GetAttribute("Id").Replace("ID_", "");
                            var elementSignatureValue = (XmlElement)elementNodeRps.GetElementsByTagName("SignatureValue")[0];

                            if (string.IsNullOrWhiteSpace(elementSignatureValue.GetAttribute("Id")))
                            {
                                var attributeId = xmlDoc.CreateAttribute("Id");
                                attributeId.Value = "ID_ASSINATURA_" + id;
                                elementSignatureValue.SetAttributeNode(attributeId);

                                mudouXml = true;
                            }
                        }
                        else
                        {
                            var listListaRps = xmlDoc.GetElementsByTagName("ListaRps");
                            foreach (XmlNode nodeListaRps in listListaRps)
                            {
                                var elementListaRps = (XmlElement)nodeListaRps;
                                foreach (XmlNode nodeRps in elementListaRps.GetElementsByTagName("Rps"))
                                {
                                    var elementRps = (XmlElement)nodeRps;
                                    if (elementRps.GetElementsByTagName("InfDeclaracaoPrestacaoServico").Count > 0)
                                    {
                                        var elementInfDeclaracao = (XmlElement)elementRps.GetElementsByTagName("InfDeclaracaoPrestacaoServico")[0];
                                        var id = elementInfDeclaracao.GetAttribute("Id").Replace("ID_", "");
                                        var elementSignatureValue = (XmlElement)elementRps.GetElementsByTagName("SignatureValue")[0];

                                        if (string.IsNullOrWhiteSpace(elementSignatureValue.GetAttribute("Id")))
                                        {
                                            var attributeId = xmlDoc.CreateAttribute("Id");
                                            attributeId.Value = "ID_ASSINATURA_" + id;
                                            elementSignatureValue.SetAttributeNode(attributeId);
                                            mudouXml = true;
                                        }
                                    }
                                }
                            }
                        }

                        if (mudouXml)
                        {
                            ConteudoXML.LoadXml(xmlDoc.OuterXml);
                        }
                    }
                    else if (Configuracoes.TagAssinatura.Equals("Pedido")) //Para o serviço CancelarNfse
                    {
                        var nodePedido = xmlDoc.GetElementsByTagName("Pedido")[0];
                        var elementNodePedido = (XmlElement)nodePedido;
                        var elementInfPedidoCancelamento = (XmlElement)elementNodePedido.GetElementsByTagName("InfPedidoCancelamento")[0];
                        var id = elementInfPedidoCancelamento.GetAttribute("Id").Replace("ID_PEDIDO_CANCELAMENTO_", "");
                        var elementSignatureValue = (XmlElement)elementNodePedido.GetElementsByTagName("SignatureValue")[0];

                        if (string.IsNullOrWhiteSpace(elementSignatureValue.GetAttribute("Id")))
                        {
                            var attributeId = xmlDoc.CreateAttribute("Id");
                            attributeId.Value = "ID_ASSINATURA_PEDIDO_CANCELAMENTO_" + id;
                            elementSignatureValue.SetAttributeNode(attributeId);

                            mudouXml = true;
                        }
                    }

                    if (mudouXml)
                    {
                        ConteudoXML.LoadXml(xmlDoc.OuterXml);
                    }

                }
            }

            #endregion
        }

        /// <summary>
        /// Conteúdo do XML assinado.
        /// </summary>
        public override XmlDocument ConteudoXMLAssinado
        {
            get
            {
                if (Configuracoes.PadraoNFSe == PadraoNFSe.DSF && Configuracoes.EncriptaTagAssinatura)
                {
                    var listLote = ConteudoXML.GetElementsByTagName("Lote");

                    foreach (XmlNode nodeLote in listLote)
                    {
                        var elementListLote = (XmlElement)nodeLote;

                        foreach (XmlNode nodeRps in elementListLote.GetElementsByTagName("RPS"))
                        {
                            var elementRps = (XmlElement)nodeRps;

                            var tagAssinatura = elementRps.GetElementsByTagName("Assinatura");

                            if (tagAssinatura.Count > 0)
                            {
                                var conteudoTagAssinatura = tagAssinatura[0].InnerText;

                                // O formato esperado do hash SHA-1 é 40 caracteres
                                // Se vier encriptado, vamos fazer nada
                                if (conteudoTagAssinatura.Length > 40)
                                {
                                    var sh1 = Criptografia.GetSHA1HashData(conteudoTagAssinatura);

                                    elementRps.GetElementsByTagName("Assinatura")[0].InnerText = sh1;
                                }
                            }
                        }
                    }
                }

                VerificarAssinarXML(Configuracoes.TagAssinatura, Configuracoes.TagAtributoID);
                VerificarAssinarXML(Configuracoes.TagLoteAssinatura, Configuracoes.TagLoteAtributoID);

                return ConteudoXML;
            }
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
                validar.Validar(ConteudoXML,
                    Configuracoes.TipoDFe.ToString() + "." + Configuracoes.PadraoNFSe.ToString() + "." + Configuracoes.SchemaArquivo,
                    Configuracoes.TargetNS,
                    Configuracoes.PadraoNFSe);

                if (!validar.Success)
                {
                    throw new ValidarXMLException(validar.ErrorMessage);
                }
            }
        }

        /// <summary>
        /// Inicializa configurações, parâmetros e propriedades para execução do serviço.
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML a ser enviado para o web-service</param>
        /// <param name="configuracao">Configurações a serem utilizadas para conexão e envio do XML para o web-service</param>
#if INTEROP
        [ComVisible(false)]
#endif
        protected override void Inicializar(XmlDocument conteudoXML, Configuracao configuracao)
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

            //base.Inicializar(conteudoXML, configuracao);
        }

        /// <summary>
        /// Validar, o conteúdo das tags do XML, alguns validações manuais que o schema não faz. Vamos implementando novas regras na medida da necessidade de cada serviço.
        /// </summary>
        protected override void XmlValidarConteudo() { }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        [ComVisible(true)]
        public virtual void Executar(string conteudoXML, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                var xmlDoc = new XmlDocument();
                xmlDoc.LoadXml(conteudoXML);

                Inicializar(xmlDoc, configuracao);

                Executar();
            }
            catch (ValidarXMLException ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
            catch (CertificadoDigitalException ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

#endif

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o webservice
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {

            if (Configuracoes.UsaCertificadoDigital && Configuracoes.NaoAssina == null && Configuracoes.NaoAssina != Configuracoes.TipoAmbiente)
            {
                if (!string.IsNullOrWhiteSpace(Configuracoes.TagAssinatura) && !AssinaturaDigital.EstaAssinado(ConteudoXML, Configuracoes.TagAssinatura))
                {
                    AssinaturaDigital.Assinar(ConteudoXML, Configuracoes.TagAssinatura, Configuracoes.TagAtributoID, Configuracoes.CertificadoDigital, AlgorithmType.Sha1, true, "Id");
                }

                if (!string.IsNullOrWhiteSpace(Configuracoes.TagLoteAssinatura) && !AssinaturaDigital.EstaAssinado(ConteudoXML, Configuracoes.TagLoteAssinatura))
                {
                    AssinaturaDigital.Assinar(ConteudoXML, Configuracoes.TagLoteAssinatura, Configuracoes.TagLoteAtributoID, Configuracoes.CertificadoDigital, AlgorithmType.Sha1, true, "Id");
                }
            }

            AjustarXMLAposAssinado();

            XmlValidar();

            base.Executar();
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
        /// <param name="encoding">Define o encodingo do stream, caso não informado ,será usado o UTF8</param>
#if INTEROP
        [ComVisible(false)]
#endif
        public virtual void GravarXmlDistribuicao(Stream stream,
                                                  string value,
                                                  Encoding encoding = null)
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
    }
}