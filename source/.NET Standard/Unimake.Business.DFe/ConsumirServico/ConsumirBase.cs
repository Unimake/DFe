﻿using System;
using System.IO;
using System.Net;
using System.Net.Security;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Exceptions;

namespace Unimake.Business.DFe
{
    /// <summary>
    /// Classe para consumir webservices e API´s
    /// </summary>
    public abstract class ConsumirBase
    {
        #region Private Fields

        /// <summary>
        /// Objeto para leitura de cookies
        /// </summary>
        private readonly CookieContainer cookies = new CookieContainer();

        private static bool TratarScapeEnvio { get; set; }
        private static bool TratarScapeRetorno { get; set; }

        #endregion Private Fields

        #region Private Methods

        /// <summary>
        /// Criar o envelope (SOAP) para envio ao webservice
        /// </summary>
        /// <param name="soap">Soap</param>
        /// <param name="xmlBody">string do XML a ser enviado no corpo do soap</param>
        /// <param name="certificado">Objeto certificado</param>
        /// <returns>string do envelope (soap)</returns>
        private string EnveloparXML(WSSoap soap, string xmlBody, X509Certificate2 certificado)
        {
            if (soap.GZIPCompress)
            {
                xmlBody = Compress.GZIPCompress(xmlBody);
            }

            if (xmlBody.IndexOf("?>") >= 0)
            {
                xmlBody = xmlBody.Substring(xmlBody.IndexOf("?>") + 2);
            }

            var retorna = "<?xml version=\"1.0\" encoding=\"utf-8\"?>";

            if (soap.TemCDATA)
            {
                soap.SoapString = soap.SoapString.Replace("{cCDATA}", "]]>").Replace("{oCDATA}", "<![CDATA[");
            }

            if (soap.PadraoNFSe == PadraoNFSe.TINUS)
            {
                var doc = new XmlDocument();
                doc.LoadXml(xmlBody);
                xmlBody = "";

                foreach (XmlNode item in doc.GetElementsByTagName(doc.ChildNodes[0].Name)[0].ChildNodes)
                {
                    xmlBody += item.OuterXml.Replace(" xmlns=\"http://www.tinus.com.br\"", "");
                }
            }
            else if (soap.PadraoNFSe == PadraoNFSe.PROPRIOBARUERISP)
            {
                var doc = new XmlDocument();
                doc.LoadXml(xmlBody);
                var xmlNode = doc.GetElementsByTagName("ArquivoRPSBase64");
                if (xmlNode.Count > 0)
                {
                    var tagNode = xmlNode[0];
                    tagNode.InnerText = tagNode.InnerText.Base64Encode();
                    doc.GetElementsByTagName("ArquivoRPSBase64")[0].InnerText = tagNode.InnerText;
                }
                xmlBody = doc.OuterXml;
            }
            else if (soap.PadraoNFSe == PadraoNFSe.IIBRASIL)
            {
                soap.SoapString = soap.SoapString.Replace("{cCDATA}", "]]>").Replace("{oCDATA}", "<![CDATA[");

                var doc = new XmlDocument();
                doc.LoadXml(xmlBody);

                if (!xmlBody.Contains("Integridade"))
                {
                    var integridade = IIBRASIL.GerarIntegridade(xmlBody, soap.Token);
                    var noIntegridade = doc.CreateNode(XmlNodeType.Element, "Integridade", null);
                    noIntegridade.InnerText = integridade;
                    doc.FirstChild.FirstChild.AppendChild(noIntegridade);
                    xmlBody = doc.OuterXml;
                }
            }
            else if (soap.PadraoNFSe == PadraoNFSe.ELOTECH)
            {
                // Se o ERP já enviar o XML com o SOAP assinado, vamos atribuir o valor do xmlBody para a propriedade "retorna"
                if (xmlBody.Contains("SOAP-ENV:Envelope"))
                {
                    retorna = xmlBody;
                }
                else
                {
                    xmlBody = xmlBody.Replace("<?xml version=\"1.0\" encoding=\"utf-8\"?>", "");

                    var soapAssinado = new XmlDocument();
                    soapAssinado = ELOTECH.AssinaSoapElotech(soap, xmlBody, certificado);

                    retorna = soapAssinado.OuterXml;
                }
            }

            if (soap.Servico == Servico.EFDReinfConsultaReciboEvento)
            {
                var doc = new XmlDocument();
                doc.LoadXml(xmlBody);

                var tpEvento = doc.GetElementsByTagName("tipoEvento")[0].InnerText;

                xmlBody = doc.GetElementsByTagName("ConsultaReciboEvento")[0].OuterXml;
                xmlBody = xmlBody.Replace("ConsultaReciboEvento", "ConsultaReciboEvento" + tpEvento);
            }

            if (soap.Servico == Servico.EFDReinfConsultaFechamento2099)
            {
                var doc = new XmlDocument();
                doc.LoadXml(xmlBody);

                var xmlBodyConteudo = doc.GetElementsByTagName("ConsultaResultadoFechamento2099")[0].OuterXml;

                xmlBody = xmlBodyConteudo;
            }

            if (TratarScapeEnvio)
            {
                xmlBody = xmlBody.Replace("<", "&lt;").Replace(">", "&gt;");

                if (soap.SoapString.IndexOf("{xmlBodyScape}") > 0)
                {
                    retorna += soap.SoapString.Replace("{xmlBodyScape}", xmlBody);
                }
                else if (soap.SoapString.IndexOf("{xmlBodyScapeEnvio}") > 0)
                {
                    retorna += soap.SoapString.Replace("{xmlBodyScapeEnvio}", xmlBody);
                }

            }
            else if (TratarScapeRetorno)
            {
                retorna += soap.SoapString.Replace("{xmlBodyScapeRetorno}", xmlBody);
            }
            else
            {
                if (soap.PadraoNFSe != PadraoNFSe.ELOTECH)
                {
                    retorna += soap.SoapString.Replace("{xmlBody}", xmlBody);
                }
            }

            return retorna;
        }

        #endregion Private Methods

        #region Public Properties

        /// <summary>
        /// Conteudo retornado pelo WebService consumido (formato string)
        /// </summary>
        public string RetornoServicoString { get; protected set; }

        /// <summary>
        /// Conteudo retornado pelo WebService consumido (formato XmlDocument)
        /// </summary>
        public XmlDocument RetornoServicoXML { get; protected set; }

        /// <summary>
        /// Stream retornada pelo Webservice. Para consumo de serviços que retornam .pdf
        /// </summary>
        public Stream RetornoStream { get; set; }

        /// <summary>
        /// Propriedade para uso interno nos testes unitários. 
        /// </summary>
        public HttpStatusCode HttpStatusCode { get; protected set; }

        #endregion Public Properties

        #region Public Methods

        /// <summary>
        /// Estabelece conexão com o Webservice e faz o envio do XML e recupera o retorno. Conteúdo retornado pelo webservice pode ser recuperado através das propriedades RetornoServicoXML ou RetornoServicoString.
        /// </summary>
        /// <param name="xml">XML a ser enviado para o webservice</param>
        /// <param name="servico">Parâmetros para execução do serviço (parâmetros do soap)</param>
        /// <param name="certificado">Certificado digital a ser utilizado na conexão com os serviços</param>
        public void ExecutarServico(XmlDocument xml, object servico, X509Certificate2 certificado)
        {
            var soap = (WSSoap)servico;

            if (certificado == null && soap.UsaCertificadoDigital)
            {
                throw new CertificadoDigitalException();
            }

            TratarScapeEnvio = false;
            TratarScapeRetorno = false;

            if (soap.SoapString.IndexOf("{xmlBodyScape}") > 0)
            {
                TratarScapeEnvio = true;
                TratarScapeRetorno = true;
            }
            else if (soap.SoapString.IndexOf("{xmlBodyScapeEnvio}") > 0)
            {
                TratarScapeEnvio = true;
                TratarScapeRetorno = false;
            }
            else if (soap.SoapString.IndexOf("{xmlBodyScapeRetorno}") > 0)
            {
                TratarScapeEnvio = false;
                TratarScapeRetorno = true;
            }

            var urlpost = new Uri(soap.EnderecoWeb);
            var soapXML = EnveloparXML(soap, xml.OuterXml, certificado);
            
            var buffer2 = Encoding.UTF8.GetBytes(soapXML);

            ServicePointManager.Expect100Continue = false;
            ServicePointManager.ServerCertificateValidationCallback = new RemoteCertificateValidationCallback(RetornoValidacao);
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12 | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls;

            var httpWebRequest = (HttpWebRequest)HttpWebRequest.Create(urlpost);
            httpWebRequest.Headers.Add("SOAPAction: " + soap.ActionWeb);
            httpWebRequest.CookieContainer = cookies;
            httpWebRequest.Timeout = soap.TimeOutWebServiceConnect;
            httpWebRequest.ContentType = (string.IsNullOrEmpty(soap.ContentType) ? "application/soap+xml; charset=utf-8;" : soap.ContentType);
            httpWebRequest.Method = "POST";
            if (soap.UsaCertificadoDigital)
            {
                httpWebRequest.ClientCertificates.Add(certificado);
            }
            httpWebRequest.ContentLength = buffer2.Length;

            //Definir dados para conexão com proxy
            if (soap.Proxy != null)
            {
                httpWebRequest.Proxy = soap.Proxy;
            }

            var postData = httpWebRequest.GetRequestStream();
            postData.Write(buffer2, 0, buffer2.Length);
            postData.Close();

            WebException webException = null;
            WebResponse responsePost = null;
            try
            {
                responsePost = (HttpWebResponse)httpWebRequest.GetResponse();
                HttpStatusCode = HttpStatusCode.OK;
            }
            catch (WebException ex)
            {
                HttpStatusCode = (HttpStatusCode)ex.Status;
                webException = ex;
                responsePost = ex.Response;

                if (ex.Response == null)
                {
                    throw (ex);
                }
            }

            var streamPost = responsePost.GetResponseStream();

            var encoding = Encoding.GetEncoding(soap.EncodingRetorno);

            var streamReaderResponse = new StreamReader(streamPost, encoding);
            var conteudoRetorno = streamReaderResponse.ReadToEnd();

            var retornoXml = new XmlDocument();
            try
            {
                if (string.IsNullOrEmpty(conteudoRetorno)) 
                    throw new ValidarXMLRetornoException($"O XML retornado pelo WebService está vazio. Conteúdo XML: {conteudoRetorno}");

                if (!conteudoRetorno.TrimStart().StartsWith("<")) 
                    throw new ValidarXMLRetornoException($"O conteúdo retornado pelo WebService não é um XML válido. Conteúdo XML: {conteudoRetorno}");

                retornoXml.LoadXml(conteudoRetorno);
            }
            catch (XmlException ex)
            {
                if (webException != null)
                {
                    throw (webException);
                }

                throw (ex);
            }
            catch (Exception ex)
            {
                throw (ex);
            }

            if (soap.TagRetorno.ToLower() != "prop:innertext")
            {
                var tagRetorno = soap.TagRetorno;

                if (retornoXml.GetElementsByTagName(tagRetorno)[0] == null)
                {
                    if (retornoXml.GetElementsByTagName("soap:Body").Count >= 1 && retornoXml.GetElementsByTagName("soap:Body")[0].ChildNodes.Count >= 1)
                    {
                        tagRetorno = retornoXml.GetElementsByTagName("soap:Body")[0].ChildNodes[0].Name;
                    }

                    if (retornoXml.GetElementsByTagName(tagRetorno)[0] == null)
                    {
                        throw new Exception("Não foi possível localizar a tag <" + tagRetorno + "> no XML retornado pelo web-service.\r\n\r\n" +
                            "Conteúdo retornado pelo servidor:\r\n\r\n" + retornoXml.InnerXml);
                    }
                }

                if (TratarScapeRetorno)
                {
                    if (soap.PadraoNFSe == PadraoNFSe.GIF && retornoXml.GetElementsByTagName(tagRetorno)[0].ChildNodes[0].OuterXml.Contains("SOAP-ENV:Fault") || 
                        soap.PadraoNFSe == PadraoNFSe.DBSELLER && soap.TagRetorno == "SOAP-ENV:Body" || soap.PadraoNFSe == PadraoNFSe.FINTEL && soap.TagRetorno == "soap:Body")
                    {
                        RetornoServicoString = retornoXml.GetElementsByTagName(tagRetorno)[0].ChildNodes[0].OuterXml;
                    }
                    else if (soap.PadraoNFSe == PadraoNFSe.GISSONLINE && retornoXml.GetElementsByTagName(tagRetorno)[0].ChildNodes[0] == null)
                    {
                        RetornoServicoString = retornoXml.ChildNodes[0].OuterXml;
                    }
                    else if (soap.PadraoNFSe == PadraoNFSe.TIPLAN && retornoXml.GetElementsByTagName(tagRetorno)[0].ChildNodes[0].OuterXml.Contains("faultcode"))
                    {
                        RetornoServicoString = retornoXml.OuterXml;
                    }
                    else if (tagRetorno == "soap:Fault" || tagRetorno.Contains("faultcode") || (soap.PadraoNFSe == PadraoNFSe.ADM_SISTEMAS && retornoXml.OuterXml.Contains("s:Fault")))
                    {
                        RetornoServicoString = retornoXml.OuterXml;
                    }

                    // Padrão TECNOSISTEMAS às vezes retorna o InnerText sem formatação e gera o erro Dados nível raiz inválidos. Linha 1, posição 1
                    // Para corrigir, pegamos o OuterXml e fazemos um replace nos scapes para ficar correto
                    else if (soap.PadraoNFSe == PadraoNFSe.TECNOSISTEMAS)
                    {
                        RetornoServicoString = retornoXml.GetElementsByTagName(tagRetorno)[0].ChildNodes[0].OuterXml.Replace("&lt;", "<").Replace("&gt;", ">");
                    }

                    else if ((soap.PadraoNFSe == PadraoNFSe.MODERNIZACAO_PUBLICA || soap.PadraoNFSe == PadraoNFSe.METROPOLIS) && (soap.Servico != Servico.NFSeConsultarNfseFaixa && soap.Servico != Servico.NFSeConsultarNfsePorRps && 
                                soap.Servico != Servico.NFSeConsultarNfseServicoPrestado && soap.Servico != Servico.NFSeConsultarNfseServicoTomado && !retornoXml.GetElementsByTagName(tagRetorno)[0].OuterXml.Contains("Resposta")))
                    {
                        
                        RetornoServicoString = retornoXml.OuterXml;
                    }

                    else
                    {
                        RetornoServicoString = retornoXml.GetElementsByTagName(tagRetorno)[0].ChildNodes[0].InnerText;
                    }

                }
                else
                {
                    RetornoServicoString = retornoXml.GetElementsByTagName(tagRetorno)[0].ChildNodes[0].OuterXml;

                    if (soap.PadraoNFSe == PadraoNFSe.DSF && tagRetorno == "soap:Fault")
                    {
                        RetornoServicoString = retornoXml.OuterXml;
                    }
                }
            }
            else
            {
                if (string.IsNullOrWhiteSpace(retornoXml.InnerText))
                {
                    throw new Exception("A propriedade InnerText do XML retornado pelo web-service está vazia.");
                }

                RetornoServicoString = retornoXml.InnerText;

                //Remover do XML retornado o conteúdo ﻿<?xml version="1.0" encoding="utf-8"?> ou gera falha na hora de transformar em XmlDocument
                if (RetornoServicoString.IndexOf("?>") >= 0)
                {
                    RetornoServicoString = RetornoServicoString.Substring(RetornoServicoString.IndexOf("?>") + 2);
                }

                //Remover quebras de linhas
                RetornoServicoString = RetornoServicoString.Replace("\r\n", "");
            }

            if (soap.PadraoNFSe == PadraoNFSe.FIORILLI || soap.PadraoNFSe == PadraoNFSe.SONNER || soap.PadraoNFSe == PadraoNFSe.SMARAPD || soap.PadraoNFSe == PadraoNFSe.DSF)
            {
                RetornoServicoString = RetornoServicoString.Replace("ns1:", string.Empty);
                RetornoServicoString = RetornoServicoString.Replace("ns2:", string.Empty);
                RetornoServicoString = RetornoServicoString.Replace("ns3:", string.Empty);
                RetornoServicoString = RetornoServicoString.Replace("ns4:", string.Empty);
            }
            else if (soap.PadraoNFSe == PadraoNFSe.SIMPLE)
            {
                RetornoServicoString = RetornoServicoString.Replace("m:", string.Empty);
            }

            RetornoServicoXML = new XmlDocument
            {
                PreserveWhitespace = false
            };
            RetornoServicoXML.LoadXml(RetornoServicoString);
        }

        #endregion Public Methods

        /// <summary>
        /// Efetua validações do certificado - Por hora retorna sempre true, ou seja, não estamos validando nada.
        /// </summary>
        /// <param name="sender">Sender</param>
        /// <param name="certificate">Certificado digital</param>
        /// <param name="chain">X509Chain</param>
        /// <param name="sslPolicyErros">Políticas de Erros SSL</param>
        /// <returns>True = Tudo ok na validação - False = Problemas na validação</returns>
        private bool RetornoValidacao(object sender,
           X509Certificate certificate,
           X509Chain chain,
           SslPolicyErrors sslPolicyErros) => true;
    }
}