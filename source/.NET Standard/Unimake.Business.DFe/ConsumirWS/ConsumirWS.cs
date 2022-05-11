using System;
using System.IO;
using System.Net;
using System.Net.Security;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe
{
    /// <summary>
    /// Classe para consumir webservices
    /// </summary>
    public class ConsumirWS
    {
        #region Private Fields

        /// <summary>
        /// Objeto para leitura de cookies
        /// </summary>
        private readonly CookieContainer cookies = new CookieContainer();
        private static bool TratarScape { get; set; }

        #endregion Private Fields

        #region Private Methods

        /// <summary>
        /// Criar o envelope (SOAP) para envio ao webservice
        /// </summary>
        /// <param name="soap">Soap</param>
        /// <param name="xmlBody">string do XML a ser enviado no corpo do soap</param>
        /// <returns>string do envelope (soap)</returns>
        private static string EnveloparXML(WSSoap soap, string xmlBody)
        {
            if(soap.GZIPCompress)
            {
                xmlBody = Compress.GZIPCompress(xmlBody);
            }

            if(xmlBody.IndexOf("?>") >= 0)
            {
                xmlBody = xmlBody.Substring(xmlBody.IndexOf("?>") + 2);
            }

            var retorna = "<?xml version=\"1.0\" encoding=\"utf-8\"?>";

            if(TratarScape)
            {
                xmlBody = xmlBody.Replace("<", "&lt;").Replace(">", "&gt;");

                retorna += soap.SoapString.Replace("{xmlBodyScape}", xmlBody);
            }
            else
            {
                retorna += soap.SoapString.Replace("{xmlBody}", xmlBody);
            }

            return retorna;
        }

        #endregion Private Methods

        #region Public Properties

        /// <summary>
        /// Conteudo retornado pelo WebService consumido (formato string)
        /// </summary>
        public string RetornoServicoString { get; private set; }

        /// <summary>
        /// Conteudo retornado pelo WebService consumido (formato XmlDocument)
        /// </summary>
        public XmlDocument RetornoServicoXML { get; private set; }

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
            TratarScape = soap.SoapString.IndexOf("{xmlBodyScape}") > 0;

            var urlpost = new Uri(soap.EnderecoWeb);
            var soapXML = EnveloparXML(soap, xml.OuterXml);
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
            httpWebRequest.ClientCertificates.Add(certificado);
            httpWebRequest.ContentLength = buffer2.Length;

            //Definir dados para conexão com proxy
            if(soap.Proxy != null)
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
            }
            catch(WebException ex)
            {
                webException = ex;
                responsePost = ex.Response;

                if(ex.Response == null)
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
                retornoXml.LoadXml(conteudoRetorno);
            }
            catch(XmlException ex)
            {
                if(webException != null)
                {
                    throw (webException);
                }

                throw (ex);
            }
            catch(Exception ex)
            {
                throw (ex);
            }

            if(soap.TagRetorno.ToLower() != "prop:innertext")
            {
                if(retornoXml.GetElementsByTagName(soap.TagRetorno)[0] == null)
                {
                    throw new Exception("Não foi possível localizar a tag <" + soap.TagRetorno + "> no XML retornado pelo webservice.\r\n\r\n" +
                        "Conteúdo retornado pelo servidor:\r\n\r\n" +
                        retornoXml.InnerXml);
                }

                if(TratarScape)
                {
                    RetornoServicoString = retornoXml.GetElementsByTagName(soap.TagRetorno)[0].ChildNodes[0].InnerText;
                }
                else
                {
                    RetornoServicoString = retornoXml.GetElementsByTagName(soap.TagRetorno)[0].ChildNodes[0].OuterXml;
                }
            }
            else
            {
                if(string.IsNullOrWhiteSpace(retornoXml.InnerText))
                {
                    throw new Exception("A propriedade InnerText do XML retornado pelo webservice está vazia.");
                }

                RetornoServicoString = retornoXml.InnerText;

                //Remover do XML retornado o conteúdo ﻿<?xml version="1.0" encoding="utf-8"?> ou gera falha na hora de transformar em XmlDocument
                if(RetornoServicoString.IndexOf("?>") >= 0)
                {
                    RetornoServicoString = RetornoServicoString.Substring(RetornoServicoString.IndexOf("?>") + 2);
                }

                //Remover quebras de linhas
                RetornoServicoString = RetornoServicoString.Replace("\r\n", "");
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