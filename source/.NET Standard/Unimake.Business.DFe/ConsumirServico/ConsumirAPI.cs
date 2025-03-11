using Org.BouncyCastle.Tls;
using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Exceptions;

namespace Unimake.Business.DFe
{
    /// <summary>{
    /// Classe para consumir API
    /// </summary>
    public class ConsumirAPI : ConsumirBase
    {

        /// <summary>
        /// Estabelece conexão com o Webservice e faz o envio do XML e recupera o retorno. Conteúdo retornado pelo webservice pode ser recuperado através das propriedades RetornoServicoXML ou RetornoServicoString.
        /// </summary>
        /// <param name="apiConfig">Parâmetros para execução do serviço (parâmetros da API)</param>
        /// <param name="certificado">Certificado digital a ser utilizado na conexão com os serviços</param>
        public void ExecutarServico(APIConfig apiConfig, X509Certificate2 certificado)
        {
            if (certificado == null && apiConfig.UsaCertificadoDigital)
            {
                throw new CertificadoDigitalException();
            }
            var httpWebRequest = CriarAPIRequest(apiConfig, certificado);

            var postData = new HttpResponseMessage();

            // Por não necessitar de conteúdo no envio, adiantei o método 
            if (string.Equals(apiConfig.MetodoAPI, "get", StringComparison.CurrentCultureIgnoreCase))
            {
                postData = httpWebRequest.GetAsync("").GetAwaiter().GetResult();
            }
            else
            {
                postData = httpWebRequest.PostAsync(apiConfig.RequestURI, apiConfig.HttpContent).GetAwaiter().GetResult();
            }

            httpWebRequest.Dispose();

            WebException webException = default(WebException);
            var responsePost = default(string);
            try
            {
                responsePost = postData.Content.ReadAsStringAsync().Result;
            }
            catch (WebException ex)
            {
                if (ex.Response == null)
                {
                    throw (ex);
                }
                else
                {
                    webException = ex;
                }
            }

            var retornoXml = new XmlDocument();
            try
            {
                HttpStatusCode = postData.StatusCode;
                var stream = default(Stream);
                retornoXml = TratarRetornoAPI.ReceberRetorno(ref apiConfig, postData, ref stream);
                RetornoStream = stream != null ? stream : null;
            }
            catch (XmlException)
            {
                if (webException != null)
                {
                    throw (webException);
                }

                throw new Exception(responsePost);
            }
            catch
            {
                throw new Exception(responsePost);
            }

            if (apiConfig.TagRetorno.ToLower() != "prop:innertext" && postData.IsSuccessStatusCode == true)
            {
                if (retornoXml.GetElementsByTagName(apiConfig.TagRetorno)[0] == null)
                {
                    throw new Exception("Não foi possível localizar a tag <" + apiConfig.TagRetorno + "> no XML retornado pelo webservice.\r\n\r\n" +
                        "Conteúdo retornado pelo servidor:\r\n\r\n" +
                        retornoXml.InnerXml);
                }
                RetornoServicoString = retornoXml.GetElementsByTagName(apiConfig.TagRetorno)[0].OuterXml;
            }
            else
            {
                if (string.IsNullOrWhiteSpace(retornoXml.InnerText))
                {
                    throw new Exception("A propriedade InnerText do XML retornado pelo webservice está vazia.");
                }

                RetornoServicoString = retornoXml.OuterXml;

                //Remover do XML retornado o conteúdo ﻿<?xml version="1.0" encoding="utf-8"?> ou gera falha na hora de transformar em XmlDocument
                if (RetornoServicoString.IndexOf("?>") >= 0)
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
            RetornoServicoXML.LoadXml(retornoXml.InnerXml);
        }

        private HttpClient CriarAPIRequest(APIConfig configuracoes, X509Certificate2 certificado)
        {
            var httpClientHandler = new HttpClientHandler();

            if (!configuracoes.UsaCertificadoDigital)
            {
                httpClientHandler.ClientCertificateOptions = ClientCertificateOption.Automatic;
                httpClientHandler.Credentials = CredentialCache.DefaultCredentials;
            }
            else
            {
                httpClientHandler.ClientCertificateOptions = ClientCertificateOption.Manual;
                httpClientHandler.ClientCertificates.Add(certificado);
            }

            var client = new HttpClient(httpClientHandler)
            {
                BaseAddress = new Uri(configuracoes.RequestURI),
            };

            if (!string.IsNullOrEmpty(configuracoes.Token))
            {
                client.DefaultRequestHeaders.Add("Authorization", configuracoes.Token);
            }

            if (!string.IsNullOrEmpty(configuracoes.Host))
            {
                client.DefaultRequestHeaders.Add("Host", configuracoes.Host);
            }

            if (!string.IsNullOrEmpty(configuracoes.ApiKey))
            {
                client.DefaultRequestHeaders.Add("api-key", $"{configuracoes.ApiKey}");
            }

            if (!string.IsNullOrEmpty(configuracoes.Cookie))
            {
                client.DefaultRequestHeaders.Add("Cookie: ", configuracoes.Cookie);
            }


            ServicePointManager.Expect100Continue = false;
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12 | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls;
            //ServicePointManager.ServerCertificateValidationCallback = new RemoteCertificateValidationCallback(RetornoValidacao);

            return client;
        }
    }
}