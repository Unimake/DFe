using System;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Exceptions;

namespace Unimake.Business.DFe
{
    /// <summary>
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

            using (var httpClient = CriarAPIRequest(apiConfig, certificado))
            using (var httpResponse = string.Equals(apiConfig.MetodoAPI, "get", StringComparison.CurrentCultureIgnoreCase)
                ? httpClient.GetAsync("").GetAwaiter().GetResult()
                : httpClient.PostAsync(apiConfig.RequestURI, apiConfig.HttpContent).GetAwaiter().GetResult())
            {
                WebException webException = null;
                string responseContent = null;
                try
                {
                    responseContent = httpResponse.Content.ReadAsStringAsync().Result;
                }
                catch (WebException ex)
                {
                    if (ex.Response == null)
                    {
                        throw;
                    }
                    else
                    {
                        webException = ex;
                    }
                }

                var retornoXml = new XmlDocument();
                try
                {
                    HttpStatusCode = httpResponse.StatusCode;
                    Stream stream = null;
                    retornoXml = TratarRetornoAPI.ReceberRetorno(ref apiConfig, httpResponse, ref stream);
                    RetornoServicoStream = stream;
                }
                catch (XmlException)
                {
                    if (webException != null)
                    {
                        throw;
                    }
                    throw new Exception(responseContent);
                }
                catch
                {
                    throw new Exception(responseContent);
                }

                if (apiConfig.TagRetorno.ToLower() != "prop:innertext" && httpResponse.IsSuccessStatusCode)
                {
                    if (retornoXml.GetElementsByTagName(apiConfig.TagRetorno)[0] == null)
                    {
                        throw new Exception("Não foi possível localizar a tag <" + apiConfig.TagRetorno + "> no XML retornado pelo webservice.\r\n\r\n" +
                            "Conteúdo retornado pelo servidor:\r\n\r\n" +
                            retornoXml.InnerXml);
                    }

                    if (apiConfig.PadraoNFSe == PadraoNFSe.PRONIM && string.IsNullOrWhiteSpace(retornoXml.GetElementsByTagName(apiConfig.TagRetorno)[0].InnerXml))
                    {
                        RetornoServicoString = retornoXml.OuterXml;
                    }
                    else
                    {
                        RetornoServicoString = retornoXml.GetElementsByTagName(apiConfig.TagRetorno)[0].OuterXml;
                    }

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
        }

        /// <summary>
        /// Cria e configura uma instância de HttpClient para requisições à API, incluindo certificado digital e headers necessários.
        /// </summary>
        /// <param name="configuracoes">Configurações da API, incluindo URI, headers e opções de autenticação.</param>
        /// <param name="certificado">Certificado digital a ser utilizado na conexão, se necessário.</param>
        /// <returns>Instância de HttpClient pronta para uso na requisição.</returns>
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
                BaseAddress = new Uri(configuracoes.RequestURI)
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