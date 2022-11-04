using Newtonsoft.Json;
using System;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Net.Security;
using System.Runtime.CompilerServices;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Exceptions;

namespace Unimake.Business.DFe
{
    /// <summary>{
    /// Classe para consumir API
    /// </summary>
    public class ConsumirAPI : ConsumirBase 
    {
        private static Configuracao Configuracoes { get; set; }

        /// <summary>
        /// Estabelece conexão com o Webservice e faz o envio do XML e recupera o retorno. Conteúdo retornado pelo webservice pode ser recuperado através das propriedades RetornoServicoXML ou RetornoServicoString.
        /// </summary>
        /// <param name="xml">XML a ser enviado para o webservice</param>
        /// <param name="apiConfig">Parâmetros para execução do serviço (parâmetros da API)</param>
        /// <param name="certificado">Certificado digital a ser utilizado na conexão com os serviços</param>
        public void ExecutarServico(XmlDocument xml, APIConfig apiConfig, X509Certificate2 certificado)
        {
            if (certificado == null)
            {
                throw new CertificadoDigitalException();
            }


            var urlpost = new Uri(apiConfig.RequestURI);
            var json = EnveloparXML(apiConfig, xml.OuterXml);

            ServicePointManager.Expect100Continue = false;
            //ServicePointManager.ServerCertificateValidationCallback = new RemoteCertificateValidationCallback(RetornoValidacao);
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12 | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls;
                        
            var httpWebRequest = new HttpClient();

            //TODO: Mauricio - Precisamos definir a possibilidade de haver proxy no consumo das APIs
            //Definir dados para conexão com proxy
            //if (apiConfig.Proxy != null)
            //{
            //    httpWebRequest.Proxy = apiConfig.Proxy;
            //}
            
            var postData = httpWebRequest.PostAsync(apiConfig.RequestURI, new StringContent(json, Encoding.UTF8, apiConfig.ContentType)).GetAwaiter().GetResult();

            WebException webException = null;
            var responsePost = "";
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

            //TODO: Mauricio - Pensar numa maneira melhor de tratar o retorno com erro das APIs, 
            if (!postData.IsSuccessStatusCode)                        //code 200 = sucesso na comunicação com o servidor
            {
                RetornoServicoString = responsePost.Substring(responsePost.IndexOf("message"));
            }
            else
            {
                var retornoXml = new XmlDocument();
                try
                {
                    retornoXml.LoadXml(responsePost);
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

                if (retornoXml.GetElementsByTagName(apiConfig.TagRetorno)[0] == null)
                {
                    throw new Exception("Não foi possível localizar a tag <" + apiConfig.TagRetorno + "> no XML retornado pela API.\r\n\r\n" +
                        "Conteúdo retornado pelo servidor:\r\n\r\n" +
                        retornoXml.InnerXml);
                }
                else
                {
                    if (string.IsNullOrWhiteSpace(retornoXml.InnerText))
                    {
                        throw new Exception("A propriedade InnerText do XML retornado pelo webservice está vazia.");
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

                RetornoServicoXML = new XmlDocument
                {
                    PreserveWhitespace = false
                };
                RetornoServicoXML.LoadXml(retornoXml.InnerXml);
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="apiConfig"></param>
        /// <param name="xmlBody"></param>
        /// <returns></returns>
        private string EnveloparXML(APIConfig apiConfig, string xmlBody)
        {
            if (apiConfig.GZipCompress)
            {
                xmlBody = Compress.GZIPCompress(xmlBody);
            }

            ///TODO: Mauricio - Precisamos ajeitar como que irá resgatar o municipio usuário/senha, declarei essas propriedades dentro de APIConfig de modo genérico
            var json = new
            {
                usuario = "",
                senha = "",
                xml = xmlBody,
            };

            var result = Newtonsoft.Json.JsonConvert.SerializeObject(json);
            return result;
        }

        private object TratarRetorno(HttpResponseMessage httpClient)
        {
            #region Objeto de resposta
            //Objeto para tratar resposta de erro da CENTI (cancelar nfse)
            var objetoResposta = new
            {
                statusCode = (int)httpClient.StatusCode,
                message = httpClient.Content.ReadAsStringAsync().Status,
            };

            #endregion





            var result = objetoResposta;
            return result;
        }
    }
}