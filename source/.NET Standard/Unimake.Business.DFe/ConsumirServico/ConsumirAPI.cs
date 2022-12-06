using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;
using System;
using System.Buffers;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Reflection;
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

            var Url = apiConfig.RequestURI;
            var Content = EnveloparXML(apiConfig, xml);

            #region Conexão API
            var Handler = new HttpClientHandler
            {
                ClientCertificateOptions = ClientCertificateOption.Automatic,                       // verificar se o modo automático atende a necessidade
            };

            var httpWebRequest = new HttpClient(Handler)
            {
                BaseAddress = new Uri(Url),
            };

            if (apiConfig.Token != null)
            {
                httpWebRequest.DefaultRequestHeaders.Add("Authorization", apiConfig.Token);
            }

            ServicePointManager.Expect100Continue = false;
            //ServicePointManager.ServerCertificateValidationCallback = new RemoteCertificateValidationCallback(RetornoValidacao);
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12 | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls;

            #endregion

            var postData = new HttpResponseMessage();
            if (apiConfig.MetodoAPI.ToLower() == "get")
            {
                postData = httpWebRequest.GetAsync("").GetAwaiter().GetResult();
            }
            else
            {
                postData = httpWebRequest.PostAsync(apiConfig.RequestURI, new StringContent(Content, Encoding.UTF8, apiConfig.ContentType)).GetAwaiter().GetResult();
            }

            WebException webException = null;
            var responsePost = string.Empty;
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

            //TODO: Mauricio - Ainda há o que melhorar no retorno com erro das APIs
            XmlDocument resultadoRetorno = new XmlDocument();
            if (postData.IsSuccessStatusCode)
            {
                switch (postData.Content.Headers.ContentType.MediaType)
                {
                    case "text/plain": //Retorno XML -> Não temos que fazer nada, já retornou no formato mais comum
                        resultadoRetorno.LoadXml(responsePost);
                        break;

                    case "application/json": //Retorno JSON -> Vamos ter que converter para XML
                        resultadoRetorno = JsonConvert.DeserializeXmlNode(responsePost, apiConfig.TagRetorno);
                        break;

                    case "application/xml": //Retorno xml
                        resultadoRetorno.LoadXml(responsePost);
                        break;
                }
            }
            else
            {
                switch (postData.Content.Headers.ContentType.MediaType)
                {
                    case "text/plain": //Retorno XML -> Não temos que fazer nada, já retornou no formato mais comum
                        break;

                    case "application/xml": //Retorno XML -> Não temos que fazer nada, já retornou no formato mais comum
                        resultadoRetorno.LoadXml(responsePost);
                        break;

                    case "application/json": //Retorno JSON -> Vamos ter que converter para XML
                        resultadoRetorno = JsonConvert.DeserializeXmlNode(responsePost, apiConfig.TagRetorno);
                        break;

                    case "text/html": //Retorno HTML -> Entendemos que sempre será erro
                        resultadoRetorno.LoadXml(responsePost);
                        break;
                }
            }

            var retornoXml = new XmlDocument();
            try
            {
                retornoXml = resultadoRetorno;

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

            if (apiConfig.TagRetorno.ToLower() != "prop:innertext")
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
                if (string.IsNullOrWhiteSpace(retornoXml.OuterXml))
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

        /// <summary>
        /// Método para envolopar o XML, formando o JSON para comunicação com a API
        /// </summary>
        /// <param name="apiConfig"></param>    Configurações básicas para consumo da API
        /// <param name="xml"></param>          Arquivo XML que será enviado
        /// <returns></returns>
        private string EnveloparXML(APIConfig apiConfig, XmlDocument xml)
        {
             var xmlBody = xml.OuterXml;
            if (apiConfig.GZipCompress)
            {
                xmlBody = Compress.GZIPCompress(xmlBody);
            }
            else if (apiConfig.B64)
            {
                xmlBody = Convert.ToBase64String(Encoding.UTF8.GetBytes(xmlBody));
            }


            var n = apiConfig.WebSoapString.CountChars('{');
            var dicionario = new Dictionary<string, string>();
            var posicaoInicial = 0;

            while (n > 0)
            {
                try
                {
                    var InicioTag = apiConfig.WebSoapString.IndexOf('{', posicaoInicial);
                    var FimTag = apiConfig.WebSoapString.IndexOf('}', posicaoInicial);
                    var tag = apiConfig.WebSoapString.Substring(InicioTag + 1, (FimTag - InicioTag) - 1);
                    posicaoInicial = FimTag + 1;

                    switch (tag.ToLower())
                    {
                        case "usuario":
                            dicionario.Add("usuario", apiConfig.MunicipioUsuario);
                            break;

                        case "senha":
                            dicionario.Add("senha", apiConfig.MunicipioSenha);
                            break;

                        case "xml":
                            dicionario.Add((apiConfig.WebAction == null ? "xml" : apiConfig.WebAction), xmlBody);
                            break;

                        case "token":
                            apiConfig.Token = "31931-P4STI3FJ9JQM74TOY9IF4YJL727SHMZH";     //temporário  teste GIAP Authorization: 31931-P4STI3FJ9JQM74TOY9IF4YJL727SHMZH
                            break;

                        default:
                            throw new Exception($"Não foi encontrado a Tag {tag} encontrada no WebSoapString - Xml de configução do Município");
                    }
                }
                catch (Exception ex)
                {
                    throw ex;
                }
                n--;
            }

            var result = "";
            if (apiConfig.ContentType == "application/json")
            {
                result = JsonConvert.SerializeObject(dicionario);
            } else
            {
                result = xmlBody;
            }
            return result;
        }
    }
}