using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.DARE;
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
            if (certificado == null && apiConfig.UsaCertificadoDigital)
            {
                throw new CertificadoDigitalException();
            }

            // Este ajuste necessita ser antes de CriarRequest() pois existe ajustes:
            //  - login de IPM 2.04
            //  - Link do Padrão NACIONAL
            //  - Link do Padrão BAUHAUS
            AjustarLink(apiConfig, xml);

            var httpWebRequest = CriarRequest(certificado, apiConfig);

            var postData = new HttpResponseMessage();

            // Por não necessitar de conteúdo no envio, adiantei o método 
            if (string.Equals(apiConfig.MetodoAPI, "get", StringComparison.CurrentCultureIgnoreCase))
            {
                postData = httpWebRequest.GetAsync("").GetAwaiter().GetResult();
            }
            else
            {
                // Todo o processamento do Content irá ficar apenas no método POST
                var Content = default(HttpContent);

                if (apiConfig.Servico != Servico.DAREEnvio)
                {
                    Content = EnveloparXML(apiConfig, xml);
                }
                else
                {
                    Content = EnveloparJSON(apiConfig, xml);
                }
                postData = httpWebRequest.PostAsync(apiConfig.RequestURI, Content).GetAwaiter().GetResult();
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

        #region Configuração da requisição - Certificado - Headers - Ajuste do Link de comunicação
        /// <summary>
        /// Configurar a comunicação - Certificado - Headers - Ajuste do Link de comunicação
        /// </summary>
        private HttpClient CriarRequest(X509Certificate2 certificado, APIConfig config)
        {
            var httpClientHandler = new HttpClientHandler();

            if (!config.UsaCertificadoDigital)
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
                BaseAddress = new Uri(config.RequestURI),
            };

            if (!config.Token.IsNullOrEmpty())
            {
                client.DefaultRequestHeaders.Add("Authorization", config.Token);
            }

            if (!config.Host.IsNullOrEmpty())
            {
                client.DefaultRequestHeaders.Add("Host", $"{config.Host}");
            }

            if (!config.ApiKey.IsNullOrEmpty())
            {
                client.DefaultRequestHeaders.Add("api-key", $"{config.ApiKey}");
            }

            ServicePointManager.Expect100Continue = false;
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12 | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls;
            //ServicePointManager.ServerCertificateValidationCallback = new RemoteCertificateValidationCallback(RetornoValidacao);

            return client;
        }

        /// <summary>
        /// Ajuste de links dinâmicos (com variáveis)  
        /// </summary>
        private void AjustarLink(APIConfig config, XmlDocument xml)
        {
            switch (config.PadraoNFSe)
            {
                case PadraoNFSe.NACIONAL:
                    var startIndex = xml.OuterXml.IndexOf("Id=\"") + 7;
                    var endIndex = xml.OuterXml.IndexOf("\"", startIndex);
                    var chave = xml.OuterXml.Substring(startIndex, (endIndex - startIndex));
                    config.RequestURI = config.RequestURI.Replace("{Chave}", chave);
                    break;

                case PadraoNFSe.IPM:
                    config.Token = "Basic " + Convert.ToBase64String(Encoding.UTF8.GetBytes($"{config.MunicipioUsuario}:{config.MunicipioSenha}"));
                    break;

                case PadraoNFSe.BAUHAUS:        //Authorization Homologação: apiConfig.Token = "9f16d93554dc1d93656e23bd4fc9d4566a4d76848517634d7bcabd5dasdasde4948f";
                    if (config.RequestURI.IndexOf("NumeroRps") > 0)
                    {
                        chave = xml.GetElementsByTagName("NumeroRps")[0].InnerText;
                        config.RequestURI = config.RequestURI.Replace("{Chave}", chave);
                    }
                    else if (config.RequestURI.IndexOf("NumeroNfse") > 0)
                    {
                        chave = xml.GetElementsByTagName("NumeroNfse")[0].InnerText;
                        config.RequestURI = config.RequestURI.Replace("{Chave}", chave);
                    }
                    break;
            }
        }

        #endregion Configuração da requisição - Certificado - Headers - Ajuste do Link de comunicação


        /// <summary>
        /// Método para envolopar o XML, formando o JSON para comunicação com a API
        /// </summary>
        /// <param name="apiConfig"></param>    Configurações básicas para consumo da API
        /// <param name="xml"></param>          Arquivo XML que será enviado
        /// <returns></returns>
        private HttpContent EnveloparXML(APIConfig apiConfig, XmlDocument xml)
        {
            var xmlBody = xml.OuterXml;
            if (apiConfig.GZipCompress)
            {
                xmlBody = Convert.ToBase64String(Encoding.UTF8.GetBytes(xmlBody));
                xmlBody = Compress.GZIPCompress(xml);
            }

            //No momento, somente IPM 2.04 está utilizando WebSoapString em comunicação API, ele precisa o login acima
            if (!string.IsNullOrWhiteSpace(apiConfig.WebSoapString))
            {
                apiConfig.WebSoapString = apiConfig.WebSoapString.Replace("{xml}", xmlBody);
                HttpContent temp = new StringContent(apiConfig.WebSoapString, Encoding.UTF8, apiConfig.ContentType);
                return temp;
            }

            if (apiConfig.ContentType == "application/json")
            {
                var dicionario = new Dictionary<string, string>();

                if (apiConfig.LoginConexao)
                {
                    dicionario.Add("usuario", apiConfig.MunicipioUsuario);
                    dicionario.Add("senha", apiConfig.MunicipioSenha);
                }

                dicionario.Add((string.IsNullOrWhiteSpace(apiConfig.WebAction) ? "xml" : apiConfig.WebAction), xmlBody);

                var Json = JsonConvert.SerializeObject(dicionario);

                HttpContent temp = new StringContent(Json, Encoding.UTF8, apiConfig.ContentType);

                return temp;
            }
            else if (apiConfig.ContentType == "multipart/form-data")
            {
                var path = string.Empty;

                if (string.IsNullOrWhiteSpace(xml.BaseURI))
                {
                    path = "arquivo.xml";
                }
                else
                {
                    path = xml.BaseURI.Substring(8, xml.BaseURI.Length - 8);
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

                if (!string.IsNullOrWhiteSpace(apiConfig.CodigoTom))               //SERÁ USADO PARA IPM 1.00 / Campo Mourão - PR 
                {
                    var usuario = new StringContent(apiConfig.MunicipioUsuario);
                    usuario.Headers.ContentType = MediaTypeHeaderValue.Parse("text/xml");
                    usuario.Headers.ContentEncoding.Add("UTF-8");
                    usuario.Headers.ContentDisposition = new ContentDispositionHeaderValue("form-data")
                    {
                        Name = "login",
                    };
                    var senha = new StringContent(apiConfig.MunicipioSenha);
                    senha.Headers.ContentType = MediaTypeHeaderValue.Parse("text/xml");
                    senha.Headers.ContentEncoding.Add("UTF-8");
                    senha.Headers.ContentDisposition = new ContentDispositionHeaderValue("form-data")
                    {
                        Name = "senha",
                    };
                    var codigoTom = new StringContent(apiConfig.CodigoTom);
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

            return new StringContent(xmlBody, Encoding.UTF8, apiConfig.ContentType);
        }

        private HttpContent EnveloparJSON(APIConfig apiConfig, XmlDocument xml)
        {
            // Desserializar XML para o objeto Dare
            XmlSerializer serializer = default(XmlSerializer);

            if (xml.OuterXml.Contains("DareLote"))
            {

                serializer = new XmlSerializer(typeof(DARELote));
                DARELote dareObj;

                using (StringReader reader = new StringReader(xml.OuterXml))
                {
                    dareObj = (DARELote)serializer.Deserialize(reader);
                }

                // Serializar o objeto para JSON
                string json = JsonConvert.SerializeObject(dareObj, Newtonsoft.Json.Formatting.Indented);

                return new StringContent(json, Encoding.UTF8, apiConfig.ContentType);
            }
            else
            {
                serializer = new XmlSerializer(typeof(DARE));
                DARE dareObj;

                using (StringReader reader = new StringReader(xml.OuterXml))
                {
                    dareObj = (DARE)serializer.Deserialize(reader);
                }

                // Serializar o objeto para JSON
                string json = JsonConvert.SerializeObject(dareObj, Newtonsoft.Json.Formatting.Indented);

                return new StringContent(json, Encoding.UTF8, apiConfig.ContentType);
            }
        }
    }
}