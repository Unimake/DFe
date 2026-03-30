using System;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe.ConsumirServico.Compatibility;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
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

            var request = new ApiConfigTransportRequestMapper().Map(apiConfig, certificado);

            using (var transportResponse = new ApiTransportExecutor().Execute(request))
            using (var httpResponse = transportResponse.HttpResponseMessage)
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
                    HttpStatusCode = transportResponse.StatusCode;
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
    }
}
