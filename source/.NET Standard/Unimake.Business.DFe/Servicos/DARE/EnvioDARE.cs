﻿#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.DARE;
using Unimake.Exceptions;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Net.Http;
using Newtonsoft.Json;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Servicos.DARE
{
    /// <summary>
    /// Enviar o xml do DARE único para a API
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.DARE.EnvioDAREUnitario")]
    [ComVisible(true)]
#endif
    public class EnvioDARE : ServicoBase, IInteropService<Unimake.Business.DFe.Xml.DARE.DARE>
    {
        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        /// <exception cref="NotImplementedException"></exception>
        protected override void DefinirConfiguracao()
        {
            var xml = new Unimake.Business.DFe.Xml.DARE.DARE();
            xml = xml.LerXML<Unimake.Business.DFe.Xml.DARE.DARE>(ConteudoXML);

            Configuracoes.Servico = Servico.DAREEnvio;
            Configuracoes.SchemaVersao = xml.Versao;

            base.DefinirConfiguracao();

            Configuracoes.HttpContent = GerarJSON();
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Contém o resultado do envio único do DARE
        /// </summary>
        public DARERetorno Result
        {
            get
            {
                if (!string.IsNullOrEmpty(RetornoWSString))
                {
                    return XMLUtility.Deserializar<DARERetorno>(RetornoWSXML);
                }

                return new DARERetorno
                {
                    DARE = new DAREUnicoRetorno
                    {
                        Erro = new Erro
                        {
                            Mensagens = new List<string>
                            {
                                "Ocorreu um erro ao tentar obter o objeto no retorno da API"
                            }
                        }
                    }
                };
            }
        }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="envioDARE">Objeto contendo o XML do DARE único</param>
        /// <param name="configuracao">Objeto contendo as configurações a serem utilizadas no envio do DARE único</param>
        public EnvioDARE(Unimake.Business.DFe.Xml.DARE.DARE envioDARE, Configuracao configuracao)
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(envioDARE?.GerarXML() ?? throw new ArgumentNullException(nameof(envioDARE)), configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="envioDARE">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao"></param>
        /// <exception cref="NotImplementedException"></exception>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] Unimake.Business.DFe.Xml.DARE.DARE envioDARE, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(envioDARE?.GerarXML() ?? throw new ArgumentNullException(nameof(envioDARE)), configuracao);
                Executar();
            }
            catch (ValidarXMLException ex)
            {
                Exceptions.ThrowHelper.Instance.Throw(ex);
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

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Gravar o XML de distribuição em uma pasta definida retornado pela API
        /// </summary>
        /// <param name="pasta">Pasta onde será grava o XML de distribuição</param>
        /// <param name="nomeArquivo">Nome para o arquivo XML</param>
        public void GravarXmlDistribuicao(string pasta, string nomeArquivo)
        {
            try
            {
                if (Result.DARE == null)
                {
                    throw new Exception("API não retornou as informações do DARE. Verifique se o DARE foi emitido!");
                }

                base.GravarXmlDistribuicao(pasta, nomeArquivo, Result.GerarXML().OuterXml);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <inheritdoc />
#if INTEROP
        [ComVisible(false)]
#endif
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML) => throw new Exception("Método não implementado! Utilize o GravarXmlDistribuicao(string pasta, string nomeArquivo)");

        /// <inheritdoc />
#if INTEROP
        [ComVisible(false)]
#endif
        public override void GravarXmlDistribuicao(Stream stream, string value, Encoding encoding = null) => throw new Exception("Método não implementado! Utilize o GravarXmlDistribuicao(string pasta, string nomeArquivo)");


        /// <summary>
        /// Gravar Guia do DARE retornadas na consulta, quando o DARE é autorizado.
        /// </summary>
        /// <param name="pasta">Pasta onde será gravado o PDF das guias</param>
        /// <param name="nomeArquivo">Nome do arquivo PDF das guias que será gravado</param>
        public void ExtrairPDF(string pasta, string nomeArquivo)
        {
            try
            {
                if (string.IsNullOrEmpty(Result.DARE.DocumentoImpressao))
                {
                    throw new Exception("API não retornou as guias em PDF. Verifique se o DARE foi emitido.");
                }

                Converter.Base64ToPDF(Result.DARE.DocumentoImpressao, Path.Combine(pasta, nomeArquivo));
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Cria o HttpContent necessário para o serviço EnvioDARE
        /// </summary>
        /// <returns></returns>
        protected override HttpContent GerarJSON()
        {
            // Desserializar XML para o objeto Dare
            XmlSerializer serializer = default(XmlSerializer);
            serializer = new XmlSerializer(typeof(Unimake.Business.DFe.Xml.DARE.DARE));

            Unimake.Business.DFe.Xml.DARE.DARE dareObj;

            using (StringReader reader = new StringReader(ConteudoXML.OuterXml))
            {
                dareObj = (Unimake.Business.DFe.Xml.DARE.DARE)serializer.Deserialize(reader);
            }

            // Serializar o objeto para JSON
            string json = JsonConvert.SerializeObject(dareObj, Newtonsoft.Json.Formatting.Indented);

            return new StringContent(json, Encoding.UTF8, Configuracoes.WebContentType);

        }

        #endregion Public Methods.

    }
}
