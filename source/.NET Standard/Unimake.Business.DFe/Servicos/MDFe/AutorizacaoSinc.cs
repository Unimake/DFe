#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.MDFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.MDFe
{
    /// <summary>
    /// Enviar o XML de MDFe para o webservice no modo síncrono
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.MDFe.AutorizacaoSinc")]
    [ComVisible(true)]
#endif
    public class AutorizacaoSinc : ServicoBase, IInteropService<Xml.MDFe.MDFe>
    {
        #region Private Fields

        private Xml.MDFe.MDFe _MDFe;

        private readonly Dictionary<string, MdfeProc> MdfeProcs = new Dictionary<string, MdfeProc>();

        #endregion Private Fields

        #region Private Methods

        private void MontarQrCode()
        {
            MDFe = new Xml.MDFe.MDFe().LerXML<Xml.MDFe.MDFe>(ConteudoXML);

            if (MDFe.InfMDFeSupl == null)
            {
                MDFe.InfMDFeSupl = new InfMDFeSupl();

                var urlQrCode = (Configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? Configuracoes.UrlQrCodeHomologacao : Configuracoes.UrlQrCodeProducao);

                var paramLinkQRCode = urlQrCode +
                    "?chMDFe=" + MDFe.InfMDFe.Chave +
                    "&tpAmb=" + ((int)MDFe.InfMDFe.Ide.TpAmb).ToString();

                if (MDFe.InfMDFe.Ide.TpEmis == TipoEmissao.ContingenciaFSIA)
                {
                    paramLinkQRCode += "&sign=" + Converter.ToRSASHA1(Configuracoes.CertificadoDigital, MDFe.InfMDFe.Chave);
                }

                MDFe.InfMDFeSupl.QrCodMDFe = paramLinkQRCode.Trim();
            }

            //Atualizar a propriedade do XML do MDFe novamente com o conteúdo atual já a tag de QRCode e link de consulta
            ConteudoXML = MDFe.GerarXML();
        }

        /// <summary>
        /// Validar o XML do MDFe e também o Modal específico
        /// </summary>
        /// <param name="xml">XML a ser validado</param>
        /// <param name="schemaArquivo">Nome do arquivo de schemas para ser utilizado na validação</param>
        /// <param name="targetNS">Namespace a ser utilizado na validação</param>
        private void ValidarXMLMDFe(XmlDocument xml, string schemaArquivo, string targetNS)
        {
            var validar = new ValidarSchema();
            validar.Validar(xml, Configuracoes.TipoDFe.ToString() + "." + schemaArquivo, targetNS);

            if (!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }

        #endregion Private Methods

        #region Protected Methods

        /// <summary>
        /// Efetuar um Ajustse no XML da NFCe logo depois de assinado
        /// </summary>
        protected override void AjustarXMLAposAssinado()
        {
            MontarQrCode();
            base.AjustarXMLAposAssinado();
        }

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (MDFe == null)
            {
                Configuracoes.Definida = false;
                return;
            }

            var xml = MDFe;

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.MDFeAutorizacaoSinc;
                Configuracoes.CodigoUF = (int)xml.InfMDFe.Ide.CUF;
                Configuracoes.TipoAmbiente = xml.InfMDFe.Ide.TpAmb;
                Configuracoes.Modelo = xml.InfMDFe.Ide.Mod;
                Configuracoes.TipoEmissao = xml.InfMDFe.Ide.TpEmis;
                Configuracoes.SchemaVersao = xml.InfMDFe.Versao;

                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Fields

        /// <summary>
        /// Propriedade com o conteúdo retornado da consulta situação do MDFe
        /// </summary>
        public List<RetConsSitMDFe> RetConsSitMDFe = new List<RetConsSitMDFe>();

        #endregion Public Fields

        #region Public Properties

        /// <summary>
        /// Objeto do XML do MDFe
        /// </summary>
        public Xml.MDFe.MDFe MDFe
        {
            get => _MDFe ?? (_MDFe = new Xml.MDFe.MDFe().LerXML<Xml.MDFe.MDFe>(ConteudoXML));
            protected set => _MDFe = value;
        }

        /// <summary>
        /// Propriedade contendo o XML da MDFe com o protocolo de autorização anexado
        /// </summary>
        public Dictionary<string, MdfeProc> MDFeProcResults
        {
            get
            {
                if (Result.ProtMDFe != null)
                {
                    if (MdfeProcs.ContainsKey(MDFe.InfMDFe.Chave))
                    {
                        MdfeProcs[MDFe.InfMDFe.Chave].ProtMDFe = Result.ProtMDFe;
                    }
                    else
                    {
                        MdfeProcs.Add(MDFe.InfMDFe.Chave, new MdfeProc
                        {
                            Versao = MDFe.InfMDFe.Versao,
                            MDFe = MDFe,
                            ProtMDFe = Result.ProtMDFe
                        });
                    }
                }
                else
                {
                    if (RetConsSitMDFe.Count <= 0)
                    {
                        throw new Exception("Defina o conteúdo da Propriedade RetConsSitMDFe, sem a definição dela não é possível obter o conteúdo da MDFeProcResults.");
                    }

                    ProtMDFe protMDFe = null;

                    #region Resultado do envio do MDFe através da consulta situação

                    foreach (var item in RetConsSitMDFe)
                    {
                        if (item != null && item.ProtMDFe != null)
                        {
                            if (item.ProtMDFe.InfProt.ChMDFe == MDFe.InfMDFe.Chave)
                            {
                                switch (item.ProtMDFe.InfProt.CStat)
                                {
                                    case 100: //MDFe Autorizado
                                        protMDFe = item.ProtMDFe;
                                        break;
                                }
                            }
                        }
                    }

                    if (MdfeProcs.ContainsKey(MDFe.InfMDFe.Chave))
                    {
                        MdfeProcs[MDFe.InfMDFe.Chave].ProtMDFe = protMDFe;
                    }
                    else
                    {
                        MdfeProcs.Add(MDFe.InfMDFe.Chave,
                            new MdfeProc
                            {
                                Versao = MDFe.InfMDFe.Versao,
                                MDFe = MDFe,
                                ProtMDFe = protMDFe
                            });
                    }

                    #endregion
                }

                return MdfeProcs;
            }
        }

#if INTEROP

        /// <summary>
        /// Recupera o XML de distribuição do MDFe no formato string
        /// </summary>
        /// <param name="chaveDFe">Chave do MDFe que é para retornar o XML de distribuição</param>
        /// <returns>XML de distribuição do MDFe</returns>
        public string GetMDFeProcResults(string chaveDFe)
        {
            var retornar = "";
            if (MDFeProcResults.Count > 0)
            {
                retornar = MDFeProcResults[chaveDFe].GerarXML().OuterXml;
            }

            return retornar;
        }

#endif

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetMDFe Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetMDFe>(RetornoWSXML);
                }

                return new RetMDFe
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public AutorizacaoSinc() : base() => MdfeProcs.Clear();

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="mdfe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public AutorizacaoSinc(Xml.MDFe.MDFe mdfe, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(mdfe?.GerarXML() ?? throw new ArgumentNullException(nameof(mdfe)), configuracao);
        }

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            if (!Configuracoes.Definida)
            {
                if (MDFe == null)
                {
                    throw new NullReferenceException($"{nameof(MDFe)} não pode ser nulo.");
                }

                DefinirConfiguracao();
            }

            base.Executar();
        }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            var xml = MDFe;

            var schemaArquivo = string.Empty;
            var schemaArquivoEspecifico = string.Empty;

            if (Configuracoes.SchemasEspecificos.Count > 0)
            {
                var modal = (int)xml.InfMDFe.Ide.Modal;

                schemaArquivo = Configuracoes.SchemasEspecificos[modal.ToString()].SchemaArquivo;
                schemaArquivoEspecifico = Configuracoes.SchemasEspecificos[modal.ToString()].SchemaArquivoEspecifico;
            }

            #region Validar o XML geral

            ValidarXMLMDFe(ConteudoXML, schemaArquivo, Configuracoes.TargetNS);

            #endregion Validar o XML geral

            #region Validar a parte específica de modal do MDFe

            var xmlEspecifico = new XmlDocument();
            foreach (XmlElement item in ConteudoXMLAssinado.GetElementsByTagName("infModal"))
            {
                xmlEspecifico.LoadXml(item.InnerXml);
            }
            ValidarXMLMDFe(xmlEspecifico, schemaArquivoEspecifico, Configuracoes.TargetNS);

            #endregion Validar a parte específica de modal do MDFe
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="mdfe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar(Xml.MDFe.MDFe mdfe, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(mdfe?.GerarXML() ?? throw new ArgumentNullException(nameof(mdfe)), configuracao);
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

        /// <summary>
        /// Definir o objeto contendo o XML a ser enviado e configuração de conexão e envio do XML para web-service
        /// </summary>
        /// <param name="mdfe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(Xml.MDFe.MDFe mdfe, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(mdfe?.GerarXML() ?? throw new ArgumentNullException(nameof(mdfe)), configuracao);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Adiciona um retorno da consulta situação da MDF-e.
        /// Este método está disponível apenas para interop
        /// </summary>
        /// <param name="item">Item que será adicionado</param>
        [ComVisible(true)]
        public void AddRetConsSitMDFe(RetConsSitMDFe item) =>
            (RetConsSitMDFe ?? (RetConsSitMDFe = new List<RetConsSitMDFe>())).Add(item);


        /// <summary>
        /// Recupera o conteúdo o único MDFe, assinado, existente no lote gerado de MDFe´s
        /// </summary>
        /// <returns>Retorna o conteúdo do MDFe, assinado, existente no lote gerado de MDFe´s</returns>
        public string GetConteudoMDFeAssinado() => (ConteudoXMLAssinado != null ? ConteudoXMLAssinado.OuterXml : "");

#endif

        /// <summary>
        /// Gravar o XML de distribuição em uma pasta no HD
        /// </summary>
        /// <param name="pasta">Pasta onde deve ser gravado o XML</param>
        public void GravarXmlDistribuicao(string pasta)
        {
            try
            {
                foreach (var item in MDFeProcResults)
                {
                    if (item.Value.ProtMDFe != null)
                    {
                        GravarXmlDistribuicao(pasta, item.Value.NomeArquivoDistribuicao, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno da consulta o protocolo da chave, abaixo, para a elaboração do arquivo de distribuição. Verifique se a chave ou recibo consultado estão de acordo com a informada na sequencia:\r\n\r\n" + Format.ChaveDFe(item.Key));
                    }
                }
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Grava o XML de distribuição no stream
        /// </summary>
        /// <param name="stream">Stream que vai receber o XML de distribuição</param>
#if INTEROP
        [ComVisible(false)]
#endif
        public void GravarXmlDistribuicao(Stream stream)
        {
            try
            {
                foreach (var item in MDFeProcResults)
                {
                    if (item.Value.ProtMDFe != null)
                    {
                        GravarXmlDistribuicao(stream, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno da consulta o protocolo da chave, abaixo, para a elaboração do arquivo de distribuição. Verifique se a chave ou recibo consultado estão de acordo com a informada na sequencia:\r\n\r\n" + Format.ChaveDFe(item.Key));
                    }
                }
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        #endregion Public Methods
    }
}