using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
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
    /// Enviar o XML de MDFe para o webservice no modo assíncrono
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.MDFe.Autorizacao")]
    [ComVisible(true)]
#endif
    public class Autorizacao : ServicoBase, IInteropService<EnviMDFe>
    {
        #region Private Fields

        private EnviMDFe _enviMDFe;

        private readonly Dictionary<string, MdfeProc> MdfeProcs = new Dictionary<string, MdfeProc>();

        #endregion Private Fields

        #region Private Methods

        private void MontarQrCode()
        {
            EnviMDFe = new EnviMDFe().LerXML<EnviMDFe>(ConteudoXML);

            if (EnviMDFe.MDFe.InfMDFeSupl == null)
            {
                EnviMDFe.MDFe.InfMDFeSupl = new InfMDFeSupl();

                var urlQrCode = (Configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? Configuracoes.UrlQrCodeHomologacao : Configuracoes.UrlQrCodeProducao);

                var paramLinkQRCode = urlQrCode +
                    "?chMDFe=" + EnviMDFe.MDFe.InfMDFe.Chave +
                    "&tpAmb=" + ((int)EnviMDFe.MDFe.InfMDFe.Ide.TpAmb).ToString();

                if (EnviMDFe.MDFe.InfMDFe.Ide.TpEmis == TipoEmissao.ContingenciaFSIA)
                {
                    paramLinkQRCode += "&sign=" + Converter.ToRSASHA1(Configuracoes.CertificadoDigital, EnviMDFe.MDFe.InfMDFe.Chave);
                }

                EnviMDFe.MDFe.InfMDFeSupl.QrCodMDFe = paramLinkQRCode.Trim();
            }

            //Atualizar a propriedade do XML do MDFe novamente com o conteúdo atual já a tag de QRCode e link de consulta
            ConteudoXML = EnviMDFe.GerarXML();
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
        /// Efetuar ajustes no XML da NFCe logo depois de assinado
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
            if (EnviMDFe == null)
            {
                Configuracoes.Definida = false;
                return;
            }

            var xml = EnviMDFe;

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.MDFeAutorizacao;
                Configuracoes.CodigoUF = (int)xml.MDFe.InfMDFe.Ide.CUF;
                Configuracoes.TipoAmbiente = xml.MDFe.InfMDFe.Ide.TpAmb;
                Configuracoes.Modelo = xml.MDFe.InfMDFe.Ide.Mod;
                Configuracoes.TipoEmissao = xml.MDFe.InfMDFe.Ide.TpEmis;
                Configuracoes.SchemaVersao = xml.Versao;

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
        public EnviMDFe EnviMDFe
        {
            get => _enviMDFe ?? (_enviMDFe = new EnviMDFe().LerXML<EnviMDFe>(ConteudoXML));
            protected set => _enviMDFe = value;
        }

        /// <summary>
        /// Propriedade contendo o XML da MDFe com o protocolo de autorização anexado
        /// </summary>
        public Dictionary<string, MdfeProc> MDFeProcResults
        {
            get
            {
                if (RetConsReciMDFe == null && RetConsSitMDFe.Count <= 0)
                {
                    throw new Exception("Defina o conteúdo da Propriedade RetConsReciMDFe ou RetConsSitMDFe, sem a definição de uma delas não é possível obter o conteúdo da MDFeProcResults.");
                }

                ProtMDFe protMDFe = null;

                if (RetConsReciMDFe != null && RetConsReciMDFe.ProtMDFe != null)
                {
                    #region Resultado do envio do MDFe através da consulta recibo

                    if (RetConsReciMDFe.CStat == 104) //Lote Processado
                    {
                        foreach (var item in RetConsReciMDFe.ProtMDFe)
                        {
                            if (item.InfProt.ChMDFe == EnviMDFe.MDFe.InfMDFe.Chave)
                            {
                                switch (item.InfProt.CStat)
                                {
                                    case 100: //MDFe Autorizado
                                        protMDFe = item;
                                        break;
                                }

                                break;
                            }
                        }
                    }

                    #endregion Resultado do envio do MDFe através da consulta recibo
                }
                else if (RetConsSitMDFe.Count > 0)
                {
                    #region Resultado do envio do MDFe através da consulta situação

                    foreach (var item in RetConsSitMDFe)
                    {
                        if (item != null && item.ProtMDFe != null)
                        {
                            if (item.ProtMDFe.InfProt.ChMDFe == EnviMDFe.MDFe.InfMDFe.Chave)
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

                    #endregion Resultado do envio do MDF-e através da consulta situação
                }

                if (MdfeProcs.ContainsKey(EnviMDFe.MDFe.InfMDFe.Chave))
                {
                    MdfeProcs[EnviMDFe.MDFe.InfMDFe.Chave].ProtMDFe = protMDFe;
                }
                else
                {
                    //Se por algum motivo não tiver assinado, só vou forçar atualizar o ConteudoXML para ficar correto na hora de gerar o arquivo de distribuição. Pode estar sem assinar no caso do desenvolvedor estar forçando gerar o XML já autorizado a partir de uma consulta situação do MDFe, caso tenha perdido na tentativa do primeiro envio.
                    if (EnviMDFe.MDFe.Signature == null)
                    {
                        ConteudoXML = ConteudoXMLAssinado;
                        AjustarXMLAposAssinado();
                    }

                    MdfeProcs.Add(EnviMDFe.MDFe.InfMDFe.Chave,
                        new MdfeProc
                        {
                            Versao = EnviMDFe.Versao,
                            MDFe = EnviMDFe.MDFe,
                            ProtMDFe = protMDFe
                        });
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

        /// <summary>
        /// Recupera o conteúdo o único MDFe, assinado, existente no lote gerado de MDFe´s
        /// </summary>
        /// <returns>Retorna o conteúdo do MDFe, assinado, existente no lote gerado de MDFe´s</returns>
        public string GetConteudoMDFeAssinado() => (ConteudoXMLAssinado != null ? ConteudoXMLAssinado.GetElementsByTagName("MDFe")[0].OuterXml : "");

#endif

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetEnviMDFe Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetEnviMDFe>(RetornoWSXML);
                }

                return new RetEnviMDFe
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        /// <summary>
        /// Propriedade com o conteúdo retornado da consulta recibo
        /// </summary>
        public RetConsReciMDFe RetConsReciMDFe { get; set; }

#if INTEROP

        /// <summary>
        /// Atribuir null para a propriedade RetConsReciMDFe. (Em FOXPRO não conseguimos atribuir NULL diretamente na propriedade, dá erro de OLE)
        /// </summary>
        public void SetNullRetConsReciMDFe() => RetConsReciMDFe = null;

#endif

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public Autorizacao() : base() => MdfeProcs.Clear();

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="enviMDFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public Autorizacao(EnviMDFe enviMDFe, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(enviMDFe?.GerarXML() ?? throw new ArgumentNullException(nameof(enviMDFe)), configuracao);
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
                if (EnviMDFe == null)
                {
                    throw new NullReferenceException($"{nameof(EnviMDFe)} não pode ser nulo.");
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
            var xml = EnviMDFe;

            var schemaArquivo = string.Empty;
            var schemaArquivoEspecifico = string.Empty;

            if (Configuracoes.SchemasEspecificos.Count > 0)
            {
                var modal = (int)xml.MDFe.InfMDFe.Ide.Modal;

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
        /// Executa o serviço: Assina o XML, valida e envia para o webservice
        /// </summary>
        /// <param name="enviMDFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o webservice</param>
        [ComVisible(true)]
        public void Executar(EnviMDFe enviMDFe, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(enviMDFe?.GerarXML() ?? throw new ArgumentNullException(nameof(enviMDFe)), configuracao);
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
        /// Adicionar o retorno da consulta situação da CTe na lista dos retornos para elaboração do XML de Distribuição
        /// </summary>
        /// <param name="item">Resultado da consulta situação do MDFe</param>
        public void AddRetConsSitMDFe(RetConsSitMDFe item) => (RetConsSitMDFe ?? (RetConsSitMDFe = new List<RetConsSitMDFe>())).Add(item);

        /// <summary>
        /// Definir o objeto contendo o XML a ser enviado e configuração de conexão e envio do XML para web-service
        /// </summary>
        /// <param name="enviMDFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o self-service</param>
        public void SetXMLConfiguracao(EnviMDFe enviMDFe, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(enviMDFe?.GerarXML() ?? throw new ArgumentNullException(nameof(enviMDFe)), configuracao);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

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