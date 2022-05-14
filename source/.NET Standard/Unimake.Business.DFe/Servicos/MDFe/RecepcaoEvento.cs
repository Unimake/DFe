#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.MDFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.MDFe
{
    /// <summary>
    /// Enviar o XML de eventos do MDFe para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.MDFe.RecepcaoEvento")]
    [ComVisible(true)]
#endif
    public class RecepcaoEvento : ServicoBase
    {
        #region Private Fields
        private EventoMDFe EventoMDFe => new EventoMDFe().LerXML<EventoMDFe>(ConteudoXML);

        #endregion Private Fields

        #region Private Methods

        private void ValidarXMLEvento(XmlDocument xml, string schemaArquivo, string targetNS)
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
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new EventoMDFe();
            xml = xml.LerXML<EventoMDFe>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.CodigoUF = (int)xml.InfEvento.COrgao;
                Configuracoes.TipoAmbiente = xml.InfEvento.TpAmb;
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            var xml = EventoMDFe;

            var schemaArquivo = string.Empty;
            var schemaArquivoEspecifico = string.Empty;

            if (Configuracoes.SchemasEspecificos.Count > 0)
            {
                var tpEvento = ((int)xml.InfEvento.TpEvento);

                schemaArquivo = Configuracoes.SchemasEspecificos[tpEvento.ToString()].SchemaArquivo;
                schemaArquivoEspecifico = Configuracoes.SchemasEspecificos[tpEvento.ToString()].SchemaArquivoEspecifico;
            }

            #region Validar o XML geral

            ValidarXMLEvento(ConteudoXML, schemaArquivo, Configuracoes.TargetNS);

            #endregion Validar o XML geral

            #region Validar a parte específica de cada evento

            var listEvento = ConteudoXML.GetElementsByTagName("eventoMDFe");
            for (var i = 0; i < listEvento.Count; i++)
            {
                var elementEvento = (XmlElement)listEvento[i];

                if (elementEvento.GetElementsByTagName("infEvento")[0] != null)
                {
                    var elementInfEvento = (XmlElement)elementEvento.GetElementsByTagName("infEvento")[0];
                    if (elementInfEvento.GetElementsByTagName("tpEvento")[0] != null)
                    {
                        var xmlEspecifico = new XmlDocument();
                        xmlEspecifico.LoadXml(elementInfEvento.GetElementsByTagName(elementInfEvento.GetElementsByTagName("detEvento")[0].FirstChild.Name)[0].OuterXml);

                        ValidarXMLEvento(xmlEspecifico, schemaArquivoEspecifico, Configuracoes.TargetNS);
                    }
                }
            }

            #endregion Validar a parte específica de cada evento
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Propriedade contendo o XML do evento com o protocolo de autorização anexado
        /// </summary>
        public ProcEventoMDFe ProcEventoMDFeResult => new ProcEventoMDFe
        {
            Versao = EventoMDFe.Versao,
            EventoMDFe = EventoMDFe,
            RetEventoMDFe = Result
        };

        /// <summary>
        /// Conteúdo retornado pelo webservice depois do envio do XML
        /// </summary>
        public RetEventoMDFe Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetEventoMDFe>(RetornoWSXML);
                }

                return new RetEventoMDFe
                {
                    InfEvento = new RetEventoMDFeInfEvento
                    {
                        CStat = 0,
                        XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                    }
                };

            }
        }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="envEvento">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public RecepcaoEvento(EventoMDFe envEvento, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(envEvento?.GerarXML() ?? throw new ArgumentNullException(nameof(envEvento)), configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public RecepcaoEvento() : base() { }

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar() => base.Executar();

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o webservice
        /// </summary>
        /// <param name="envEvento">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o webservice</param>
        [ComVisible(true)]
        public void Executar(EventoMDFe envEvento, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(envEvento?.GerarXML() ?? throw new ArgumentNullException(nameof(envEvento)), configuracao);
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
        /// <param name="envEvento">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(EventoMDFe envEvento, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(envEvento?.GerarXML() ?? throw new ArgumentNullException(nameof(envEvento)), configuracao);
            }
            catch (Exception ex)
            {
                Exceptions.ThrowHelper.Instance.Throw(ex);
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
                GravarXmlDistribuicao(pasta, ProcEventoMDFeResult.NomeArquivoDistribuicao, ProcEventoMDFeResult.GerarXML().OuterXml);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Grava o XML de dsitribuição no stream
        /// </summary>
        /// <param name="stream">Stream que vai receber o XML de distribuição</param>
        public void GravarXmlDistribuicao(Stream stream)
        {
            try
            {
                GravarXmlDistribuicao(stream, ProcEventoMDFeResult.GerarXML().OuterXml);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        #endregion Public Methods
    }
}