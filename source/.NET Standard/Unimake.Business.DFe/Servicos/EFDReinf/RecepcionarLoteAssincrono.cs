#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.EFDReinf;
using Unimake.Exceptions;
using Unimake.Business.DFe.Utility;
using System.Xml;

namespace Unimake.Business.DFe.Servicos.EFDReinf
{
    /// <summary>
    /// Enviar o XML o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.EFDReinf.RecepcionarLoteAssincrono")]
    [ComVisible(true)]
#endif
    public class RecepcionarLoteAssincrono : ServicoBase, IInteropService<ReinfEnvioLoteEventos>
    {

        private ReinfEnvioLoteEventos _ReinfEnvioLoteEventos;

        /// <summary>
        /// Objeto do XML do lote eventos REINF
        /// </summary>
        public ReinfEnvioLoteEventos ReinfEnvioLoteEventos
        { 
            get => _ReinfEnvioLoteEventos ?? (_ReinfEnvioLoteEventos = new ReinfEnvioLoteEventos().LerXML<ReinfEnvioLoteEventos>(ConteudoXML));
            protected set => _ReinfEnvioLoteEventos = value;
        }

        private void ValidarXMLEvento(XmlDocument xml, string schemaArquivo, string targetNS)
        {
            var validar = new ValidarSchema();
            validar.Validar(xml, (Configuracoes.TipoDFe).ToString() + "." + schemaArquivo, targetNS);

            if (!validar.Success) 
            { 
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }

        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ReinfEnvioLoteEventos();
            xml = xml.LerXML<ReinfEnvioLoteEventos>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.EFDReinfRecepcionarLoteAssincrono;
                Configuracoes.CodigoUF = (int)UFBrasil.AN;
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            var xml = ReinfEnvioLoteEventos;
            var schemaArquivoEvento = string.Empty;

            ValidarXMLEvento(ConteudoXML, Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

            if (Configuracoes.TiposEventosEspecificos.Count > 0)
            {
                string eventoEspecifico;
                var listEventos = ConteudoXML.GetElementsByTagName("evento");

                foreach (var nodeEvento in listEventos)
                {
                    var elementEvento = (XmlElement)nodeEvento;
                    var reinfEvento = elementEvento.GetElementsByTagName("Reinf")[0];

                    var xmlEventoEspecifico = new XmlDocument();
                    xmlEventoEspecifico.LoadXml(reinfEvento.OuterXml);

                    eventoEspecifico = reinfEvento.FirstChild.Name;
                    schemaArquivoEvento = Configuracoes.TiposEventosEspecificos[eventoEspecifico.ToString()].SchemaArquivoEvento;

                    ValidarXMLEvento(xmlEventoEspecifico, schemaArquivoEvento, Configuracoes.TiposEventosEspecificos[eventoEspecifico.ToString()].TargetNS);
                }
            }
        }

        #endregion Protected Methods

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public RecepcionarLoteAssincrono() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="reinfRecepcionarLoteAssinc">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public RecepcionarLoteAssincrono(ReinfEnvioLoteEventos reinfRecepcionarLoteAssinc, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(reinfRecepcionarLoteAssinc?.GerarXML() ?? throw new ArgumentNullException(nameof(reinfRecepcionarLoteAssinc)), configuracao);
        }

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public ReinfRetornoLoteAssincrono Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<ReinfRetornoLoteAssincrono>(RetornoWSXML);
                }

                return new ReinfRetornoLoteAssincrono
                {
                    RetornoLoteEventosAssincrono = new RetornoLoteEventosAssincrono
                    {
                        Status = new Xml.EFDReinf.Status
                        {
                            CdResposta = 0,
                            DescResposta = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ",
                        }
                    }
                };
            }
        }


        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="reinfRecepcionarLoteAssinc">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] ReinfEnvioLoteEventos reinfRecepcionarLoteAssinc, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(reinfRecepcionarLoteAssinc?.GerarXML() ?? throw new ArgumentNullException(nameof(reinfRecepcionarLoteAssinc)), configuracao);
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

        /// <inheritdoc />
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            try
            {
                throw new Exception("Não existe XML de distribuição para consulta status do serviço.");
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        #endregion Public Methods
    }
}