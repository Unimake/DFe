#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.EFDReinf;
using Unimake.Exceptions;
using System.Xml;

namespace Unimake.Business.DFe.Servicos.EFDReinf
{
    /// <summary>
    /// Enviar o XML de consulta recibo Evento EFDReinf para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento")]
    [ComVisible(true)]
#endif
    public class ConsultaReciboEvento : ServicoBase, IInteropService<ReinfConsulta>
    {
        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ReinfConsulta();
            xml = xml.LerXML<ReinfConsulta>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.EFDReinfConsultaReciboEvento;
                Configuracoes.CodigoUF = (int)UFBrasil.AN;
                Configuracoes.SchemaVersao = xml.Versao;
                Configuracoes.TipoEventoEFDReinf = xml.ConsultaReciboEvento.TipoEvento;

                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public ReinfRetornoRecibo Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<ReinfRetornoRecibo>(RetornoWSXML);
                }

                return new ReinfRetornoRecibo { };
            }
        }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultaReciboEvento() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="reinfConsulta">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaReciboEvento(ReinfConsulta reinfConsulta, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(reinfConsulta?.GerarXML() ?? throw new ArgumentNullException(nameof(reinfConsulta)), configuracao);
        }

        ///<summary>
        ///Construtor simplificado para API
        /// </summary>
        /// <param name="tipoEvento">Tipo do evento a ser consultado</param>
        /// <param name="tipoInscricao">Tipo de incrição do contribuinte</param>
        /// <param name="numInscricao">Número da inscrição do contribuinte</param>
        /// <param name="tipoAmbiente">Ambiente de Produção ou Homologação</param>
        /// <param name="configuracao">Configuração para conexão e envio do XML</param>
        public ConsultaReciboEvento(string tipoEvento, TiposInscricao tipoInscricao, string numInscricao, TipoAmbiente tipoAmbiente, Configuracao configuracao) : this()
        {
            if (string.IsNullOrEmpty(tipoEvento))
            {
                throw new ArgumentNullException(nameof(tipoEvento));
            }

            if (string.IsNullOrEmpty(tipoInscricao.ToString()))
            {
                throw new ArgumentNullException(nameof(tipoInscricao));
            }

            if (string.IsNullOrEmpty(numInscricao))
            {
                throw new ArgumentNullException(nameof(numInscricao));
            }

            if (string.IsNullOrEmpty(tipoAmbiente.ToString()))
            {
                throw new ArgumentNullException(nameof(tipoAmbiente));
            }

            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            configuracao.TipoEventoEFDReinf = tipoEvento;

            var xml = new ReinfConsulta
            {
                Versao = "1.05.01",
                ConsultaReciboEvento = new Xml.EFDReinf.ConsultaReciboEvento
                {
                    TipoEvento = tipoEvento,
                    TpInsc = tipoInscricao,
                    NrInsc = numInscricao,
                    PerApurField = DateTime.Now.ToString("yyyy-MM")
                }

            };

            var doc = new XmlDocument();
            doc.LoadXml(xml?.GerarXML().OuterXml);

            Inicializar(doc, configuracao);
        }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="reinfConsulta">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] ReinfConsulta reinfConsulta, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(reinfConsulta?.GerarXML() ?? throw new ArgumentNullException(nameof(reinfConsulta)), configuracao);
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