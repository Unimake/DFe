#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.EFDReinf;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.EFDReinf
{
    /// <summary>
    /// Enviar o XML de consulta recibo Evento EFDReinf para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.EFDReinf.ConsultaLoteAssincrono")]
    [ComVisible(true)]
#endif
    public class ConsultaLoteAssincrono : ServicoBase, IInteropService<ReinfConsultaLoteAssincrono>
    {
        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ReinfConsultaLoteAssincrono();
            xml = xml.LerXML<ReinfConsultaLoteAssincrono>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.EFDReinfConsultaLoteAssincrono;
                Configuracoes.CodigoUF = (int)UFBrasil.AN;
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultaLoteAssincrono() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="reinfConsultaLoteAssinc">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaLoteAssincrono(ReinfConsultaLoteAssincrono reinfConsultaLoteAssinc, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }
            configuracao.NumeroProtocolo = reinfConsultaLoteAssinc.ConsultaLoteAssincrono.NumeroProtocolo;

            Inicializar(reinfConsultaLoteAssinc?.GerarXML() ?? throw new ArgumentNullException(nameof(reinfConsultaLoteAssinc)), configuracao);
        }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="reinfConsultaLoteAssinc">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] ReinfConsultaLoteAssincrono reinfConsultaLoteAssinc, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(reinfConsultaLoteAssinc?.GerarXML() ?? throw new ArgumentNullException(nameof(reinfConsultaLoteAssinc)), configuracao);
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