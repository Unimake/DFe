using System;
using System.Runtime.InteropServices;
using System.Text;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.ESocial;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.ESocial
{
    /// <summary>
    /// Enviar o XML o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.ESocial.EnviarLoteEventosESocial")]
    [ComVisible(true)]
#endif
    public class EnviarLoteEventosESocial : ServicoBase, IInteropService<ESocialEnvioLoteEventos>
    {
        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ESocialEnvioLoteEventos();
            xml = xml.LerXML<ESocialEnvioLoteEventos>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.ESocialEnviarLoteEventos;
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
        public EnviarLoteEventosESocial() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="eSocialEnviarLoteEventos">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public EnviarLoteEventosESocial(ESocialEnvioLoteEventos eSocialEnviarLoteEventos, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(eSocialEnviarLoteEventos?.GerarXML() ?? throw new ArgumentNullException(nameof(eSocialEnviarLoteEventos)), configuracao);
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
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] ESocialEnvioLoteEventos reinfRecepcionarLoteAssinc, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
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
            //try
            //{
            //    throw new Exception("Não existe XML de distribuição para consulta status do serviço.");
            //}
            //catch (Exception ex)
            //{
            //    ThrowHelper.Instance.Throw(ex);
            //}
        }

        #endregion Public Methods
    }
}

