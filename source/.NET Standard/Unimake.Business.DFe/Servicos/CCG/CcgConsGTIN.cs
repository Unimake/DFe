#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Exceptions;
using Unimake.Business.DFe.Xml.CCG;

namespace Unimake.Business.DFe.Servicos.CCG
{
    /// <summary>
    /// Enviar o XML de consulta de códigos GTIN para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CCG.CcgConsGTIN")]
    [ComVisible(true)]
#endif
    public class CcgConsGTIN : ServicoBase, IInteropService<ConsGTIN>
    {
        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ConsGTIN();
            xml = xml.LerXML<ConsGTIN>(ConteudoXML);

            Configuracoes.Servico = Servico.CCGConsGTIN;
            Configuracoes.CodigoUF = (int)UFBrasil.RS;
            Configuracoes.TipoAmbiente = TipoAmbiente.Producao;
            if (!Configuracoes.Definida)
            {
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetConsGTIN Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetConsGTIN>(RetornoWSXML);
                }

                return new RetConsGTIN
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public CcgConsGTIN() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="consGTIN">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public CcgConsGTIN(ConsGTIN consGTIN, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(consGTIN?.GerarXML() ?? throw new ArgumentNullException(nameof(consGTIN)), configuracao);
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="consGTIN">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar(ConsGTIN consGTIN, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(consGTIN?.GerarXML() ?? throw new ArgumentNullException(nameof(consGTIN)), configuracao);
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

#endif

        /// <inheritdoc />
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            try
            {
                throw new Exception("Não existe XML de distribuição para consulta de códigos GTIN.");
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }
    }
}