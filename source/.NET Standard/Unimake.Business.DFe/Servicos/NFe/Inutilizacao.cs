#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFe
{
    /// <summary>
    /// Enviar o XML de inutilização de NFe para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFe.Inutilizacao")]
    [ComVisible(true)]
#endif
    public class Inutilizacao : ServicoBase, IInteropService<InutNFe>
    {
        #region Private Properties

        private InutNFe InutNFe => new InutNFe().LerXML<InutNFe>(ConteudoXML);

        #endregion Private Properties

        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new InutNFe();
            xml = xml.LerXML<InutNFe>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.NFeInutilizacao;
                Configuracoes.CodigoUF = (int)xml.InfInut.CUF;
                Configuracoes.TipoAmbiente = xml.InfInut.TpAmb;
                Configuracoes.Modelo = xml.InfInut.Mod;
                Configuracoes.TipoEmissao = TipoEmissao.Normal; //Inutilização só funciona no tipo de emissão Normal, ou seja, não tem inutilização em SVC-AN ou SVC-RS
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Propriedade contendo o XML da inutilização com o protocolo de autorização anexado
        /// </summary>
        public ProcInutNFe ProcInutNFeResult => new ProcInutNFe
        {
            Versao = InutNFe.Versao,
            InutNFe = InutNFe,
            RetInutNFe = Result
        };

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetInutNFe Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetInutNFe>(RetornoWSXML);
                }

                return new RetInutNFe
                {
                    InfInut = new InfInut
                    {
                        CStat = 0,
                        XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                    }
                };
            }
        }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            if (InutNFe.InfInut.CUF != UFBrasil.MT && !string.IsNullOrWhiteSpace(InutNFe.InfInut.CPF))
            {
                throw new Exception("TAG CPF da inutilização só pode ser utilizada para envios a SEFAZ do Mato Grosso (MT), demais estados só permitem inutilização para CNPJ.");
            }

            //Se for SEFAZ do MT e for CPF, não pode validar, pois não existe CPF no pacote de Schema padrão nacional. Somente o MT está permitindo enviar CPF no XML de inutilização.
            if (InutNFe.InfInut.CUF != UFBrasil.MT || string.IsNullOrWhiteSpace(InutNFe.InfInut.CPF))
            {
                base.XmlValidar();
            }
        }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="inutNFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public Inutilizacao(InutNFe inutNFe, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(inutNFe?.GerarXML() ?? throw new ArgumentNullException(nameof(inutNFe)), configuracao);

        }
        /// <summary>
        /// Construtor
        /// </summary>
        public Inutilizacao() : base() { }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="inutNFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar(InutNFe inutNFe, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(inutNFe?.GerarXML() ?? throw new ArgumentNullException(nameof(inutNFe)), configuracao);
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
        /// <param name="inutNFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(InutNFe inutNFe, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(inutNFe?.GerarXML() ?? throw new System.ArgumentNullException(nameof(inutNFe)), configuracao);
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
                GravarXmlDistribuicao(pasta, ProcInutNFeResult.NomeArquivoDistribuicao, ProcInutNFeResult.GerarXML().OuterXml);
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
        public void GravarXmlDistribuicao(System.IO.Stream stream)
        {
            try
            {
                GravarXmlDistribuicao(stream, ProcInutNFeResult.GerarXML().OuterXml);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        #endregion Public Methods
    }
}