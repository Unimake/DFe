#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.IO;
using System.Text;
using Unimake.Business.DFe.Security;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFe
{
    /// <summary>
    /// Classe base para os serviços da NFe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFe.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase : Servicos.ServicoBase
    {
        #region Protected Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        protected ServicoBase() : base() { }

        #endregion Protected Constructors

        #region Protected Methods

        /// <summary>
        /// Definir configurações
        /// </summary>
        protected override void DefinirConfiguracao() { }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            XmlValidarConteudo(); // Efetuar a validação antes de validar schema para evitar alguns erros que não ficam claros para o desenvolvedor.

            var validar = new ValidarSchema();
            validar.Validar(ConteudoXML, Configuracoes.TipoDFe.ToString() + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

            if (!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }

        /// <summary>
        /// Validar, o conteúdo das tags do XML, alguns validações manuais que o schema não faz. Vamos implementando novas regras na medida da necessidade de cada serviço.
        /// </summary>
        protected override void XmlValidarConteudo() { }

        #endregion Protected Methods

        #region Public Methods

        /// <summary>
        /// Executar o serviço
        /// </summary>       
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            if (!string.IsNullOrWhiteSpace(Configuracoes.TagAssinatura) &&
               !AssinaturaDigital.EstaAssinado(ConteudoXML, Configuracoes.TagAssinatura))
            {
                AssinaturaDigital.Assinar(ConteudoXML, Configuracoes.TagAssinatura, Configuracoes.TagAtributoID, Configuracoes.CertificadoDigital, AlgorithmType.Sha1, true, "Id");
            }

            AjustarXMLAposAssinado();

            XmlValidar();

            base.Executar();
        }

        /// <summary>
        /// Gravar o XML de distribuição em uma pasta no HD
        /// </summary>
        /// <param name="pasta">Pasta onde deve ser gravado o XML no HD</param>
        /// <param name="nomeArquivo">Nome do arquivo a ser gravado no HD</param>
        /// <param name="conteudoXML">String contendo o conteúdo do XML a ser gravado no HD</param>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            StreamWriter streamWriter = null;

            try
            {
                var conteudoXmlDistribuicao = conteudoXML;

                streamWriter = File.CreateText(Path.Combine(pasta, nomeArquivo));
                streamWriter.Write(conteudoXmlDistribuicao);
            }
            finally
            {
                if (streamWriter != null)
                {
                    streamWriter.Close();
                }
            }
        }

        /// <summary>
        /// Gravar o XML de distribuição em um stream
        /// </summary>
        /// <param name="value">Conteúdo a ser gravado no stream</param>
        /// <param name="stream">Stream que vai receber o conteúdo do XML</param>
        /// <param name="encoding">Define o encoding do stream, caso não informado ,será usado o UTF8</param>
#if INTEROP
        [ComVisible(false)]
#endif
        public virtual void GravarXmlDistribuicao(Stream stream, string value, Encoding encoding = null)
        {
            if (stream is null)
            {
                throw new ArgumentNullException(nameof(stream));
            }

            if (string.IsNullOrEmpty(value))
            {
                throw new ArgumentNullException(nameof(value));
            }

            if (encoding == null)
            {
                encoding = Encoding.UTF8;
            }

            var byteData = encoding.GetBytes(value);
            stream.Write(byteData, 0, byteData.Length);
            stream.Close();
        }

        #endregion Public Methods
    }
}