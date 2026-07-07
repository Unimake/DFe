#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.IO;
using System.Text;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.BPe
{
    /// <summary>
    /// Classe base para os serviços do BPe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.BPe.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase : Servicos.ServicoBase
    {
        /// <summary>
        /// Construtor
        /// </summary>
        protected ServicoBase() : base() { }

        /// <summary>
        /// Definir configurações
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            Configuracoes.TipoDFe = TipoDFe.BPe;
            Configuracoes.Load(GetType().Name);
        }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            XmlValidarConteudo();

            var resultadoValidacao = ValidarXMLCentralizado();

            if (!resultadoValidacao.Validado)
            {
                throw new ValidarXMLException(resultadoValidacao.MensagemRetorno);
            }
        }

        /// <summary>
        /// Validar o conteúdo das tags do XML.
        /// </summary>
        protected override void XmlValidarConteudo() { }

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar() => base.Executar();

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
                streamWriter = File.CreateText(Path.Combine(pasta, nomeArquivo));
                streamWriter.Write(conteudoXML);
            }
            finally
            {
                if (streamWriter != null)
                {
                    streamWriter.Close();
                }
            }
        }

#if INTEROP

        /// <summary>
        /// Gravar o XML de distribuição em uma pasta no HD
        /// </summary>
        /// <param name="pasta">Pasta onde deve ser gravado o XML no HD</param>
        /// <param name="nomeArquivo">Nome do arquivo a ser gravado no HD</param>
        /// <param name="conteudoXML">String contendo o conteúdo do XML a ser gravado no HD</param>
        public void GravarXmlDistribuicaoComConteudo(string pasta, string nomeArquivo, string conteudoXML) => GravarXmlDistribuicao(pasta, nomeArquivo, conteudoXML);

#endif

        /// <summary>
        /// Gravar o XML de distribuição em um stream
        /// </summary>
        /// <param name="stream">Stream que vai receber o conteúdo do XML</param>
        /// <param name="value">Conteúdo a ser gravado no stream</param>
        /// <param name="encoding">Define o encoding do stream, caso não informado será usado UTF8</param>
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
    }
}
