#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.IO;
using System.Text;
using System.Xml;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFeABI
{
    /// <summary>
    /// Classe base para os serviços da NFeABI.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFeABI.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase : Servicos.ServicoBase
    {
        /// <summary>
        /// Construtor.
        /// </summary>
        protected ServicoBase() : base() { }

        /// <summary>
        /// Define as configurações do serviço.
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            Configuracoes.TipoDFe = TipoDFe.NFeABI;
            Configuracoes.Load(GetType().Name);
        }

        /// <summary>
        /// Valida o XML do serviço.
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
        /// Valida o conteúdo das tags do XML.
        /// </summary>
        protected override void XmlValidarConteudo() { }

        /// <summary>
        /// Cria uma cópia do retorno compatível com a sigla de UF atualmente devolvida em homologação.
        /// </summary>
        /// <param name="retornoXml">XML bruto recebido do serviço.</param>
        /// <returns>Cópia do XML com tags cUF normalizadas para o código IBGE.</returns>
        protected XmlDocument CriarRetornoCompativel(string retornoXml)
        {
            var retorno = new XmlDocument();
            retorno.LoadXml(retornoXml);

            var namespaceManager = new XmlNamespaceManager(retorno.NameTable);
            namespaceManager.AddNamespace("nfeabi", "http://www.portalfiscal.inf.br/nfeabi");

            foreach (XmlNode tagCUF in retorno.SelectNodes("//nfeabi:cUF", namespaceManager))
            {
                if (!int.TryParse(tagCUF.InnerText, out _))
                {
                    UFBrasil uf;
                    if (Enum.TryParse(tagCUF.InnerText, true, out uf))
                    {
                        tagCUF.InnerText = ((int)uf).ToString();
                    }
                }
            }

            return retorno;
        }

        /// <summary>
        /// Executa o serviço.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar() => base.Executar();

        /// <summary>
        /// Grava o XML de distribuição em uma pasta.
        /// </summary>
        /// <param name="pasta">Pasta de destino.</param>
        /// <param name="nomeArquivo">Nome do arquivo.</param>
        /// <param name="conteudoXML">Conteúdo XML.</param>
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
        /// Grava o XML de distribuição em uma pasta.
        /// </summary>
        /// <param name="pasta">Pasta de destino.</param>
        /// <param name="nomeArquivo">Nome do arquivo.</param>
        /// <param name="conteudoXML">Conteúdo XML.</param>
        public void GravarXmlDistribuicaoComConteudo(string pasta, string nomeArquivo, string conteudoXML) => GravarXmlDistribuicao(pasta, nomeArquivo, conteudoXML);
#endif

        /// <summary>
        /// Grava o XML de distribuição em um stream.
        /// </summary>
        /// <param name="stream">Stream de destino.</param>
        /// <param name="value">Conteúdo XML.</param>
        /// <param name="encoding">Codificação; UTF-8 quando não informada.</param>
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

            encoding = encoding ?? Encoding.UTF8;
            var byteData = encoding.GetBytes(value);
            stream.Write(byteData, 0, byteData.Length);
            stream.Close();
        }
    }
}
