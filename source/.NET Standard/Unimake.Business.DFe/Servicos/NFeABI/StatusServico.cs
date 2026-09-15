#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFeABI;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFeABI
{
    /// <summary>
    /// Consulta o status do serviço da NFeABI.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFeABI.StatusServico")]
    [ComVisible(true)]
#endif
    public class StatusServico : ServicoBase, IInteropService<ConsStatServNFeABI>
    {
        /// <summary>
        /// Define as configurações da consulta de status.
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ConsStatServNFeABI().LerXML<ConsStatServNFeABI>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.NFeABIStatusServico;
                Configuracoes.CodigoUF = (int)xml.CUF;
                Configuracoes.TipoAmbiente = xml.TpAmb;
                Configuracoes.SchemaVersao = xml.Versao;
                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Retorno tipado do serviço.
        /// </summary>
        public RetConsStatServNFeABI Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetConsStatServNFeABI>(CriarRetornoCompativel(RetornoWSString));
                }

                return new RetConsStatServNFeABI
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        /// <summary>
        /// Construtor.
        /// </summary>
        public StatusServico() : base() { }

        /// <summary>
        /// Construtor.
        /// </summary>
        /// <param name="consStatServNFeABI">XML tipado a enviar.</param>
        /// <param name="configuracao">Configuração do serviço.</param>
        public StatusServico(ConsStatServNFeABI consStatServNFeABI, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(consStatServNFeABI?.GerarXML() ?? throw new ArgumentNullException(nameof(consStatServNFeABI)), configuracao);
        }

        /// <summary>
        /// Construtor.
        /// </summary>
        /// <param name="conteudoXML">XML a enviar.</param>
        /// <param name="configuracao">Configuração do serviço.</param>
        public StatusServico(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);
            Inicializar(doc, configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa a consulta de status.
        /// </summary>
        /// <param name="consStatServNFeABI">XML tipado a enviar.</param>
        /// <param name="configuracao">Configuração do serviço.</param>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] ConsStatServNFeABI consStatServNFeABI, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                Inicializar(consStatServNFeABI?.GerarXML() ?? throw new ArgumentNullException(nameof(consStatServNFeABI)), configuracao ?? throw new ArgumentNullException(nameof(configuracao)));
                Executar();
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
            ThrowHelper.Instance.Throw(new Exception("Não existe XML de distribuição para consulta status do serviço."));
        }
    }
}
