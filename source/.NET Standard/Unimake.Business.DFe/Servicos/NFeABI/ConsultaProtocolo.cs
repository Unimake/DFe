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
    /// Consulta a situação atual de uma NFeABI pela chave de acesso.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFeABI.ConsultaProtocolo")]
    [ComVisible(true)]
#endif
    public class ConsultaProtocolo : ServicoBase, IInteropService<ConsSitNFeABI>
    {
        /// <summary>
        /// Define as configurações da consulta de protocolo.
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ConsSitNFeABI().LerXML<ConsSitNFeABI>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.NFeABIConsultaProtocolo;
                Configuracoes.CodigoUF = Convert.ToInt32(xml.ChNFeABI.Substring(0, 2));
                Configuracoes.TipoAmbiente = xml.TpAmb;
                Configuracoes.Modelo = (ModeloDFe)int.Parse(xml.ChNFeABI.Substring(20, 2));
                Configuracoes.SchemaVersao = xml.Versao;
                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Retorno tipado da consulta de protocolo.
        /// </summary>
        public RetConsSitNFeABI Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetConsSitNFeABI>(CriarRetornoCompativel(RetornoWSString));
                }

                return new RetConsSitNFeABI
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        /// <summary>
        /// Construtor.
        /// </summary>
        public ConsultaProtocolo() : base() { }

        /// <summary>
        /// Construtor.
        /// </summary>
        /// <param name="consSitNFeABI">XML tipado a enviar.</param>
        /// <param name="configuracao">Configuração do serviço.</param>
        public ConsultaProtocolo(ConsSitNFeABI consSitNFeABI, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(consSitNFeABI?.GerarXML() ?? throw new ArgumentNullException(nameof(consSitNFeABI)), configuracao);
        }

        /// <summary>
        /// Construtor.
        /// </summary>
        /// <param name="conteudoXML">XML a enviar.</param>
        /// <param name="configuracao">Configuração do serviço.</param>
        public ConsultaProtocolo(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);
            Inicializar(doc, configuracao);
        }

        /// <summary>
        /// Construtor simplificado para consulta pela chave de acesso.
        /// </summary>
        /// <param name="chNFeABI">Chave de acesso da NFeABI.</param>
        /// <param name="tipoAmbiente">Ambiente de execução.</param>
        /// <param name="configuracao">Configuração do serviço.</param>
        public ConsultaProtocolo(string chNFeABI, TipoAmbiente tipoAmbiente, Configuracao configuracao) : this()
        {
            if (string.IsNullOrWhiteSpace(chNFeABI))
            {
                throw new ArgumentNullException(nameof(chNFeABI));
            }

            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var xml = new ConsSitNFeABI
            {
                Versao = "1.00",
                TpAmb = tipoAmbiente,
                XServ = "CONSULTAR",
                ChNFeABI = chNFeABI
            };

            Inicializar(xml.GerarXML(), configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa a consulta de protocolo.
        /// </summary>
        /// <param name="consSitNFeABI">XML tipado a enviar.</param>
        /// <param name="configuracao">Configuração do serviço.</param>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] ConsSitNFeABI consSitNFeABI, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                Inicializar(consSitNFeABI?.GerarXML() ?? throw new ArgumentNullException(nameof(consSitNFeABI)), configuracao ?? throw new ArgumentNullException(nameof(configuracao)));
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
            ThrowHelper.Instance.Throw(new Exception("Não existe XML de distribuição para consulta de protocolo."));
        }
    }
}
