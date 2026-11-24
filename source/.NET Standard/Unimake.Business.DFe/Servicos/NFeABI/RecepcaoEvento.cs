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
    /// <summary>Envia um evento da NF-e ABI ao Ambiente Nacional.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFeABI.RecepcaoEvento")]
    [ComVisible(true)]
#endif
    public class RecepcaoEvento : ServicoBase, IInteropService<EventoNFeABI>
    {
        /// <summary>Cria o serviço para inicialização posterior.</summary>
        public RecepcaoEvento() { }

        /// <summary>Prepara o evento tipado para envio.</summary>
        /// <param name="evento">Evento da NF-e ABI.</param>
        /// <param name="configuracao">Configuração do serviço.</param>
        public RecepcaoEvento(EventoNFeABI evento, Configuracao configuracao) : this()
        {
            Inicializar(evento?.GerarXML() ?? throw new ArgumentNullException(nameof(evento)), configuracao);
        }

        /// <summary>Prepara um XML de evento para envio.</summary>
        /// <param name="conteudoXML">XML do evento.</param>
        /// <param name="configuracao">Configuração do serviço.</param>
        public RecepcaoEvento(string conteudoXML, Configuracao configuracao) : this()
        {
            var xml = new XmlDocument();
            xml.LoadXml(conteudoXML);
            Inicializar(xml, configuracao);
        }

        /// <summary>Evento preparado para envio, incluindo a assinatura.</summary>
        public EventoNFeABI EventoNFeABI => XMLUtility.Deserializar<EventoNFeABI>(ConteudoXMLAssinado);

        /// <summary>Retorno tipado do registro do evento.</summary>
        public RetEventoNFeABI Result
        {
            get
            {
                if (RetornoWSXML?.DocumentElement != null)
                {
                    return XMLUtility.Deserializar<RetEventoNFeABI>(RetornoWSXML);
                }

                return new RetEventoNFeABI
                {
                    InfEvento = new InfRetEventoNFeABI
                    {
                        CStat = 0,
                        XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                    }
                };
            }
        }

        /// <inheritdoc />
        protected override void DefinirConfiguracao()
        {
            var evento = XMLUtility.Deserializar<EventoNFeABI>(ConteudoXML);
            var informacoes = evento.InfEvento
                ?? throw new ValidarXMLException("O grupo obrigatório <infEvento> não foi localizado na NFeABI.");
            var codigoUF = (int)informacoes.COrgao;

            // O código 91 identifica o Ambiente Nacional no XML. O catálogo é carregado pela UF da chave.
            if (informacoes.COrgao == UFBrasil.AN)
            {
                var chave = informacoes.ChNFeABI;
                if (chave == null || chave.Length != 44 || !int.TryParse(chave.Substring(0, 2), out codigoUF))
                {
                    throw new ValidarXMLException("A chave da NFeABI não contém o código da UF necessário para a recepção do evento.");
                }
            }

            Configuracoes.Servico = Servico.NFeABIRecepcaoEvento;
            Configuracoes.CodigoUF = codigoUF;
            Configuracoes.TipoAmbiente = informacoes.TpAmb;
            Configuracoes.TipoEmissao = TipoEmissao.Normal;
            Configuracoes.Modelo = ModeloDFe.NFeABI;
            Configuracoes.SchemaVersao = evento.Versao;
            base.DefinirConfiguracao();
        }

#if INTEROP
        /// <summary>Prepara o evento tipado e transmite ao autorizador.</summary>
        /// <param name="evento">Evento da NF-e ABI.</param>
        /// <param name="configuracao">Configuração do serviço.</param>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] EventoNFeABI evento, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                Inicializar(evento?.GerarXML() ?? throw new ArgumentNullException(nameof(evento)), configuracao);
                Executar();
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }
#endif
    }
}
