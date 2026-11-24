#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.AACBS;
using Modelo = Unimake.Business.DFe.Xml.AACBS.ConsultarLinkDownload;

namespace Unimake.Business.DFe.Servicos.AACBS
{
    /// <summary>Consulta a URL assinada de um arquivo de retorno.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.AACBS.ConsultarLinkDownload")]
    [ComVisible(true)]
#endif
    public class ConsultarLinkDownload : ServicoBase, IInteropService<Modelo>
    {
        /// <summary>Objeto correspondente ao XML de envio.</summary>
        public Modelo Envio => LerEnvio<Modelo>();

        /// <summary>Resultado da operação ou erro local quando não há retorno disponível.</summary>
        public RetConsultarLinkDownload Result => LerResultado<RetConsultarLinkDownload>();

        internal override Servico Operacao => Servico.AACBSConsultarLinkDownload;

        internal override RetornoAACBSBase ExecutarOperacao() =>
            EnviarFiscal(RotaTiquete(Envio.TiqueteSolicitacao), null, RespostaAACBS.Link);

        /// <summary>Cria o serviço sem inicializar o envio.</summary>
        public ConsultarLinkDownload() { }

        /// <summary>Inicializa o serviço com o objeto de envio e a configuração de acesso.</summary>
        public ConsultarLinkDownload(Modelo xml, Configuracao configuracao) => InicializarServico(xml, configuracao);

        /// <summary>Inicializa o serviço com o XML de envio e a configuração de acesso.</summary>
        public ConsultarLinkDownload(string conteudoXML, Configuracao configuracao) => InicializarServico(conteudoXML, configuracao);

#if INTEROP
        /// <summary>Inicializa e executa a operação pela interface COM.</summary>
        [ComVisible(true)]
        public void Executar(Modelo xml, Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
