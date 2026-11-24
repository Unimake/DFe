#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.AACBS;
using Modelo = Unimake.Business.DFe.Xml.AACBS.ConsultarSituacaoSolicitacao;

namespace Unimake.Business.DFe.Servicos.AACBS
{
    /// <summary>Consulta a situação de um tíquete da apuração assistida.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.AACBS.ConsultarSituacaoSolicitacao")]
    [ComVisible(true)]
#endif
    public class ConsultarSituacaoSolicitacao : ServicoBase, IInteropService<Modelo>
    {
        /// <summary>Objeto correspondente ao XML de envio.</summary>
        public Modelo Envio => LerEnvio<Modelo>();

        /// <summary>Resultado da operação ou erro local quando não há retorno disponível.</summary>
        public RetConsultarSituacaoSolicitacao Result => LerResultado<RetConsultarSituacaoSolicitacao>();

        internal override Servico Operacao => Servico.AACBSConsultarSituacaoSolicitacao;

        internal override RetornoAACBSBase ExecutarOperacao() =>
            EnviarFiscal(RotaTiquete(Envio.TiqueteSolicitacao), null, RespostaAACBS.Situacao);

        /// <summary>Cria o serviço sem inicializar o envio.</summary>
        public ConsultarSituacaoSolicitacao() { }

        /// <summary>Inicializa o serviço com o objeto de envio e a configuração de acesso.</summary>
        public ConsultarSituacaoSolicitacao(Modelo xml, Configuracao configuracao) => InicializarServico(xml, configuracao);

        /// <summary>Inicializa o serviço com o XML de envio e a configuração de acesso.</summary>
        public ConsultarSituacaoSolicitacao(string conteudoXML, Configuracao configuracao) => InicializarServico(conteudoXML, configuracao);

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
