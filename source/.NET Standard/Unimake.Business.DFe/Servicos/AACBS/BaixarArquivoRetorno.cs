#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.AACBS;
using Modelo = Unimake.Business.DFe.Xml.AACBS.BaixarArquivoRetorno;

namespace Unimake.Business.DFe.Servicos.AACBS
{
    /// <summary>Baixa o arquivo de débitos a partir da URL assinada.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.AACBS.BaixarArquivoRetorno")]
    [ComVisible(true)]
#endif
    public class BaixarArquivoRetorno : ServicoBase, IInteropService<Modelo>
    {
        /// <summary>Objeto correspondente ao XML de envio.</summary>
        public Modelo Envio => LerEnvio<Modelo>();

        /// <summary>Resultado da operação ou erro local quando não há retorno disponível.</summary>
        public RetBaixarArquivoRetorno Result => LerResultado<RetBaixarArquivoRetorno>();

        internal override Servico Operacao => Servico.AACBSBaixarArquivoRetorno;

        internal override RetornoAACBSBase ExecutarOperacao() =>
            Baixar(Envio);

        /// <summary>Cria o serviço sem inicializar o envio.</summary>
        public BaixarArquivoRetorno() { }

        /// <summary>Inicializa o serviço com o objeto de envio e a configuração de acesso.</summary>
        public BaixarArquivoRetorno(Modelo xml, Configuracao configuracao) => InicializarServico(xml, configuracao);

        /// <summary>Inicializa o serviço com o XML de envio e a configuração de acesso.</summary>
        public BaixarArquivoRetorno(string conteudoXML, Configuracao configuracao) => InicializarServico(conteudoXML, configuracao);

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
