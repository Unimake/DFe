#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.AACBS;
using Modelo = Unimake.Business.DFe.Xml.AACBS.Autenticar;

namespace Unimake.Business.DFe.Servicos.AACBS
{
    /// <summary>Obtém ou reutiliza o token de acesso da credencial Receita Integra.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.AACBS.Autenticar")]
    [ComVisible(true)]
#endif
    public class Autenticar : ServicoBase, IInteropService<Modelo>
    {
        /// <summary>Objeto correspondente ao XML de envio.</summary>
        public Modelo Envio => LerEnvio<Modelo>();

        /// <summary>Resultado da operação ou erro local quando não há retorno disponível.</summary>
        public RetAutenticar Result => LerResultado<RetAutenticar>();

        internal override Servico Operacao => Servico.AACBSAutenticar;

        internal override RetornoAACBSBase ExecutarOperacao() =>
            ObterAutenticacao();

        /// <summary>Cria o serviço sem inicializar o envio.</summary>
        public Autenticar() { }

        /// <summary>Inicializa o serviço com o objeto de envio e a configuração de acesso.</summary>
        public Autenticar(Modelo xml, Configuracao configuracao) => InicializarServico(xml, configuracao);

        /// <summary>Inicializa o serviço com o XML de envio e a configuração de acesso.</summary>
        public Autenticar(string conteudoXML, Configuracao configuracao) => InicializarServico(conteudoXML, configuracao);

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
