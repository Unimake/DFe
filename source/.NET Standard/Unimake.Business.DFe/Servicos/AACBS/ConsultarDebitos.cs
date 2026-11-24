#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.AACBS;
using Modelo = Unimake.Business.DFe.Xml.AACBS.ConsultarDebitos;

namespace Unimake.Business.DFe.Servicos.AACBS
{
    /// <summary>Abre uma solicitação de débitos da CBS.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.AACBS.ConsultarDebitos")]
    [ComVisible(true)]
#endif
    public class ConsultarDebitos : ServicoBase, IInteropService<Modelo>
    {
        /// <summary>Objeto correspondente ao XML de envio.</summary>
        public Modelo Envio => LerEnvio<Modelo>();

        /// <summary>Resultado da operação ou erro local quando não há retorno disponível.</summary>
        public RetConsultarDebitos Result => LerResultado<RetConsultarDebitos>();

        internal override Servico Operacao => Servico.AACBSConsultarDebitos;

        internal override RetornoAACBSBase ExecutarOperacao() =>
            EnviarFiscal(Rota.Replace("{cnpj}", Envio.Cnpj),
                new Newtonsoft.Json.Linq.JObject { ["urlRetorno"] = Envio.UrlRetorno ?? string.Empty }.ToString(Newtonsoft.Json.Formatting.None),
                RespostaAACBS.Abertura);

        /// <summary>Cria o serviço sem inicializar o envio.</summary>
        public ConsultarDebitos() { }

        /// <summary>Inicializa o serviço com o objeto de envio e a configuração de acesso.</summary>
        public ConsultarDebitos(Modelo xml, Configuracao configuracao) => InicializarServico(xml, configuracao);

        /// <summary>Inicializa o serviço com o XML de envio e a configuração de acesso.</summary>
        public ConsultarDebitos(string conteudoXML, Configuracao configuracao) => InicializarServico(conteudoXML, configuracao);

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
