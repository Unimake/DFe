#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Net;
using System.Text.Json;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.AACBS;
using Modelo = Unimake.Business.DFe.Xml.AACBS.ConsultarRecolhimentos;

namespace Unimake.Business.DFe.Servicos.AACBS
{
    /// <summary>Abre uma solicitação de recolhimentos da CBS.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.AACBS.ConsultarRecolhimentos")]
    [ComVisible(true)]
#endif
    public class ConsultarRecolhimentos : ServicoBase, IInteropService<Modelo>
    {
        /// <summary>Objeto correspondente ao XML de envio.</summary>
        public Modelo Envio => LerEnvio<Modelo>();

        /// <summary>Resultado da operação ou erro local quando não há retorno disponível.</summary>
        public RetConsultarRecolhimentos Result => LerResultado<RetConsultarRecolhimentos>();

        internal override Servico Operacao => Servico.AACBSConsultarRecolhimentos;

        /// <summary>Cria o serviço sem inicializar o envio.</summary>
        public ConsultarRecolhimentos() { }

        /// <summary>Inicializa o serviço com o objeto de envio e a configuração de acesso.</summary>
        public ConsultarRecolhimentos(Modelo xml, Configuracao configuracao) => InicializarServico(xml, configuracao);

        /// <summary>Inicializa o serviço com o XML de envio e a configuração de acesso.</summary>
        public ConsultarRecolhimentos(string conteudoXML, Configuracao configuracao) => InicializarServico(conteudoXML, configuracao);

        internal override RetornoAACBSBase ExecutarOperacao()
        {
            var envio = Envio;
            var corpo = new JObject { ["urlRetorno"] = envio.UrlRetorno ?? string.Empty };
            return EnviarFiscal(Rota.Replace("{cnpj}", envio.Cnpj), corpo.ToString(Formatting.None), LerAbertura);
        }

        private static RetConsultarRecolhimentos LerAbertura(JsonElement json, HttpStatusCode status)
        {
            var resultado = RespostaAACBS.Abertura(json, status);
            return new RetConsultarRecolhimentos
            {
                TiqueteSolicitacao = resultado.TiqueteSolicitacao,
                TEASegundos = resultado.TEASegundos,
                CodigoErro = resultado.CodigoErro,
                MensagemErro = resultado.MensagemErro
            };
        }

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
