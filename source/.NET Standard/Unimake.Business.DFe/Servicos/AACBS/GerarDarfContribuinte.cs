#if INTEROP
using System.Runtime.InteropServices;
#endif
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.AACBS;
using Modelo = Unimake.Business.DFe.Xml.AACBS.GerarDarfContribuinte;

namespace Unimake.Business.DFe.Servicos.AACBS
{
    /// <summary>Solicita a geração de DARF do contribuinte para um período de apuração.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.AACBS.GerarDarfContribuinte")]
    [ComVisible(true)]
#endif
    public class GerarDarfContribuinte : ServicoBase, IInteropService<Modelo>
    {
        /// <summary>Objeto correspondente ao XML de envio.</summary>
        public Modelo Envio => LerEnvio<Modelo>();

        /// <summary>Resultado da operação ou erro local quando não há retorno disponível.</summary>
        public RetGerarDarfContribuinte Result => LerResultado<RetGerarDarfContribuinte>();

        internal override Servico Operacao => Servico.AACBSGerarDarfContribuinte;

        internal override RetornoAACBSBase ExecutarOperacao()
        {
            var envio = Envio;
            var corpo = new JObject
            {
                ["pa"] = envio.Pa,
                ["opcaoPagamento"] = envio.OpcaoPagamento
            };
            if (envio.OpcaoPagamento == "PAGAMENTO_PARCIAL")
                corpo["valorDarfParcial"] = envio.ValorDarfParcial.Value;

            return EnviarFiscal(Rota.Replace("{cnpj}", envio.Cnpj), corpo.ToString(Formatting.None),
                RespostaAACBS.DarfContribuinte);
        }

        /// <summary>Cria o serviço sem inicializar o envio.</summary>
        public GerarDarfContribuinte() { }

        /// <summary>Inicializa o serviço com o objeto de envio e a configuração de acesso.</summary>
        public GerarDarfContribuinte(Modelo xml, Configuracao configuracao) => InicializarServico(xml, configuracao);

        /// <summary>Inicializa o serviço com o XML de envio e a configuração de acesso.</summary>
        public GerarDarfContribuinte(string conteudoXML, Configuracao configuracao) => InicializarServico(conteudoXML, configuracao);

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
