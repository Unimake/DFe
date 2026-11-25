#if INTEROP
using System.Runtime.InteropServices;
#endif
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.AACBS;
using Modelo = Unimake.Business.DFe.Xml.AACBS.GerarDarfAdquirente;

namespace Unimake.Business.DFe.Servicos.AACBS
{
    /// <summary>Solicita a geração de DARF pelo adquirente para um ou mais DF-e.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.AACBS.GerarDarfAdquirente")]
    [ComVisible(true)]
#endif
    public class GerarDarfAdquirente : ServicoBase, IInteropService<Modelo>
    {
        /// <summary>Objeto correspondente ao XML de envio.</summary>
        public Modelo Envio => LerEnvio<Modelo>();

        /// <summary>Resultado global da operação ou erro local quando não há retorno disponível.</summary>
        public RetGerarDarfAdquirente Result => LerResultado<RetGerarDarfAdquirente>();

        internal override Servico Operacao => Servico.AACBSGerarDarfAdquirente;

        internal override RetornoAACBSBase ExecutarOperacao()
        {
            var envio = Envio;
            var corpo = new JArray();
            foreach (var solicitacao in envio.Solicitacoes)
            {
                var item = new JObject
                {
                    ["chaveDFE"] = solicitacao.ChaveDFE,
                    ["opcaoPagamento"] = solicitacao.OpcaoPagamento
                };
                if (solicitacao.OpcaoPagamento == "PAGAMENTO_PARCIAL")
                    item["valorDarfParcial"] = solicitacao.ValorDarfParcial.Value;

                corpo.Add(item);
            }
            return EnviarFiscal(Rota.Replace("{cnpj}", envio.Cnpj), corpo.ToString(Formatting.None),
                RespostaAACBS.DarfAdquirente);
        }

        /// <summary>Cria o serviço sem inicializar o envio.</summary>
        public GerarDarfAdquirente() { }

        /// <summary>Inicializa o serviço com o objeto de envio e a configuração de acesso.</summary>
        public GerarDarfAdquirente(Modelo xml, Configuracao configuracao) => InicializarServico(xml, configuracao);

        /// <summary>Inicializa o serviço com o XML de envio e a configuração de acesso.</summary>
        public GerarDarfAdquirente(string conteudoXML, Configuracao configuracao) => InicializarServico(conteudoXML, configuracao);

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
