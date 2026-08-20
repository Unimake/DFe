#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Enviar o XML de Cancelamento da NFSe para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.CancelarNfse")]
    [ComVisible(true)]
#endif
    public class CancelarNfse : ServicoBase
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public CancelarNfse() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public CancelarNfse(string conteudoXML, Configuracao configuracao) : this()
        {
            var xmlDoc = new XmlDocument();
            xmlDoc.LoadXml(conteudoXML);

            Inicializar(xmlDoc, configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public CancelarNfse(XmlDocument conteudoXML, Configuracao configuracao) : this() => Inicializar(conteudoXML, configuracao);

        /// <summary>
        /// Obtém a tag de configuração correspondente ao serviço de cancelamento solicitado.
        /// </summary>
        /// <returns>Nome da tag do serviço.</returns>
        protected override string ObterNomeTagServico()
        {
            if (GetType() != typeof(CancelarNfse))
            {
                return base.ObterNomeTagServico();
            }

            switch (Configuracoes.Servico)
            {
                case Servico.NFSeCancelarNfse:
                    return nameof(CancelarNfse);

                case Servico.NFSeCancelamentoNfe:
                    return "CancelamentoNfe";

                case Servico.NFSeCancelarNotaFiscal:
                    return "CancelarNotaFiscal";

                default:
                    throw new System.InvalidOperationException(
                        $"O serviço {Configuracoes.Servico} não pode ser executado pela classe {nameof(CancelarNfse)}. " +
                        $"Serviços aceitos: {Servico.NFSeCancelarNfse}, {Servico.NFSeCancelamentoNfe} e " +
                        $"{Servico.NFSeCancelarNotaFiscal}.");
            }
        }

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                base.DefinirConfiguracao();
            }
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        [ComVisible(true)]
        public override void Executar(string conteudoXML, Configuracao configuracao) => base.Executar(conteudoXML, configuracao);

#endif

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            if (!Configuracoes.Definida)
            {
                DefinirConfiguracao();
            }

            base.Executar();
        }
    }
}
