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
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.SolicitacaoInutilizacao")]
    [ComVisible(true)]
#endif
    public class SolicitacaoInutilizacao : CancelarNfse
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public SolicitacaoInutilizacao() : base()
        { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public SolicitacaoInutilizacao(XmlDocument conteudoXML, Configuracao configuracao) : base(conteudoXML, configuracao)
        { }
    }
}