#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Consultar URL da Nfse no webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.ConsultarUrlNfse")]
    [ComVisible(true)]
#endif
    public class ConsultarUrlNfse : ConsultarNfse
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarUrlNfse() : base()
        { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarUrlNfse(XmlDocument conteudoXML, Configuracao configuracao) : base(conteudoXML, configuracao)
        { }
    }
}