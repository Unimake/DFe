#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Consultar RPS disponivel para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.ConsultarRpsDisponivel")]
    [ComVisible(true)]
#endif
    public class ConsultarRpsDisponivel : ConsultarNfse
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarRpsDisponivel() : base()
        { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarRpsDisponivel(XmlDocument conteudoXML, Configuracao configuracao) : base(conteudoXML, configuracao)
        { }
    }
}