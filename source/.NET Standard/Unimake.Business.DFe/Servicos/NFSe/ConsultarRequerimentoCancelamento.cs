#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;

namespace Unimake.DFe.Test.NFSe
{
    /// <summary>
    /// Consulta ao requerimento de cancelamento - Implementado em 23/11 para o Padrão AGILI
    /// </summary>
    public class ConsultarRequerimentoCancelamento : ConsultarNfse
    {
        /// <summary>
        /// Enviar o XML de Consulta da NFSe para o webservice
        /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.ConsultarNotaPrestador")]
    [ComVisible(true)]
#endif
        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarRequerimentoCancelamento() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarRequerimentoCancelamento(XmlDocument conteudoXML, Configuracao configuracao) : base(conteudoXML, configuracao) { }

    }
}
