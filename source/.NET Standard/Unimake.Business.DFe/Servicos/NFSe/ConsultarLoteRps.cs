#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Enviar o XML de Consulta Lote RPS para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.ConsultarLoteRps")]
    [ComVisible(true)]
#endif
    public class ConsultarLoteRps: ConsultarNfse
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarLoteRps() : base()
        { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarLoteRps(string conteudoXML, Configuracao configuracao) : this()
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
        public ConsultarLoteRps(XmlDocument conteudoXML, Configuracao configuracao) : base(conteudoXML, configuracao)
        { }
    }
}