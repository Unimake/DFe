using System.Xml;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Enviar o XML de Consulta NFSe de Serviços Prestados para o webservice
    /// </summary>
    public class ConsultarNfseServicoPrestado: ConsultarNfse
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarNfseServicoPrestado() : base()
        { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarNfseServicoPrestado(XmlDocument conteudoXML, Configuracao configuracao) : base(conteudoXML, configuracao)
        { }
    }
}