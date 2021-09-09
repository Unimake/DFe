using System.Xml;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Enviar o XML de NFSe para o webservice
    /// </summary>
    public class RecepcionarLoteRpsSincrono: GerarNfse
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public RecepcionarLoteRpsSincrono() : base()
        { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public RecepcionarLoteRpsSincrono(XmlDocument conteudoXML, Configuracao configuracao) : base(conteudoXML, configuracao)
        { }
    }
}