#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Enviar o XML de Consulta NFSe por RPS para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfsePorRps")]
    [ComVisible(true)]
#endif
    public class ConsultarNfsePorRps: ConsultarNfse
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarNfsePorRps() : base()
        { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarNfsePorRps(string conteudoXML, Configuracao configuracao) : this()
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
        public ConsultarNfsePorRps(XmlDocument conteudoXML, Configuracao configuracao) : base(conteudoXML, configuracao)
        { }

        ///<summary>
        ///Resultado da consutla de NFSe por RPS - Apenas para padrão NACIONAL
        ///Em caso de sucesso, a tag chaveAcesso será preenchida com a chave de acesso da NFSe consultada (50 dígitos).
        ///Em caso de erro, a propriedade Erro será preenchida com as informações do erro ocorrido.
        /// </summary>
#if INTEROP
        [ComVisible(true)]
#endif
        public Xml.NFSe.NACIONAL.RetConsultarNfsePorRps Result
        {
            get
            {
                try
                {
                    var tagRaiz = RetornoWSXML.DocumentElement?.Name;
                    if(tagRaiz == "temp")
                    {
                        return XMLUtility.Deserializar<Xml.NFSe.NACIONAL.RetConsultarNfsePorRps>(RetornoWSXML);
                    }
                    return null;
                }
                catch
                {
                    return null;
                }
            }
        }
    }
}