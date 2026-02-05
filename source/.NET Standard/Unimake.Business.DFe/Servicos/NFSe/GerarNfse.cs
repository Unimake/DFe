#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Enviar o XML de NFSe para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.GerarNfse")]
    [ComVisible(true)]
#endif
    public class GerarNfse : ServicoBase
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public GerarNfse() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public GerarNfse(string conteudoXML, Configuracao configuracao) : this()
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
        public GerarNfse(XmlDocument conteudoXML, Configuracao configuracao) : this() => Inicializar(conteudoXML, configuracao);

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

        /// <summary>
        /// Resultado quando GerarNfse foi bem-sucedido (apenas para padrão NACIONAL).
        /// Retorna null se houve erro (usar ResultErro).
        /// </summary>
#if INTEROP
        [ComVisible(true)]
#endif
        public Xml.NFSe.NACIONAL.NFSe.NFSe Result
        {
            get
            {
                if (string.IsNullOrWhiteSpace(RetornoWSString))
                    return null;

                try
                {
                    var tagRaiz = RetornoWSXML.DocumentElement?.Name;
                    if (tagRaiz == "NFSe")
                    {
                        return XMLUtility.Deserializar<Xml.NFSe.NACIONAL.NFSe.NFSe>(RetornoWSXML);
                    }
                    return null;
                }
                catch
                {
                    return null;
                }
            }
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
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