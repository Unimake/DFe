#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;

namespace Unimake.Business.DFe.Servicos.CTe
{
    /// <summary>
    /// Classe base dos serviços de CTe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CTe.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase: NFe.ServicoBase
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ServicoBase(XmlDocument conteudoXML, Configuracao configuracao)
            : base(conteudoXML, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public ServicoBase()
            : base()
        {
        }

        #endregion Public Constructors
    }
}