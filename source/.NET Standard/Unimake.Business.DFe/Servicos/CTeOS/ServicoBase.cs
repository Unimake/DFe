#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CTeOS
{
    /// <summary>
    /// Classe base para consumo dos webservices do CTeOS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CTeOS.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase : NFe.ServicoBase
    {
        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public ServicoBase() : base() { }

        #endregion Public Constructors

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            var validar = new ValidarSchema();
            validar.Validar(ConteudoXML, TipoDFe.CTe.ToString() + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

            if (!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }
    }
}