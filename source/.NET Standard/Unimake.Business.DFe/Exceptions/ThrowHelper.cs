using System;
using System.Runtime.InteropServices;

namespace Unimake.Exceptions
{
    /// <summary>
    /// Classe para capturar dados da exception para disponibilizar a outras linguagens de programação, que não .NET, com tipos simples.
    /// </summary>
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Exceptions.ThrowHelper")]
    [ComVisible(true)]
    public class ThrowHelper
    {
        #region Private Fields

        private static ThrowHelper _instance;

        #endregion Private Fields

        #region Public Properties

        /// <summary>
        /// Instancia estática da ThrowHelper para receber a mensagem original da exceção
        /// </summary>
        public static ThrowHelper Instance => _instance ?? (_instance = new ThrowHelper());

        /// <summary>
        /// Mensagem da exceção gerada
        /// </summary>
        public string Message { get; private set; }

        #endregion Public Properties

        #region Public Methods

        /// <summary>
        /// Recuperar o conteúdo da mensagem de exceção
        /// </summary>
        public string GetMessage() => Instance.Message ?? "";

        /// <summary>
        /// Setar a mensagem de erro da exceção para que outras linguagens consigam pegar o erro através desta propriedade
        /// </summary>
        /// <param name="ex">Exception</param>
        public void Throw(Exception ex)
        {
            Instance.Message = ex.GetLastException().Message;
            throw ex;
        }

        #endregion Public Methods
    }
}