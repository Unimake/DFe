using System;

namespace Unimake.Security.Exceptions
{
    /// <summary>
    /// Exceção certificado digital
    /// </summary>
    public class ExceptionCertificadoDigital: Exception
    {
        #region Public Constructors

        /// <summary>
        /// Exceção quando o certificado digital não é localizado
        /// </summary>
        public ExceptionCertificadoDigital()
            : base("Certificado digital não localizado ou o mesmo está com falha.")
        {
        }

        #endregion Public Constructors
    }
}