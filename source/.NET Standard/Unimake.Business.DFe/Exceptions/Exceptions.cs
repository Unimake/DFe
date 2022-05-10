using System;

namespace Unimake.Exceptions
{
    /// <summary>
    /// Classe de exceção quando o certificado digital não é localizado ou está com falha
    /// </summary>
    public class CertificadoDigitalException : Exception
    {
        /// <summary>
        /// Exceção quando o certificado digital não é localizado ou está com falha
        /// </summary>
        public CertificadoDigitalException()
            : base("Certificado digital não localizado ou o mesmo está com falha.") { }

    }

    /// <summary>
    /// Classe de exceção quando ocorre erros na validação dos XML
    /// </summary>
    public class ValidarXMLException : Exception
    {
        /// <summary>
        /// Exceção quando ocorre erros na validação dos XML
        /// </summary>
        /// <param name="message">Mensagem para exceção</param>
        public ValidarXMLException(string message)
            : base(message) { }
    }
}
