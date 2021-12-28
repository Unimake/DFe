using System;

namespace Unimake.Security.Exceptions
{
    /// <summary>
    /// Classe de exceção quando o certificado digital não é localizado ou está com falha
    /// </summary>
    public class ExceptionCertificadoDigital: Exception
    {
        /// <summary>
        /// Exceção quando o certificado digital não é localizado ou está com falha
        /// </summary>
        public ExceptionCertificadoDigital()
            : base("Certificado digital não localizado ou o mesmo está com falha.") { }

    }

    /// <summary>
    /// Classe de exceção quando ocorre erros na validação dos XML
    /// </summary>
    public class ExceptionValidacaoXML: Exception
    {
        /// <summary>
        /// Exceção quando ocorre erros na validação dos XML
        /// </summary>
        /// <param name="message">Mensagem para exceção</param>
        public ExceptionValidacaoXML(string message)
            : base(message) { }
    }
}