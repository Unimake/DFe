using System;
using System.Runtime.InteropServices;

namespace Unimake.Exceptions
{
    /// <summary>
    /// Classe de exceção quando o certificado digital não é localizado ou está com falha
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Exceptions.CertificadoDigitalException")]
    [ComVisible(true)]
#endif
    public class CertificadoDigitalException : Exception
    {
        /// <summary>
        /// Exceção quando o certificado digital não é localizado ou está com falha
        /// </summary>
        public CertificadoDigitalException()
            : base("Certificado digital não localizado ou o mesmo está com falha.") => HResult = (int)ErrorCodes.CertificadoDigitalNaoLocalizado;

        /// <summary>
        /// Exceção quando o certificado digital não é localizado ou está com falha
        /// </summary>
        /// <param name="message">Mensagem de exceção</param>
        public CertificadoDigitalException(string message)
            : base(message) => HResult = (int)ErrorCodes.CertificadoDigitalNaoLocalizado;

        /// <summary>
        /// Exceção quando o certificado digital não é localizado ou está com falha
        /// </summary>
        /// <param name="message">Mensagem de exceção</param>
        /// <param name="errorCode">Erro ocorrido</param>
        public CertificadoDigitalException(string message, ErrorCodes errorCode)
            : base(message) => HResult = (int)errorCode;
    }

    /// <summary>
    /// Classe de exceção quando ocorre erros de validação de regras do DFe (NFe, CTe, MDFe, NFCe, etc...)
    /// </summary>
    public class ValidatorDFeException : Exception
    {
        /// <summary>
        /// Exceção quando ocorre erros na validação de regras do DFe (NFe, CTe, MDFe, NFCe, etc...)
        /// </summary>
        /// <param name="message">Mensagem para exceção</param>
        public ValidatorDFeException(string message) : base(message) => HResult = (int)ErrorCodes.ValidatorDFe;
    }

    /// <summary>
    /// Classe de exceção quando ocorre erros na validação dos XML contra schema (arquivo XSD)
    /// </summary>
    public class ValidarXMLException : Exception
    {
        /// <summary>
        /// Exceção quando ocorre erros na validação dos XML
        /// </summary>
        /// <param name="message">Mensagem para exceção</param>
        public ValidarXMLException(string message)
            : base(message) => HResult = (int)ErrorCodes.ValidacaoSchemaXML;
    }

    /// <summary>
    /// Códigos de erros das exceções geradas na DLL. Útil para outras linguagens (INTEROP)
    /// </summary>
    public enum ErrorCodes
    {
        /// <summary>
        /// Certificado digital não localizado ou o mesmo está com falha
        /// </summary>
        CertificadoDigitalNaoLocalizado = 1,

        /// <summary>
        /// Erro de validação do XML contra o schema (arquivo XSD)
        /// </summary>
        ValidacaoSchemaXML = 2,

        /// <summary>
        /// Erro de validação de diversas das regras dos documentos fiscais eletrônicos (NFe, CTe, MDFe, NFCe, etc...). Validação realizada pelo Validator da DLL Unimake.DFe.
        /// </summary>
        ValidatorDFe = 3,

        /// <summary>
        /// Senha do certificado digital está incorreta
        /// </summary>
        SenhaCertificadoIncorreta = 4
    }

    /// <summary>
    /// Exceção ao carregar o certificado digital
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Exceptions.CarregarCertificadoException")]
    [ComVisible(true)]
#endif
    public class CarregarCertificadoException : Exception
    {
        #region Public Constructors

        /// <summary>
        /// Falha ao carregar certificado digital
        /// </summary>
        /// <param name="message">Mensagem da exceção</param>
        public CarregarCertificadoException(string message)
            : base(message)
        {
        }

        #endregion Public Constructors
    }
}
