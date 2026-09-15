#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;

namespace Unimake.Business.DFe.Lookup
{
    /// <summary>
    /// Exceção lançada por uma implementação de <see cref="IPessoaLookup"/> quando a consulta de
    /// pessoa/endereço devolve erro de negócio ou falha de transporte, e pelo <see cref="TomadorResolver"/>
    /// quando os dados devolvidos não bastam para montar um destinatário válido.
    /// </summary>
    [Serializable]
#if INTEROP
    [ComVisible(false)]
#endif
    public class PessoaLookupException : Exception
    {
        /// <summary>
        /// Código de erro devolvido pelo provedor (na API cpfcnpj.com.br, o campo <c>erroCodigo</c>), quando disponível.
        /// Vazio para falhas de transporte ou respostas sem código.
        /// </summary>
        public string CodigoErro { get; }

        /// <summary>
        /// Inicializa a exceção apenas com a mensagem.
        /// </summary>
        /// <param name="message">Mensagem descritiva do erro.</param>
        public PessoaLookupException(string message)
            : base(message) => CodigoErro = "";

        /// <summary>
        /// Inicializa a exceção com a mensagem e o código de erro do provedor.
        /// </summary>
        /// <param name="message">Mensagem descritiva do erro.</param>
        /// <param name="codigoErro">Código de erro devolvido pelo provedor.</param>
        public PessoaLookupException(string message, string codigoErro)
            : base(message) => CodigoErro = codigoErro ?? "";

        /// <summary>
        /// Inicializa a exceção com a mensagem e a exceção interna (falha de transporte, por exemplo).
        /// </summary>
        /// <param name="message">Mensagem descritiva do erro.</param>
        /// <param name="innerException">Exceção que originou a falha.</param>
        public PessoaLookupException(string message, Exception innerException)
            : base(message, innerException) => CodigoErro = "";
    }
}
