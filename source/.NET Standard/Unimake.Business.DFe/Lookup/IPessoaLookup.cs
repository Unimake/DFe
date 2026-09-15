#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

namespace Unimake.Business.DFe.Lookup
{
    /// <summary>
    /// Contrato de consulta de pessoa/endereço. Permite injetar uma implementação
    /// alternativa ou um teste offline no lugar da API real. A implementação de
    /// referência é <see cref="CpfCnpjComBrLookup"/>.
    /// </summary>
    /// <remarks>
    /// Este namespace é somente .NET (sem exposição COM/INTEROP), por usar API assíncrona baseada em Task.
    /// </remarks>
#if INTEROP
    [ComVisible(false)]
#endif
    public interface IPessoaLookup
    {
        /// <summary>
        /// Consulta um CPF.
        /// </summary>
        /// <param name="cpf">CPF a consultar (com ou sem máscara).</param>
        /// <param name="comEndereco">
        /// Quando <c>true</c>, traz também o endereço; quando <c>false</c>,
        /// apenas o nome (na implementação de referência, pacotes 3 e 1 da API cpfcnpj.com.br).
        /// </param>
        /// <param name="cancellationToken">Token de cancelamento.</param>
        /// <returns>Resultado neutro com os dados encontrados.</returns>
        Task<ResultadoPessoa> ConsultarCpfAsync(string cpf, bool comEndereco, CancellationToken cancellationToken = default(CancellationToken));

        /// <summary>
        /// Consulta um CNPJ.
        /// </summary>
        /// <param name="cnpj">CNPJ a consultar (com ou sem máscara).</param>
        /// <param name="comSimplesNacional">
        /// Quando <c>true</c>, traz também a situação cadastral e do Simples Nacional;
        /// quando <c>false</c>, apenas razão, fantasia e endereço da matriz (na implementação de referência, pacotes 6 e 5).
        /// </param>
        /// <param name="cancellationToken">Token de cancelamento.</param>
        /// <returns>Resultado neutro com os dados encontrados.</returns>
        Task<ResultadoPessoa> ConsultarCnpjAsync(string cnpj, bool comSimplesNacional, CancellationToken cancellationToken = default(CancellationToken));

        /// <summary>
        /// Consulta as inscrições estaduais de um CNPJ (na implementação de referência, pacote 16).
        /// </summary>
        /// <param name="cnpj">CNPJ a consultar (com ou sem máscara).</param>
        /// <param name="cancellationToken">Token de cancelamento.</param>
        /// <returns>Lista de inscrições estaduais, possivelmente vazia. Nunca nula.</returns>
        Task<IReadOnlyList<InscricaoEstadualInfo>> ConsultarInscricoesEstaduaisAsync(string cnpj, CancellationToken cancellationToken = default(CancellationToken));
    }
}
