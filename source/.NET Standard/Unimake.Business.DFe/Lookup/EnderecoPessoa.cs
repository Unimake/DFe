#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;

namespace Unimake.Business.DFe.Lookup
{
    /// <summary>
    /// Endereço neutro devolvido por uma consulta de pessoa. Não depende de nenhum
    /// tipo de XML de DFe; os mapeadores (ex.: <see cref="TomadorResolver"/>) convertem
    /// estes campos para o grupo de endereço do documento fiscal desejado.
    /// </summary>
    [Serializable]
#if INTEROP
    [ComVisible(false)]
#endif
    public class EnderecoPessoa
    {
        /// <summary>
        /// Logradouro (rua, avenida etc.).
        /// </summary>
        public string Logradouro { get; set; }

        /// <summary>
        /// Número do imóvel.
        /// </summary>
        public string Numero { get; set; }

        /// <summary>
        /// Complemento do endereço.
        /// </summary>
        public string Complemento { get; set; }

        /// <summary>
        /// Bairro.
        /// </summary>
        public string Bairro { get; set; }

        /// <summary>
        /// CEP, somente dígitos.
        /// </summary>
        public string Cep { get; set; }

        /// <summary>
        /// Nome do município.
        /// </summary>
        public string Municipio { get; set; }

        /// <summary>
        /// Sigla da unidade federativa (ex.: <c>SP</c>).
        /// </summary>
        public string Uf { get; set; }

        /// <summary>
        /// Código IBGE do município (7 dígitos). Zero quando não informado.
        /// </summary>
        public int CodigoMunicipioIbge { get; set; }

        /// <summary>
        /// Nome do país. Padrão <c>Brasil</c>.
        /// </summary>
        public string Pais { get; set; } = "Brasil";
    }
}
