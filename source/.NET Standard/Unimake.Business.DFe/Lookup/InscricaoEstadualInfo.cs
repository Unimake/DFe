#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;

namespace Unimake.Business.DFe.Lookup
{
    /// <summary>
    /// Inscrição estadual devolvida pelo pacote 16 da API cpfcnpj.com.br,
    /// já associada à unidade federativa correspondente.
    /// </summary>
    [Serializable]
#if INTEROP
    [ComVisible(false)]
#endif
    public class InscricaoEstadualInfo
    {
        /// <summary>
        /// Número da inscrição estadual, somente dígitos.
        /// </summary>
        public string InscricaoEstadual { get; set; }

        /// <summary>
        /// Indica se a inscrição estadual está ativa.
        /// </summary>
        public bool Ativo { get; set; }

        /// <summary>
        /// Sigla da unidade federativa da inscrição (ex.: <c>MG</c>).
        /// </summary>
        public string Uf { get; set; }

        /// <summary>
        /// Código IBGE do estado (2 dígitos). Zero quando não informado.
        /// </summary>
        public int CodigoEstadoIbge { get; set; }
    }
}
