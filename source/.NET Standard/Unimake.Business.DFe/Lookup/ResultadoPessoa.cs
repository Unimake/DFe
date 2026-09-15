#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;

namespace Unimake.Business.DFe.Lookup
{
    /// <summary>
    /// Resultado neutro de uma consulta de pessoa (física ou jurídica) na API cpfcnpj.com.br.
    /// É um DTO independente de qualquer tipo de XML de DFe. Os mapeadores por documento
    /// (ex.: <see cref="TomadorResolver"/>) consomem este objeto para preencher o destinatário.
    /// </summary>
    [Serializable]
#if INTEROP
    [ComVisible(false)]
#endif
    public class ResultadoPessoa
    {
        /// <summary>
        /// Documento consultado (CPF ou CNPJ), somente dígitos.
        /// </summary>
        public string Documento { get; set; }

        /// <summary>
        /// Indica se o documento é de pessoa física (CPF). Falso para pessoa jurídica (CNPJ).
        /// </summary>
        public bool PessoaFisica { get; set; }

        /// <summary>
        /// Nome (pessoa física) ou razão social (pessoa jurídica).
        /// </summary>
        public string Nome { get; set; }

        /// <summary>
        /// Nome fantasia. Preenchido apenas para pessoa jurídica.
        /// </summary>
        public string NomeFantasia { get; set; }

        /// <summary>
        /// Endereço principal. Nulo quando a consulta não retorna endereço (ex.: pacote 1).
        /// </summary>
        public EnderecoPessoa Endereco { get; set; }

        /// <summary>
        /// Endereços adicionais devolvidos pela consulta (quando houver). Nunca nulo.
        /// </summary>
        public List<EnderecoPessoa> EnderecosAdicionais { get; set; } = new List<EnderecoPessoa>();

        /// <summary>
        /// Indica se a pessoa jurídica é optante pelo Simples Nacional (pacote 6).
        /// Falso quando o dado não foi consultado.
        /// </summary>
        public bool OptanteSimplesNacional { get; set; }

        /// <summary>
        /// Indica se a pessoa jurídica é MEI (pacote 6). Falso quando o dado não foi consultado.
        /// </summary>
        public bool Mei { get; set; }

        /// <summary>
        /// Situação cadastral da pessoa jurídica (ex.: <c>Ativa</c>), quando disponível (pacote 6).
        /// </summary>
        public string SituacaoCadastral { get; set; }
    }
}
