#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Lookup
{
    /// <summary>
    /// Resolvedor de destinatário para NFC-e/NFe (modelos 65 e 55). A partir de um CPF/CNPJ,
    /// consulta um <see cref="IPessoaLookup"/> (qualquer provedor; a implementação de referência é
    /// <see cref="CpfCnpjComBrLookup"/>) e devolve um <see cref="Dest"/> com <see cref="EnderDest"/>
    /// preenchido (nome, endereço e código IBGE de município em <c>cMun</c>).
    /// O núcleo da biblioteca não depende desta classe: é um utilitário opcional.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Campos obrigatórios do schema nunca são inventados nem preenchidos com valor neutro:
    /// quando o provedor não devolve logradouro, bairro, município, código IBGE do município ou
    /// UF válida, é lançada <see cref="PessoaLookupException"/>. A única convenção aplicada é o
    /// número do logradouro, que vira <c>S/N</c> quando ausente. Assim nenhum documento sai com
    /// <c>cMun</c> zerado ou UF não definida por omissão do provedor.
    /// </para>
    /// <para>
    /// A API não fornece Inscrição Estadual nos pacotes de endereço nem Inscrição Municipal.
    /// A IE é opcional e vem do pacote 16: a seleção é feita pela UF do endereço, preferindo
    /// inscrições ativas. Só há preenchimento de IE e <c>indIEDest = 1</c> quando existe uma
    /// inscrição ATIVA na UF do endereço; caso contrário <c>indIEDest = 9</c> (não contribuinte).
    /// Isenção (<c>indIEDest = 2</c>) nunca é presumida.
    /// </para>
    /// <para>
    /// A regra da NF-e exige que a Inscrição Estadual do destinatário seja da MESMA UF do endereço
    /// informado em <c>enderDest</c>. Por isso, inscrições ativas em outras UFs são ignoradas de
    /// propósito: usar a IE de outro estado geraria rejeição na SEFAZ. Empresa com filiais em
    /// vários estados precisa, portanto, do endereço da filial que realmente é o destinatário.
    /// </para>
    /// <para>
    /// Este namespace é somente .NET (sem exposição COM/INTEROP), por usar API assíncrona baseada em Task.
    /// </para>
    /// </remarks>
#if INTEROP
    [ComVisible(false)]
#endif
    public class TomadorResolver
    {
        /// <summary>
        /// Menor código IBGE de município válido (7 dígitos).
        /// </summary>
        private const int CodigoMunicipioMinimo = 1000000;

        /// <summary>
        /// Maior código IBGE de município válido (7 dígitos).
        /// </summary>
        private const int CodigoMunicipioMaximo = 9999999;

        /// <summary>
        /// Menor código IBGE de unidade federativa (Rondônia).
        /// </summary>
        private const int CodigoUfMinimo = 11;

        /// <summary>
        /// Maior código IBGE de unidade federativa (Distrito Federal).
        /// </summary>
        private const int CodigoUfMaximo = 53;

        private readonly IPessoaLookup _lookup;

        /// <summary>
        /// Cria o resolvedor.
        /// </summary>
        /// <param name="lookup">Implementação de consulta de pessoa. Obrigatória.</param>
        /// <exception cref="ArgumentNullException">Quando <paramref name="lookup"/> é nulo.</exception>
        public TomadorResolver(IPessoaLookup lookup)
        {
            _lookup = lookup ?? throw new ArgumentNullException(nameof(lookup));
        }

        /// <summary>
        /// Resolve um destinatário pessoa física (consumidor final) a partir do CPF.
        /// O indicador de IE fica sempre como não contribuinte (<c>indIEDest = 9</c>).
        /// </summary>
        /// <param name="cpf">CPF do destinatário (com ou sem máscara).</param>
        /// <param name="comEndereco">Quando <c>true</c>, também preenche o endereço (pacote 3).</param>
        /// <param name="cancellationToken">Token de cancelamento.</param>
        /// <returns>Destinatário preenchido.</returns>
        /// <exception cref="PessoaLookupException">
        /// Quando o provedor devolve documento fora do formato ou endereço sem os campos
        /// obrigatórios do schema (logradouro, bairro, município, código IBGE ou UF).
        /// </exception>
        public async Task<Dest> ResolverPorCpfAsync(string cpf, bool comEndereco, CancellationToken cancellationToken = default(CancellationToken))
        {
            var resultado = await _lookup.ConsultarCpfAsync(cpf, comEndereco, cancellationToken).ConfigureAwait(false);

            var dest = new Dest { IndIEDest = IndicadorIEDestinatario.NaoContribuinte };
            PreencherDestinatario(dest, resultado);
            return dest;
        }

        /// <summary>
        /// Resolve um destinatário pessoa jurídica a partir do CNPJ.
        /// </summary>
        /// <param name="cnpj">CNPJ do destinatário (com ou sem máscara).</param>
        /// <param name="resolverInscricaoEstadual">
        /// Quando <c>true</c>, consulta o pacote 16 e tenta preencher a IE pela UF do endereço.
        /// </param>
        /// <param name="cancellationToken">Token de cancelamento.</param>
        /// <returns>Destinatário preenchido.</returns>
        /// <exception cref="PessoaLookupException">
        /// Quando o provedor devolve documento fora do formato ou endereço sem os campos
        /// obrigatórios do schema (logradouro, bairro, município, código IBGE ou UF).
        /// </exception>
        public async Task<Dest> ResolverPorCnpjAsync(string cnpj, bool resolverInscricaoEstadual, CancellationToken cancellationToken = default(CancellationToken))
        {
            var resultado = await _lookup.ConsultarCnpjAsync(cnpj, false, cancellationToken).ConfigureAwait(false);

            var dest = new Dest { IndIEDest = IndicadorIEDestinatario.NaoContribuinte };
            PreencherDestinatario(dest, resultado);

            if(resolverInscricaoEstadual)
            {
                var inscricoes = await _lookup.ConsultarInscricoesEstaduaisAsync(cnpj, cancellationToken).ConfigureAwait(false);
                AplicarInscricaoEstadual(dest, inscricoes);
            }

            return dest;
        }

        /// <summary>
        /// Preenche um <see cref="Dest"/> existente a partir de um <see cref="ResultadoPessoa"/>,
        /// sem tocar no indicador de IE. Útil para reaproveitar um destinatário já parcialmente montado.
        /// Pessoa física preenche <c>CPF</c> e limpa <c>CNPJ</c>; pessoa jurídica faz o inverso,
        /// de modo que as duas tags nunca saem juntas no XML.
        /// </summary>
        /// <param name="dest">Destinatário a preencher. Obrigatório.</param>
        /// <param name="resultado">Dados neutros da pessoa. Obrigatório.</param>
        /// <exception cref="ArgumentNullException">Quando <paramref name="dest"/> ou <paramref name="resultado"/> é nulo.</exception>
        /// <exception cref="PessoaLookupException">
        /// Quando o documento está ausente ou fora do formato (CPF com 11 dígitos; CNPJ com 14
        /// caracteres alfanuméricos terminados em 2 dígitos), ou quando o endereço devolvido pelo
        /// provedor não traz logradouro, bairro, município, código IBGE do município ou UF válida.
        /// </exception>
        public void PreencherDestinatario(Dest dest, ResultadoPessoa resultado)
        {
            if(dest == null)
            {
                throw new ArgumentNullException(nameof(dest));
            }

            if(resultado == null)
            {
                throw new ArgumentNullException(nameof(resultado));
            }

            var documento = NormalizarDocumento(resultado.Documento);

            if(resultado.PessoaFisica)
            {
                if(documento.Length != 11 || !documento.All(char.IsDigit))
                {
                    throw new PessoaLookupException("CPF do destinatário ausente ou fora do formato (esperado 11 dígitos): " + Exibir(resultado.Documento) + ".");
                }

                dest.CPF = documento;
                dest.CNPJ = null;
            }
            else
            {
                if(documento.Length != 14 || !char.IsDigit(documento[12]) || !char.IsDigit(documento[13]))
                {
                    throw new PessoaLookupException("CNPJ do destinatário ausente ou fora do formato (esperado 14 caracteres alfanuméricos com os 2 últimos numéricos): " + Exibir(resultado.Documento) + ".");
                }

                dest.CNPJ = documento;
                dest.CPF = null;
            }

            if(!string.IsNullOrWhiteSpace(resultado.Nome))
            {
                dest.XNome = resultado.Nome;
            }

            if(resultado.Endereco != null)
            {
                dest.EnderDest = MapearEndereco(resultado.Endereco);
            }
        }

        /// <summary>
        /// Seleciona a inscrição estadual ativa correspondente à UF informada.
        /// Inscrições ativas em outras UFs são ignoradas de propósito: a NF-e exige que a IE do
        /// destinatário seja da mesma UF do endereço declarado em <c>enderDest</c>.
        /// </summary>
        /// <param name="inscricoes">Inscrições estaduais (ex.: pacote 16).</param>
        /// <param name="ufSigla">Sigla da UF do endereço (ex.: <c>MG</c>).</param>
        /// <returns>A primeira inscrição ATIVA na UF, ou <c>null</c> quando não houver.</returns>
        public InscricaoEstadualInfo SelecionarInscricaoEstadual(IEnumerable<InscricaoEstadualInfo> inscricoes, string ufSigla)
        {
            if(inscricoes == null || string.IsNullOrWhiteSpace(ufSigla))
            {
                return null;
            }

            return inscricoes.FirstOrDefault(i =>
                i != null &&
                i.Ativo &&
                !string.IsNullOrWhiteSpace(i.Uf) &&
                string.Equals(i.Uf.Trim(), ufSigla.Trim(), StringComparison.OrdinalIgnoreCase));
        }

        /// <summary>
        /// Aplica a IE e o indicador de IE ao destinatário, usando a UF já validada em
        /// <see cref="Dest.EnderDest"/>. Sem endereço não há UF de referência e o destinatário
        /// fica como não contribuinte.
        /// </summary>
        /// <param name="dest">Destinatário a ajustar.</param>
        /// <param name="inscricoes">Inscrições estaduais devolvidas pelo provedor.</param>
        private void AplicarInscricaoEstadual(Dest dest, IEnumerable<InscricaoEstadualInfo> inscricoes)
        {
            var ufSigla = dest.EnderDest == null ? null : dest.EnderDest.UF.ToString();
            var inscricao = SelecionarInscricaoEstadual(inscricoes, ufSigla);

            if(inscricao != null && !string.IsNullOrWhiteSpace(inscricao.InscricaoEstadual))
            {
                dest.IE = inscricao.InscricaoEstadual;
                dest.IndIEDest = IndicadorIEDestinatario.ContribuinteICMS;
            }
            else
            {
                dest.IE = null;
                dest.IndIEDest = IndicadorIEDestinatario.NaoContribuinte;
            }
        }

        /// <summary>
        /// Converte o endereço neutro do provedor em <see cref="EnderDest"/>, validando os campos
        /// que o schema da NF-e exige.
        /// </summary>
        /// <param name="endereco">Endereço devolvido pelo provedor.</param>
        /// <returns>Endereço do destinatário pronto para o XML.</returns>
        /// <exception cref="PessoaLookupException">
        /// Quando logradouro, bairro, município, código IBGE do município ou UF estão ausentes ou inválidos.
        /// </exception>
        private static EnderDest MapearEndereco(EnderecoPessoa endereco)
        {
            if(string.IsNullOrWhiteSpace(endereco.Logradouro))
            {
                throw new PessoaLookupException("Logradouro não informado pelo provedor; não é possível preencher xLgr.");
            }

            if(string.IsNullOrWhiteSpace(endereco.Bairro))
            {
                throw new PessoaLookupException("Bairro não informado pelo provedor; não é possível preencher xBairro.");
            }

            if(string.IsNullOrWhiteSpace(endereco.Municipio))
            {
                throw new PessoaLookupException("Município não informado pelo provedor; não é possível preencher xMun.");
            }

            if(endereco.CodigoMunicipioIbge < CodigoMunicipioMinimo || endereco.CodigoMunicipioIbge > CodigoMunicipioMaximo)
            {
                throw new PessoaLookupException("Código IBGE do município não informado pelo provedor; não é possível preencher cMun.");
            }

            return new EnderDest
            {
                XLgr = endereco.Logradouro,
                Nro = string.IsNullOrWhiteSpace(endereco.Numero) ? "S/N" : endereco.Numero,
                XCpl = endereco.Complemento,
                XBairro = endereco.Bairro,
                CMun = endereco.CodigoMunicipioIbge,
                XMun = endereco.Municipio,
                UF = ConverterUf(endereco.Uf),
                CEP = endereco.Cep,
                CPais = 1058,
                XPais = string.IsNullOrWhiteSpace(endereco.Pais) ? "Brasil" : endereco.Pais
            };
        }

        /// <summary>
        /// Converte a sigla de UF devolvida pelo provedor no enumerador da biblioteca, aceitando
        /// apenas siglas de estado (duas letras, com código IBGE de 11 a 53). Códigos de ambiente
        /// como <c>SVRS</c>, <c>AN</c> ou <c>EX</c>, números e valores vazios são recusados, para
        /// que a tag <c>UF</c> nunca saia como <c>NaoDefinido</c>.
        /// </summary>
        /// <param name="ufSigla">Sigla informada pelo provedor.</param>
        /// <returns>UF correspondente.</returns>
        /// <exception cref="PessoaLookupException">Quando a sigla está ausente ou não é uma UF brasileira.</exception>
        private static UFBrasil ConverterUf(string ufSigla)
        {
            var sigla = string.IsNullOrWhiteSpace(ufSigla) ? string.Empty : ufSigla.Trim();

            if(sigla.Length == 2 &&
               char.IsLetter(sigla[0]) &&
               char.IsLetter(sigla[1]) &&
               Enum.TryParse(sigla, true, out UFBrasil uf) &&
               Enum.IsDefined(typeof(UFBrasil), uf) &&
               (int)uf >= CodigoUfMinimo &&
               (int)uf <= CodigoUfMaximo)
            {
                return uf;
            }

            throw new PessoaLookupException("UF do endereço ausente ou inválida: " + Exibir(ufSigla) + ".");
        }

        /// <summary>
        /// Remove máscara e espaços do documento, deixando só letras e dígitos em maiúsculas
        /// (o CNPJ alfanumérico usa letras nos 12 primeiros caracteres).
        /// </summary>
        /// <param name="documento">Documento como veio do provedor.</param>
        /// <returns>Documento normalizado, ou vazio quando não há conteúdo.</returns>
        private static string NormalizarDocumento(string documento)
        {
            if(string.IsNullOrWhiteSpace(documento))
            {
                return string.Empty;
            }

            return new string(documento.Where(char.IsLetterOrDigit).Select(char.ToUpperInvariant).ToArray());
        }

        /// <summary>
        /// Formata um valor para mensagem de erro, sem deixar a mensagem terminar em branco.
        /// </summary>
        /// <param name="valor">Valor recebido do provedor.</param>
        /// <returns>O valor entre aspas, ou a palavra <c>(vazio)</c>.</returns>
        private static string Exibir(string valor) => string.IsNullOrWhiteSpace(valor) ? "(vazio)" : "\"" + valor.Trim() + "\"";
    }
}
