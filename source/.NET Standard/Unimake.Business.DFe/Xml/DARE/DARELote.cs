#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.DARE
{

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.DARELote")]
    [ComVisible(true)]
#endif

    /// <summary>
    /// 
    /// </summary>

    [Serializable()]
    [XmlRoot("DareLote", Namespace = "https://portal.fazenda.sp.gov.br/servicos/dare", IsNullable = false)]
    public class DARELote : XMLBase
    {
        [XmlElement("dadosContribuinteNaoCadastrado")]


        public DadosContribuinteNaoCadastrado DadosContribuinteNaoCadastrado { get; set; }

        [XmlElement("erro")]
        public Erro Erro { get; set; }

        [XmlElement("tipoAgrupamentoFilhotes")]
        public string TipoAgrupamentoFilhotes { get; set; }

        [XmlElement("itensParaGeracao")]

        public List<ItensParaGeracao> ItensParaGeracao { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddItensParaGeracao(ItensParaGeracao item)
        {
            if (ItensParaGeracao == null)
            {
                ItensParaGeracao = new List<ItensParaGeracao>();
            }

            ItensParaGeracao.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ItensParaGeracao (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ItensParaGeracao</returns>
        public ItensParaGeracao GetItensParaGeracao(int index)
        {
            if ((ItensParaGeracao?.Count ?? 0) == 0)
            {
                return default;
            };

            return ItensParaGeracao[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ItensParaGeracao
        /// </summary>
        public int GetItensParaGeracaoCount => (ItensParaGeracao != null ? ItensParaGeracao.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.DadosContribuinteNaoCadastrado")]
    [ComVisible(true)]
#endif

    /// <summary>
    /// 
    /// </summary>

    public class DadosContribuinteNaoCadastrado
    {
        [XmlElement("numeroControleDarePrincipal")]
        public string NumeroControleDarePrincipal { get; set; }

        [XmlElement("gerarPDF")]
        public string GerarPDF { get; set; }

        [XmlElement("codigoBarra44")]
        public string CodigoBarra44 { get; set; }

        [XmlElement("codigoBarra48")]
        public string CodigoBarra48 { get; set; }

        [XmlElement("cnpj")]
        public string Cnpj { get; set; }

        [XmlElement("cpf")]
        public string Cpf { get; set; }

        [XmlElement("cpr")]
        public string Cpr { get; set; }

        [XmlElement("cidade")]
        public string Cidade { get; set; }

        [XmlElement("dataVencimento")]
        public string DataVencimento { get; set; }

        [XmlElement("documentoImpressao")]
        public string DocumentoImpressao { get; set; }

        [XmlElement("endereco")]
        public string Endereco { get; set; }

        [XmlElement("funcionalidadeOrigem")]
        public string FuncionalidadeOrigem { get; set; }

        [XmlElement("inscricaoEstadual")]
        public string InscricaoEstadual { get; set; }

        [XmlElement("pixCopiaCola")]
        public string PixCopiaCola { get; set; }

        [XmlElement("possiveisReceitas")]
        public List<string> PossiveisReceitas { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPossiveisReceitas(PossiveisReceitas item)
        {
            if (PossiveisReceitas == null)
            {
                PossiveisReceitas = new List<PossiveisReceitas>();
            }

            PossiveisReceitas.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PossiveisReceitas (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PossiveisReceitas</returns>
        public PossiveisReceitas GetPossiveisReceitas(int index)
        {
            if ((PossiveisReceitas?.Count ?? 0) == 0)
            {
                return default;
            };

            return PossiveisReceitas[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista PossiveisReceitas
        /// </summary>
        public int GetPossiveisReceitasCount => (PossiveisReceitas != null ? PossiveisReceitas.Count : 0);
#endif

        [XmlElement("razaoSocial")]
        public string RazaoSocial { get; set; }

        [XmlElement("receita")]
        public ReceitaDARE Receita { get; set; }

        [XmlElement("referencia")]
        public string Referencia { get; set; }

        [XmlElement("telefone")]
        public string Telefone { get; set; }

        [XmlElement("uf")]
        public string Uf { get; set; }

        [XmlElement("valor")]
        public string Valor { get; set; }

        [XmlElement("valorJuros")]
        public string ValorJuros { get; set; }

        [XmlElement("valorMulta")]
        public string ValorMulta { get; set; }

        [XmlElement("valorTotal")]
        public string ValorTotal { get; set; }

        [XmlElement("linha06")]
        public string Linha06 { get; set; }

        [XmlElement("linha08")]
        public string Linha08 { get; set; }

        [XmlElement("numeroGuia")]
        public string NumeroGuia { get; set; }

    }
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.Erro")]
    [ComVisible(true)]
#endif

    /// <summary>
    /// 
    /// </summary>

    public class Erro
    {
        [XmlElement("estaOk")]
        public string EstaOk { get; set; }

        [XmlElement("mensagens")]
        public string Mensagens { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.ItensParaGeracao")]
    [ComVisible(true)]
#endif

    /// <summary>
    /// 
    /// </summary>

    public class ItensParaGeracao
    {

        [XmlElement("Dare")]
        public List<DARE> DARE { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.DARE")]
    [ComVisible(true)]
#endif
    public class DARE
    {
        [XmlElement("numeroControleDarePrincipal")]
        public string NumeroControleDarePrincipal { get; set; }

        [XmlElement("gerarPDF")]
        public string GerarPDF { get; set; }

        [XmlElement("codigoBarra44")]
        public string CodigoBarra44 { get; set; }

        [XmlElement("codigoBarra48")]
        public string CodigoBarra48 { get; set; }

        [XmlElement("cnpj")]
        public string Cnpj { get; set; }

        [XmlElement("cpf")]
        public string Cpf { get; set; }

        [XmlElement("cpr")]
        public string Cpr { get; set; }

        [XmlElement("cidade")]
        public string Cidade { get; set; }

        [XmlElement("dataVencimento")]
        public string DataVencimento { get; set; }

        [XmlElement("documentoImpressao")]
        public string DocumentoImpressao { get; set; }

        [XmlElement("endereco")]
        public string Endereco { get; set; }

        [XmlElement("funcionalidadeOrigem")]
        public string FuncionalidadeOrigem { get; set; }

        [XmlElement("inscricaoEstadual")]
        public string InscricaoEstadual { get; set; }

        [XmlElement("pixCopiaCola")]
        public string PixCopiaCola { get; set; }

        [XmlElement("possiveisReceitas")]
        public string PossiveisReceitas { get; set; }

        [XmlElement("razaoSocial")]
        public string RazaoSocial { get; set; }

        [XmlElement("receita")]
        public ReceitaDARE Receita { get; set; }

        [XmlElement("referencia")]
        public string Referencia { get; set; }

        [XmlElement("telefone")]
        public string Telefone { get; set; }

        [XmlElement("uf")]
        public string Uf { get; set; }

        [XmlElement("valor")]
        public string Valor { get; set; }

        [XmlElement("valorJuros")]
        public string ValorJuros { get; set; }

        [XmlElement("valorMulta")]
        public string ValorMulta { get; set; }

        [XmlElement("valorTotal")]
        public string ValorTotal { get; set; }

        [XmlElement("linha06")]
        public string Linha06 { get; set; }

        [XmlElement("linha08")]
        public string Linha08 { get; set; }

        [XmlElement("numeroGuia")]
        public string NumeroGuia { get; set; }
    }
}