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
    /// DARE - SP - Documento de Arrecadação de Receitas Estaduais
    /// Envio individual
    /// </summary>
    [Serializable()]
    [XmlRoot("DareLote", Namespace = "https://portal.fazenda.sp.gov.br/servicos/dare", IsNullable = false)]
    public class DARELote : XMLBase
    {
        /// <summary>
        /// Dados do contribuinte não cadastrado no sistema. 
        /// Contém informações relevantes sobre o contribuinte que não está registrado no cadastro oficial.
        /// </summary>
        [XmlElement("dadosContribuinteNaoCadastrado")]
        public DadosContribuinteNaoCadastrado DadosContribuinteNaoCadastrado { get; set; }

        /// <summary>
        /// Informações sobre erros relacionados ao processamento do lote DARE. 
        /// Fornece detalhes sobre problemas ou falhas encontradas.
        /// </summary>
        [XmlElement("erro")]
        public Erro Erro { get; set; }

        /// <summary>
        /// Tipo de agrupamento dos "filhotes" no lote DARE. 
        /// Este campo define como os subelementos ou documentos são agrupados no lote.
        /// </summary>
        [XmlElement("tipoAgrupamentoFilhotes")]
        public string TipoAgrupamentoFilhotes { get; set; }

        /// <summary>
        /// Lista de itens para geração no lote DARE. 
        /// Contém os diferentes elementos que compõem o lote para processamento.
        /// </summary>
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

    public class DadosContribuinteNaoCadastrado : DARE { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.Erro")]
    [ComVisible(true)]
#endif
    public class Erro
    {
        /// <summary>
        /// Indica se o processamento do DARE está OK ou não. 
        /// Um valor que indica o status geral do processamento.
        /// </summary>
        [XmlElement("estaOk")]
        public string EstaOk { get; set; }

        /// <summary>
        /// Mensagens de erro associadas ao DARE. 
        /// Pode incluir uma ou mais mensagens explicando os problemas encontrados.
        /// </summary>

        [XmlElement("mensagens")]
        public string Mensagens { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.ItensParaGeracao")]
    [ComVisible(true)]
#endif
    public class ItensParaGeracao
    {
        /// <summary>
        /// Lista de objetos DARE para geração. 
        /// Cada item nesta lista representa um DARE individual que compõe o lote.
        /// </summary>
        [XmlElement("Dare")]
        public List<DARE> DARE { get; set; }
    }
}