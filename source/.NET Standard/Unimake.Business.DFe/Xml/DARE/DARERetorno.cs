#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Schema;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.DARE
{

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.DARERetorno")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Retorno DARE Unico e Lote
    /// </summary>
    [Serializable()]

    [XmlRoot("DareRetorno", Namespace = "", IsNullable = false)]
    public class DARERetorno : XMLBase
    {
        #region DARE RETORNO NEGATIVO ÚNICO

        [XmlElement("receita")]
        public ReceitaDARERetorno Receita { get; set; }

        [XmlElement("cpf")]
        public string CPF { get; set; }

        [XmlElement("cidade")]
        public string Cidade { get; set; }

        [XmlElement("codigoBarra44")]
        public string CodigoBarra44 { get; set; }

        [XmlElement("codigoBarra48")]
        public string CodigoBarra48 { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DataVencimento { get; set; }
#else
        public DateTime DataVencimento { get; set; }
#endif

        [XmlElement("dataVencimento")]
        public string DataVencimentoField
        {
            get => DataVencimento.ToString("yyyy-MM-ddTHH:mm:ss");
#if INTEROP
            set => DataVencimento = DateTime.Parse(value);
#else
            set => DataVencimento = DateTime.Parse(value);
#endif
        }

        [XmlElement("endereco")]
        public string Endereco { get; set; }

        [XmlElement("erro")]
        public ErroRetorno Erro { get; set; }

        [XmlElement("gerarPDF")]
        public string GerarPDF { get; set; }

        [XmlElement("pixCopiaCola")]
        public string PixCopiaCola{ get; set; }

        [XmlElement("razaoSocial")]
        public string RazaoSocial { get; set; }

        [XmlElement("referencia")]
        public string Referencia { get; set; }

        /// <summary>
        ///  Número de telefone para contato relacionado ao DARE. 
        /// Este é o número pelo qual a entidade pode ser contatada para questões relacionadas ao documento.
        /// </summary>
        [XmlElement("telefone")]
        public string Telefone { get; set; }

        /// <summary>
        /// Unidade Federativa (UF) onde a entidade está localizada. 
        /// Este campo indica o estado brasileiro em que a entidade está registrada.
        /// </summary>
        [XmlElement("uf")]
        public string Uf { get; set; }

        /// <summary>
        /// Valor principal do DARE. 
        /// Este é o valor base a ser pago, antes da adição de juros e multas.
        /// </summary>
        [XmlElement("valor")]
        public string Valor { get; set; }

        [XmlElement("valorTotal")]
        public string ValorTotal { get; set; }

        #endregion DARE RETORNO NEGATIVO ÚNICO

        #region DARE RETORNO NEGATIVO LOTE

        [XmlElement("errors")]
        public Errors Errors { get;set; }

        [XmlElement("type")]
        public string Type { get; set; }

        [XmlElement("title")]
        public string Title { get; set; }

        [XmlElement("status")]
        public string Status { get; set; }

        [XmlElement("traceId")]
        public string TraceId { get; set; }

        #endregion DARE RETORNO NEGATIVO LOTE

        #region DARE RETORNO POSITIVO LOTE

        [XmlElement("itensParaGeracao")]
        public ItensParaGeracaoRetorno ItensParaGeracao { get; set; }

        /// <summary>
        /// Tipo de agrupamento dos "filhotes" no lote DARE. 
        /// Este campo define como os subelementos ou documentos são agrupados no lote.
        /// </summary>
        [XmlElement("tipoAgrupamentoFilhotes")]
        public string TipoAgrupamentoFilhotes { get; set; }

        [XmlElement("zipDownload")]
        public string ZipDownload{ get; set; }

        #endregion DARE RETORNO POSITIVO LOTE

        #region ShouldSerialize
        public bool ShouldSerializeDataVencimentoStringField() => !string.IsNullOrEmpty(DataVencimentoField);
        public bool ShouldSerializeDataVencimentoField() => DataVencimento > DateTime.MinValue;
        public bool ShouldSerializeGerarPDFField() => GerarPDF != null;
        #endregion ShouldSerialize
    }

    #region DARE RETORNO ÚNICO

    #region Receita DARE Retorno
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.ReceitaDARERetorno")]
    [ComVisible(true)]
#endif
    public class ReceitaDARERetorno
    {
        [XmlElement("codigo")]
        public string codigo { get; set; }

        [XmlElement("codigoServicoDARE")]
        public string codigoServicoDARE { get; set; }

        [XmlElement("nome")]
        public string nome { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeNomeField() => !string.IsNullOrWhiteSpace(nome);
        #endregion ShouldSerialize
    }
    #endregion Receita DARE Retorno

    #region ErroRetorno
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.ErroRetorno")]
    [ComVisible(true)]
#endif
    public class ErroRetorno
    {
        [XmlElement("mensagens")]
        public Mensagens Mensagens { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.Mensagens")]
    [ComVisible(true)]
#endif
    public class Mensagens
    {
        [XmlElement("mensagem")]
        public List<string> Mensagem { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddMensagem(string item)
        {
            if (Mensagem == null)
            {
                Mensagem = new List<string>();
            }

            Mensagem.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Mensagem (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Mensagem</returns>
        public string GetMensagem(int index)
        {
            if ((Mensagem?.Count ?? 0) == 0)
            {
                return default;
            };

            return Mensagem[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Mensagem
        /// </summary>
        public int GetMensagemCount => (Mensagem != null ? Mensagem.Count : 0);
#endif
    }

    #endregion ErroRetorno

    #endregion DARE RETORNO ÚNICO

    #region DARE RETORNO NEGATIVO LOTE
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.Erros")]
    [ComVisible(true)]
#endif
    public class Errors
    {
        /*
         * Verificar como irá ficar esta parte, já que será variável o nome das propriedades
         */
    }
    #endregion DARE RETORNO NEGATIVO LOTE

    #region DARE RETORNO POSITIVO LOTE
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.ItensParaGeracaoRetorno")]
    [ComVisible(true)]
#endif
    public class ItensParaGeracaoRetorno
    {
        /// <summary>
        /// Lista de itens para geração no lote DARE. 
        /// Contém os diferentes elementos que compõem o lote para processamento.
        /// </summary>

        [XmlElement("item")]
        public List<ItensDARERetorno> Item{ get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddItem(ItensDARERetorno novoItem)
        {
            if (Item == null)
            {
                Item = new List<ItensDARERetorno>();
            }

            Item.Add(novoItem);
        }

        /// <summary>
        /// Retorna o elemento da lista Item (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Item</returns>
        public ItensDARERetorno GetItem(int index)
        {
            if ((Item?.Count ?? 0) == 0)
            {
                return default;
            };

            return Item[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Item
        /// </summary>
        public int GetItemCount => (Item != null ? Item.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.ItensDARERetorno")]
    [ComVisible(true)]
#endif
    public class ItensDARERetorno
    {
        [XmlElement("receita")]
        public ReceitaDARERetorno Receita { get; set; }

        [XmlElement("cnpj")]
        public string CNPJ { get; set; }

        [XmlElement("cpf")]
        public string CPF { get; set; }

        [XmlElement("cidade")]
        public string Cidade { get; set; }

        [XmlElement("codigoBarra44")]
        public string CodigoBarra44 { get; set; }

        [XmlElement("codigoBarra48")]
        public string CodigoBarra48 { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DataVencimento { get; set; }
#else
        public DateTimeOffset DataVencimento { get; set; }
#endif

        [XmlElement("DataVencimento")]
        public string DataVencimentoField
        {
            get => DataVencimento.ToString("yyyy-MM-dd");
#if INTEROP
            set => DataVencimento = DateTime.Parse(value);
#else
            set => DataVencimento = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("endereco")]
        public string Endereco { get; set; }

        [XmlElement("gerarPDF")]
        public Boolean GerarPDF { get; set; }

        [XmlElement("referencia")]
        public string Referencia { get; set; }

        /// <summary>
        ///  Número de telefone para contato relacionado ao DARE. 
        /// Este é o número pelo qual a entidade pode ser contatada para questões relacionadas ao documento.
        /// </summary>
        [XmlElement("telefone")]
        public string Telefone { get; set; }

        /// <summary>
        /// Valor principal do DARE. 
        /// Este é o valor base a ser pago, antes da adição de juros e multas.
        /// </summary>
        [XmlElement("valor")]
        public string Valor { get; set; }

        [XmlElement("valorTotal")]
        public string ValorTotal { get; set; }

    }
    #endregion DARE RETORNO POSITIVO LOTE
}