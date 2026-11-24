#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Pagamentos agrupados por data de arrecadação.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.ApuracaoPagamentos")]
    [ComVisible(true)]
#endif
    [XmlType(TypeName = "ApuracaoPagamentosTipo", Namespace = "http://www.unimake.com.br/dfe/aacbs")]
    public class ApuracaoPagamentos
    {
        /// <summary>
        /// Data civil da arrecadação.
        /// </summary>
        [XmlElement("dataArrecadacao", Order = 0)]
        public string DataArrecadacao { get; set; }

        /// <summary>
        /// Pagamentos da data de arrecadação.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlArray("pagamentos", Order = 1)]
        [XmlArrayItem("item", IsNullable = true)]
        public List<PagamentoArquivo> Pagamentos { get; set; } = new List<PagamentoArquivo>();

#if INTEROP
        /// <summary>
        /// Adiciona um item à coleção Pagamentos.
        /// </summary>
        /// <param name="item">Item a adicionar.</param>
        public void AddPagamentos(PagamentoArquivo item)
        {
            if (Pagamentos == null)
            {
                Pagamentos = new List<PagamentoArquivo>();
            }

            Pagamentos.Add(item);
        }

        /// <summary>
        /// Retorna um item da coleção Pagamentos.
        /// </summary>
        /// <param name="index">Índice baseado em zero.</param>
        /// <returns>Item da coleção, ou nulo quando a coleção está vazia.</returns>
        public PagamentoArquivo GetPagamentos(int index) => GetPagamentosCount() == 0 ? null : Pagamentos[index];

        /// <summary>
        /// Retorna a quantidade de itens da coleção Pagamentos.
        /// </summary>
        public int GetPagamentosCount() => Pagamentos?.Count ?? 0;
#endif
    }

    /// <summary>
    /// Pagamento da CBS por DARF RAD ou PCONT.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.PagamentoArquivo")]
    [ComVisible(true)]
#endif
    [XmlType(TypeName = "PagamentoArquivoTipo", Namespace = "http://www.unimake.com.br/dfe/aacbs")]
    public class PagamentoArquivo
    {
        /// <summary>
        /// Número do DARF com 17 dígitos.
        /// </summary>
        [XmlElement("numeroDARF", Order = 0)]
        public string NumeroDarf { get; set; }

        /// <summary>
        /// Código do tipo de pagamento, RAD (1) ou PCONT (2).
        /// </summary>
        [XmlElement("tipo", Order = 1)]
        public int Tipo { get; set; }

        /// <summary>
        /// CNPJ do adquirente no pagamento RAD.
        /// </summary>
        [XmlElement("niAdquirente", Order = 2)]
        public string NiAdquirente { get; set; }

        /// <summary>
        /// CNPJ do contribuinte.
        /// </summary>
        [XmlElement("niContribuinte", Order = 3)]
        public string NiContribuinte { get; set; }

        /// <summary>
        /// Itens da composição do pagamento.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlArray("composicao", Order = 4)]
        [XmlArrayItem("item", IsNullable = true)]
        public List<ComposicaoPagamentoArquivo> Composicao { get; set; } = new List<ComposicaoPagamentoArquivo>();

#if INTEROP
        /// <summary>
        /// Adiciona um item à coleção Composicao.
        /// </summary>
        /// <param name="item">Item a adicionar.</param>
        public void AddComposicao(ComposicaoPagamentoArquivo item)
        {
            if (Composicao == null)
            {
                Composicao = new List<ComposicaoPagamentoArquivo>();
            }

            Composicao.Add(item);
        }

        /// <summary>
        /// Retorna um item da coleção Composicao.
        /// </summary>
        /// <param name="index">Índice baseado em zero.</param>
        /// <returns>Item da coleção, ou nulo quando a coleção está vazia.</returns>
        public ComposicaoPagamentoArquivo GetComposicao(int index) => GetComposicaoCount() == 0 ? null : Composicao[index];

        /// <summary>
        /// Retorna a quantidade de itens da coleção Composicao.
        /// </summary>
        public int GetComposicaoCount() => Composicao?.Count ?? 0;
#endif
    }

    /// <summary>
    /// Item da composição do pagamento da CBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.ComposicaoPagamentoArquivo")]
    [ComVisible(true)]
#endif
    [XmlType(TypeName = "ComposicaoPagamentoArquivoTipo", Namespace = "http://www.unimake.com.br/dfe/aacbs")]
    public class ComposicaoPagamentoArquivo
    {
        /// <summary>
        /// Número sequencial do item na composição.
        /// </summary>
        [XmlElement("sequencial", Order = 0)]
        public int Sequencial { get; set; }

        /// <summary>
        /// Período de apuração no formato MM/AAAA.
        /// </summary>
        [XmlElement("pa", Order = 1)]
        public string Pa { get; set; }

        /// <summary>
        /// Vencimento como data civil ou, no RAD, instante com fuso.
        /// </summary>
        [XmlElement("vencimento", Order = 2)]
        public string Vencimento { get; set; }

        /// <summary>
        /// Instante de vencimento no RAD, quando informado com fuso.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public DateTimeOffset? VencimentoInstante =>
            Vencimento?.IndexOf('T') >= 0 ? AACBSFormat.ParseInstant(Vencimento) : (DateTimeOffset?)null;

        /// <summary>
        /// CNPJ do contribuinte no item de pagamento RAD.
        /// </summary>
        [XmlElement("niContribuinte", Order = 3)]
        public string NiContribuinte { get; set; }

        /// <summary>
        /// Chave do documento fiscal eletrônico.
        /// </summary>
        [XmlElement("chaveDFE", Order = 4)]
        public string ChaveDFe { get; set; }

        /// <summary>
        /// Valor do principal.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal Principal { get; set; }

        /// <summary>
        /// Valor do principal em formato decimal invariável.
        /// </summary>
        [XmlElement("principal", Order = 5)]
        public string PrincipalField
        {
            get => AACBSFormat.FormatDecimal(Principal);
            set => Principal = AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor da multa.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal Multa { get; set; }

        /// <summary>
        /// Valor da multa em formato decimal invariável.
        /// </summary>
        [XmlElement("multa", Order = 6)]
        public string MultaField
        {
            get => AACBSFormat.FormatDecimal(Multa);
            set => Multa = AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor dos juros.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal Juros { get; set; }

        /// <summary>
        /// Valor dos juros em formato decimal invariável.
        /// </summary>
        [XmlElement("juros", Order = 7)]
        public string JurosField
        {
            get => AACBSFormat.FormatDecimal(Juros);
            set => Juros = AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor total informado no arquivo.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal Total { get; set; }

        /// <summary>
        /// Valor total informado no arquivo em formato decimal invariável.
        /// </summary>
        [XmlElement("total", Order = 8)]
        public string TotalField
        {
            get => AACBSFormat.FormatDecimal(Total);
            set => Total = AACBSFormat.ParseDecimal(value);
        }
    }
}
