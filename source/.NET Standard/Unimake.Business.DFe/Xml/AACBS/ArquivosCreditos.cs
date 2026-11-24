#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Créditos agrupados por período de apuração.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.ApuracaoCreditos")]
    [ComVisible(true)]
#endif
    [XmlType(TypeName = "ApuracaoCreditosTipo", Namespace = "http://www.unimake.com.br/dfe/aacbs")]
    public class ApuracaoCreditos
    {
        /// <summary>
        /// Período de apuração no formato MM/AAAA.
        /// </summary>
        [XmlElement("pa", Order = 0)]
        public string Pa { get; set; }

        /// <summary>
        /// Documentos de crédito do período.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlArray("creditos", Order = 1)]
        [XmlArrayItem("item", IsNullable = true)]
        public List<CreditoArquivo> Creditos { get; set; } = new List<CreditoArquivo>();

#if INTEROP
        /// <summary>
        /// Adiciona um item à coleção Creditos.
        /// </summary>
        /// <param name="item">Item a adicionar.</param>
        public void AddCreditos(CreditoArquivo item)
        {
            if (Creditos == null)
            {
                Creditos = new List<CreditoArquivo>();
            }

            Creditos.Add(item);
        }

        /// <summary>
        /// Retorna um item da coleção Creditos.
        /// </summary>
        /// <param name="index">Índice baseado em zero.</param>
        /// <returns>Item da coleção, ou nulo quando a coleção está vazia.</returns>
        public CreditoArquivo GetCreditos(int index) => GetCreditosCount() == 0 ? null : Creditos[index];

        /// <summary>
        /// Retorna a quantidade de itens da coleção Creditos.
        /// </summary>
        public int GetCreditosCount() => Creditos?.Count ?? 0;
#endif
    }

    /// <summary>
    /// Documento de crédito da CBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.CreditoArquivo")]
    [ComVisible(true)]
#endif
    [XmlType(TypeName = "CreditoArquivoTipo", Namespace = "http://www.unimake.com.br/dfe/aacbs")]
    public class CreditoArquivo
    {
        /// <summary>
        /// Código da origem do registro.
        /// </summary>
        [XmlElement("origem", Order = 0)]
        public int Origem { get; set; }

        /// <summary>
        /// Código do tipo de documento.
        /// </summary>
        [XmlElement("documento", Order = 1)]
        public int Documento { get; set; }

        /// <summary>
        /// Chave de identificação do documento.
        /// </summary>
        [XmlElement("chave", Order = 2)]
        public string Chave { get; set; }

        /// <summary>
        /// Instante de emissão do documento.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public DateTimeOffset Emissao
        {
            get => AACBSFormat.ParseInstant(EmissaoField);
            set => EmissaoField = AACBSFormat.FormatInstant(value);
        }

        /// <summary>
        /// Instante de emissão do documento na representação textual original.
        /// </summary>
        [XmlElement("emissao", Order = 3)]
        public string EmissaoField { get; set; }

        /// <summary>
        /// Instante de registro do documento.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public DateTimeOffset Registro
        {
            get => AACBSFormat.ParseInstant(RegistroField);
            set => RegistroField = AACBSFormat.FormatInstant(value);
        }

        /// <summary>
        /// Instante de registro do documento na representação textual original.
        /// </summary>
        [XmlElement("registro", Order = 4)]
        public string RegistroField { get; set; }

        /// <summary>
        /// Instante da última atualização do documento.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public DateTimeOffset Atualizacao
        {
            get => AACBSFormat.ParseInstant(AtualizacaoField);
            set => AtualizacaoField = AACBSFormat.FormatInstant(value);
        }

        /// <summary>
        /// Instante da última atualização do documento na representação textual original.
        /// </summary>
        [XmlElement("atualizacao", Order = 5)]
        public string AtualizacaoField { get; set; }

        /// <summary>
        /// Valores da CBS vinculados ao documento.
        /// </summary>
        [XmlElement("cbs", Order = 6)]
        public CbsCredito Cbs { get; set; }
    }

    /// <summary>
    /// Valores da CBS do crédito.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.CbsCredito")]
    [ComVisible(true)]
#endif
    [XmlType(TypeName = "CbsCreditoTipo", Namespace = "http://www.unimake.com.br/dfe/aacbs")]
    public class CbsCredito
    {
        /// <summary>
        /// Valor excedente da CBS.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal? Excedente { get; set; }

        /// <summary>
        /// Valor excedente da CBS em formato decimal invariável.
        /// </summary>
        [XmlElement("excedente", Order = 0)]
        public string ExcedenteField
        {
            get => Excedente.HasValue ? AACBSFormat.FormatDecimal(Excedente.Value) : null;
            set => Excedente = value == null ? (decimal?)null : AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor apurado da CBS.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal Apurado { get; set; }

        /// <summary>
        /// Valor apurado da CBS em formato decimal invariável.
        /// </summary>
        [XmlElement("apurado", Order = 1)]
        public string ApuradoField
        {
            get => AACBSFormat.FormatDecimal(Apurado);
            set => Apurado = AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Detalhamento da apropriação do crédito.
        /// </summary>
        [XmlElement("apropriacao", Order = 2)]
        public ApropriacaoCredito Apropriacao { get; set; }
    }

    /// <summary>
    /// Apropriação do crédito da CBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.ApropriacaoCredito")]
    [ComVisible(true)]
#endif
    [XmlType(TypeName = "ApropriacaoCreditoTipo", Namespace = "http://www.unimake.com.br/dfe/aacbs")]
    public class ApropriacaoCredito
    {
        /// <summary>
        /// Valor inapropriável.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal? Inapropriavel { get; set; }

        /// <summary>
        /// Valor inapropriável em formato decimal invariável.
        /// </summary>
        [XmlElement("inapropriavel", Order = 0)]
        public string InapropriavelField
        {
            get => Inapropriavel.HasValue ? AACBSFormat.FormatDecimal(Inapropriavel.Value) : null;
            set => Inapropriavel = value == null ? (decimal?)null : AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor suspenso.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal? Suspenso { get; set; }

        /// <summary>
        /// Valor suspenso em formato decimal invariável.
        /// </summary>
        [XmlElement("suspenso", Order = 1)]
        public string SuspensoField
        {
            get => Suspenso.HasValue ? AACBSFormat.FormatDecimal(Suspenso.Value) : null;
            set => Suspenso = value == null ? (decimal?)null : AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor prescrito.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal? Prescrito { get; set; }

        /// <summary>
        /// Valor prescrito em formato decimal invariável.
        /// </summary>
        [XmlElement("prescrito", Order = 2)]
        public string PrescritoField
        {
            get => Prescrito.HasValue ? AACBSFormat.FormatDecimal(Prescrito.Value) : null;
            set => Prescrito = value == null ? (decimal?)null : AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor a apropriar.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal? AApropriar { get; set; }

        /// <summary>
        /// Valor a apropriar em formato decimal invariável.
        /// </summary>
        [XmlElement("aApropriar", Order = 3)]
        public string AApropriarField
        {
            get => AApropriar.HasValue ? AACBSFormat.FormatDecimal(AApropriar.Value) : null;
            set => AApropriar = value == null ? (decimal?)null : AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor apropriado.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal? Apropriado { get; set; }

        /// <summary>
        /// Valor apropriado em formato decimal invariável.
        /// </summary>
        [XmlElement("apropriado", Order = 4)]
        public string ApropriadoField
        {
            get => Apropriado.HasValue ? AACBSFormat.FormatDecimal(Apropriado.Value) : null;
            set => Apropriado = value == null ? (decimal?)null : AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Detalhamento da utilização do crédito.
        /// </summary>
        [XmlElement("utilizacao", Order = 5)]
        public UtilizacaoCredito Utilizacao { get; set; }
    }

    /// <summary>
    /// Utilização do crédito da CBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.UtilizacaoCredito")]
    [ComVisible(true)]
#endif
    [XmlType(TypeName = "UtilizacaoCreditoTipo", Namespace = "http://www.unimake.com.br/dfe/aacbs")]
    public class UtilizacaoCredito
    {
        /// <summary>
        /// Valor inutilizável.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal? Inutilizavel { get; set; }

        /// <summary>
        /// Valor inutilizável em formato decimal invariável.
        /// </summary>
        [XmlElement("inutilizavel", Order = 0)]
        public string InutilizavelField
        {
            get => Inutilizavel.HasValue ? AACBSFormat.FormatDecimal(Inutilizavel.Value) : null;
            set => Inutilizavel = value == null ? (decimal?)null : AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor utilizado.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal? Utilizado { get; set; }

        /// <summary>
        /// Valor utilizado em formato decimal invariável.
        /// </summary>
        [XmlElement("utilizado", Order = 1)]
        public string UtilizadoField
        {
            get => Utilizado.HasValue ? AACBSFormat.FormatDecimal(Utilizado.Value) : null;
            set => Utilizado = value == null ? (decimal?)null : AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor restabelecido.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal? Restabelecido { get; set; }

        /// <summary>
        /// Valor restabelecido em formato decimal invariável.
        /// </summary>
        [XmlElement("restabelecido", Order = 2)]
        public string RestabelecidoField
        {
            get => Restabelecido.HasValue ? AACBSFormat.FormatDecimal(Restabelecido.Value) : null;
            set => Restabelecido = value == null ? (decimal?)null : AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Detalhamento do crédito não utilizado.
        /// </summary>
        [XmlElement("naoUtilizado", Order = 3)]
        public NaoUtilizadoCredito NaoUtilizado { get; set; }
    }

    /// <summary>
    /// Crédito da CBS não utilizado.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.NaoUtilizadoCredito")]
    [ComVisible(true)]
#endif
    [XmlType(TypeName = "NaoUtilizadoCreditoTipo", Namespace = "http://www.unimake.com.br/dfe/aacbs")]
    public class NaoUtilizadoCredito
    {
        /// <summary>
        /// Saldo credor da CBS.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal SaldoCredor { get; set; }

        /// <summary>
        /// Saldo credor da CBS em formato decimal invariável.
        /// </summary>
        [XmlElement("saldoCredor", Order = 0)]
        public string SaldoCredorField
        {
            get => AACBSFormat.FormatDecimal(SaldoCredor);
            set => SaldoCredor = AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor do pedido de ressarcimento.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal? PedidoRessarcimento { get; set; }

        /// <summary>
        /// Valor do pedido de ressarcimento em formato decimal invariável.
        /// </summary>
        [XmlElement("pedidoRessarcimento", Order = 1)]
        public string PedidoRessarcimentoField
        {
            get => PedidoRessarcimento.HasValue ? AACBSFormat.FormatDecimal(PedidoRessarcimento.Value) : null;
            set => PedidoRessarcimento = value == null ? (decimal?)null : AACBSFormat.ParseDecimal(value);
        }
    }
}
