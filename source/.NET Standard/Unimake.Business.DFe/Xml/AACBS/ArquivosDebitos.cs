#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Débitos agrupados por período de apuração.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.ApuracaoDebitos")]
    [ComVisible(true)]
#endif
    [XmlType(TypeName = "ApuracaoDebitosTipo", Namespace = "http://www.unimake.com.br/dfe/aacbs")]
    public class ApuracaoDebitos
    {
        /// <summary>
        /// Período de apuração no formato MM/AAAA.
        /// </summary>
        [XmlElement("pa", Order = 0)]
        public string Pa { get; set; }

        /// <summary>
        /// Documentos de débito do período.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlArray("debitos", Order = 1)]
        [XmlArrayItem("item", IsNullable = true)]
        public List<DebitoArquivo> Debitos { get; set; } = new List<DebitoArquivo>();

#if INTEROP
        /// <summary>
        /// Adiciona um item à coleção Debitos.
        /// </summary>
        /// <param name="item">Item a adicionar.</param>
        public void AddDebitos(DebitoArquivo item)
        {
            if (Debitos == null)
            {
                Debitos = new List<DebitoArquivo>();
            }

            Debitos.Add(item);
        }

        /// <summary>
        /// Retorna um item da coleção Debitos.
        /// </summary>
        /// <param name="index">Índice baseado em zero.</param>
        /// <returns>Item da coleção, ou nulo quando a coleção está vazia.</returns>
        public DebitoArquivo GetDebitos(int index) => GetDebitosCount() == 0 ? null : Debitos[index];

        /// <summary>
        /// Retorna a quantidade de itens da coleção Debitos.
        /// </summary>
        public int GetDebitosCount() => Debitos?.Count ?? 0;
#endif
    }

    /// <summary>
    /// Documento de débito da CBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.DebitoArquivo")]
    [ComVisible(true)]
#endif
    [XmlType(TypeName = "DebitoArquivoTipo", Namespace = "http://www.unimake.com.br/dfe/aacbs")]
    public class DebitoArquivo
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
        public CbsDebito Cbs { get; set; }
    }

    /// <summary>
    /// Valores da CBS do débito.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.CbsDebito")]
    [ComVisible(true)]
#endif
    [XmlType(TypeName = "CbsDebitoTipo", Namespace = "http://www.unimake.com.br/dfe/aacbs")]
    public class CbsDebito
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
        /// Valor inexigível.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal? Inexigivel { get; set; }

        /// <summary>
        /// Valor inexigível em formato decimal invariável.
        /// </summary>
        [XmlElement("inexigivel", Order = 2)]
        public string InexigivelField
        {
            get => Inexigivel.HasValue ? AACBSFormat.FormatDecimal(Inexigivel.Value) : null;
            set => Inexigivel = value == null ? (decimal?)null : AACBSFormat.ParseDecimal(value);
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
        [XmlElement("suspenso", Order = 3)]
        public string SuspensoField
        {
            get => Suspenso.HasValue ? AACBSFormat.FormatDecimal(Suspenso.Value) : null;
            set => Suspenso = value == null ? (decimal?)null : AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor de pagamento antecipado.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal? PgtoAntecipado { get; set; }

        /// <summary>
        /// Valor de pagamento antecipado em formato decimal invariável.
        /// </summary>
        [XmlElement("pgtoAntecipado", Order = 4)]
        public string PgtoAntecipadoField
        {
            get => PgtoAntecipado.HasValue ? AACBSFormat.FormatDecimal(PgtoAntecipado.Value) : null;
            set => PgtoAntecipado = value == null ? (decimal?)null : AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Valor extinto.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal? Extinto { get; set; }

        /// <summary>
        /// Valor extinto em formato decimal invariável.
        /// </summary>
        [XmlElement("extinto", Order = 5)]
        public string ExtintoField
        {
            get => Extinto.HasValue ? AACBSFormat.FormatDecimal(Extinto.Value) : null;
            set => Extinto = value == null ? (decimal?)null : AACBSFormat.ParseDecimal(value);
        }

        /// <summary>
        /// Saldo devedor da CBS.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public decimal SaldoDevedor { get; set; }

        /// <summary>
        /// Saldo devedor da CBS em formato decimal invariável.
        /// </summary>
        [XmlElement("saldoDevedor", Order = 6)]
        public string SaldoDevedorField
        {
            get => AACBSFormat.FormatDecimal(SaldoDevedor);
            set => SaldoDevedor = AACBSFormat.ParseDecimal(value);
        }
    }
}
