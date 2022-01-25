#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.GNRE
{
#if INTEROP
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("TLote_GNRE", Namespace = "http://www.gnre.pe.gov.br", IsNullable = false)]
    public class TLoteGNRE: XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; } = "2.00";

        [XmlElement("guias")]
        public Guias Guias { get; set; }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Guias
    {
        [XmlElement("TDadosGNRE")]
        public List<TDadosGNRE> TDadosGNRE { get; set; }

#if INTEROP
        #region Add - Interop

        public void AddTDadosGNRE(TDadosGNRE tdadosGNRE)
        {
            if(TDadosGNRE == null)
            {
                TDadosGNRE = new List<TDadosGNRE>();
            }

            TDadosGNRE.Add(tdadosGNRE);
        }

        #endregion
#endif
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class TDadosGNRE
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; } = "2.00";

        [XmlElement("ufFavorecida")]
        public UFBrasil UfFavorecida { get; set; }

        [XmlElement("tipoGnre")]
        public TipoGuiaGNRE TipoGNRE { get; set; }

        [XmlElement("contribuinteEmitente")]
        public ContribuinteEmitente ContribuinteEmitente { get; set; }

        [XmlElement("itensGNRE")]
        public ItensGNRE ItensGNRE { get; set; }

        [XmlIgnore]
        public double ValorGNRE { get; set; }

        [XmlElement("valorGNRE")]
        public string ValorGNREField
        {
            get => ValorGNRE.ToString("F2", CultureInfo.InvariantCulture);
            set => ValorGNRE = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public DateTime? DataPagamento { get; set; }

        [XmlElement("dataPagamento")]
        public string DataPagamentoField
        {
            get => DataPagamento?.ToString("yyyy-MM-dd");
            set => DataPagamento = DateTime.Parse(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeDataPagamentoField() => DataPagamento != null;

        #endregion
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ContribuinteEmitente
    {
        [XmlElement("identificacao")]
        public Identificacao Identificacao { get; set; }

        [XmlElement("razaoSocial")]
        public string RazaoSocial { get; set; }

        [XmlElement("endereco")]
        public string Endereco { get; set; }

        [XmlElement("municipio")]
        public string Municipio { get; set; }

        [XmlElement("uf")]
        public UFBrasil? UF { get; set; }

        [XmlElement("cep")]
        public string CEP { get; set; }

        [XmlElement("telefone")]
        public string Telefone { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeUF() => UF != null;

        #endregion
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Identificacao
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);
        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        #endregion
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ItensGNRE
    {
        [XmlElement("item")]
        public List<Item> Item { get; set; }

#if INTEROP
        #region Add - Interop

        public void AddItem(Item item)
        {
            if(Item == null)
            {
                Item = new List<Item>();
            }

            Item.Add(item);
        }

        #endregion
#endif
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Item
    {
        [XmlElement("receita")]
        public string Receita { get; set; }

        [XmlElement("detalhamentoReceita")]
        public string DetalhamentoReceita { get; set; }

        [XmlElement("documentoOrigem")]
        public DocumentoOrigem DocumentoOrigem { get; set; }

        [XmlElement("produto")]
        public string Produto { get; set; }

        [XmlElement("referencia")]
        public Referencia Referencia { get; set; }

        [XmlIgnore]
        public DateTime? DataVencimento { get; set; }

        [XmlElement("dataVencimento")]
        public string DataVencimentoField
        {
            get => DataVencimento?.ToString("yyyy-MM-dd");
            set => DataVencimento = DateTime.Parse(value);
        }

        [XmlElement("valor")]
        public List<Valor> Valor { get; set; }

        [XmlElement("convenio")]
        public string Convenio { get; set; }

        [XmlElement("contribuinteDestinatario")]
        public ContribuinteDestinatario ContribuinteDestinatario { get; set; }

        [XmlElement("camposExtras")]
        public CamposExtras CamposExtras { get; set; }

#if INTEROP
        #region Add - Interop

        public void AddValor(Valor valor)
        {
            if(Valor == null)
            {
                Valor = new List<Valor>();
            }

            Valor.Add(valor);
        }

        #endregion
#endif

        #region ShouldSerialize

        public bool ShouldSerializeDetalhamentoReceita() => !string.IsNullOrWhiteSpace(DetalhamentoReceita);
        public bool ShouldSerializeProduto() => !string.IsNullOrWhiteSpace(Produto);
        public bool ShouldSerializeDataVencimentoField() => DataVencimento != null;
        public bool ShouldSerializeConvenio() => !string.IsNullOrWhiteSpace(Convenio);

        #endregion
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class DocumentoOrigem
    {
        [XmlAttribute("tipo")]
        public string Tipo { get; set; }

        [XmlText()]
        public string Value { get; set; }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Referencia
    {
        private int? PeriodoField;

        [XmlElement("periodo")]
        public int? Periodo
        {
            get => PeriodoField;
            set
            {
                if(value < 0 || value > 5)
                {
                    throw new Exception("Conteúdo da tag <periodo> da tag <referencia> inválido! Conteúdos aceitos: 0 a 5.");
                }

                PeriodoField = value;
            }
        }

        [XmlElement("mes")]
        public Meses? Mes { get; set; }

        [XmlElement("ano")]
        public string Ano { get; set; }

        [XmlElement("parcela")]
        public string Parcela { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializePeriodo() => Periodo != null;
        public bool ShouldSerializeMes() => Mes != null;
        public bool ShouldSerializeAno() => !string.IsNullOrWhiteSpace(Ano) && Mes != null;
        public bool ShouldSerializeParcela() => !string.IsNullOrWhiteSpace(Parcela);

        #endregion
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Valor
    {
        [XmlAttribute("tipo")]
        public ItemValorTipo Tipo { get; set; }

        [XmlIgnore]
        public double ValorOriginal { get; set; }

        [XmlText()]
        public string Value
        {
            get => ValorOriginal.ToString("F2", CultureInfo.InvariantCulture);
            set => ValorOriginal = Utility.Converter.ToDouble(value);
        }

        public enum ItemValorTipo
        {
            [XmlEnum("11")]
            Item11,

            [XmlEnum("12")]
            Item12,

            [XmlEnum("21")]
            Item21,

            [XmlEnum("22")]
            Item22,

            [XmlEnum("31")]
            Item31,

            [XmlEnum("32")]
            Item32,

            [XmlEnum("41")]
            Item41,

            [XmlEnum("42")]
            Item42,

            [XmlEnum("51")]
            Item51,

            [XmlEnum("52")]
            Item52,
        }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ContribuinteDestinatario
    {
        [XmlElement("identificacao")]
        public Identificacao Identificacao { get; set; }

        [XmlElement("razaoSocial")]
        public string RazaoSocial { get; set; }

        [XmlElement("municipio")]
        public string Municipio { get; set; }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class CamposExtras
    {
        [XmlElement("campoExtra")]
        public List<CampoExtra> CampoExtra { get; set; }

#if INTEROP
        #region Add - Interop

        public void AddCampoExtra(CampoExtra campoExtra)
        {
            if(CampoExtra == null)
            {
                CampoExtra = new List<CampoExtra>();
            }

            CampoExtra.Add(campoExtra);
        }

        #endregion
#endif
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class CampoExtra
    {
        [XmlElement("codigo")]
        public int Codigo { get; set; }

        [XmlElement("valor")]
        public string Valor { get; set; }
    }
}