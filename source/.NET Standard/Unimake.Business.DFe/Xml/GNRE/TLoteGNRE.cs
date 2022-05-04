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
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.TLoteGNRE")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("TLote_GNRE", Namespace = "http://www.gnre.pe.gov.br", IsNullable = false)]
    public class TLoteGNRE : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; } = "2.00";

        [XmlElement("guias")]
        public Guias Guias { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Guias")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Guias
    {
        [XmlElement("TDadosGNRE")]
        public List<TDadosGNRE> TDadosGNRE { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="tdadosGNRE">Elemento</param>
        public void AddTDadosGNRE(TDadosGNRE tdadosGNRE)
        {
            if (TDadosGNRE == null)
            {
                TDadosGNRE = new List<TDadosGNRE>();
            }

            TDadosGNRE.Add(tdadosGNRE);
        }

        /// <summary>
        /// Retorna o elemento da lista TDadosGNRE (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TDadosGNRE</returns>
        public TDadosGNRE GetTDadosGNRE(int index)
        {
            if ((TDadosGNRE?.Count ?? 0) == 0)
            {
                return default;
            };

            return TDadosGNRE[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TDadosGNRE
        /// </summary>
        public int GetTDadosGNRECount => (TDadosGNRE != null ? TDadosGNRE.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.TDadosGNRE")]
    [ComVisible(true)]
#endif
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
        public DateTime DataPagamento { get; set; }

        [XmlElement("dataPagamento")]
        public string DataPagamentoField
        {
            get => DataPagamento.ToString("yyyy-MM-dd");
            set => DataPagamento = DateTime.Parse(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeDataPagamentoField() => DataPagamento > DateTime.MinValue;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ContribuinteEmitente")]
    [ComVisible(true)]
#endif
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
#if INTEROP
        public UFBrasil UF { get; set; } = UFBrasil.NaoDefinido;
#else
        public UFBrasil? UF { get; set; }
#endif

        [XmlElement("cep")]
        public string CEP { get; set; }

        [XmlElement("telefone")]
        public string Telefone { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeUF() => UF != null && UF != UFBrasil.NaoDefinido;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Identificacao")]
    [ComVisible(true)]
#endif
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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ItensGNRE")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ItensGNRE
    {
        [XmlElement("item")]
        public List<Item> Item { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddItem(Item item)
        {
            if (Item == null)
            {
                Item = new List<Item>();
            }

            Item.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Item (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Item</returns>
        public Item GetItem(int index)
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
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Item")]
    [ComVisible(true)]
#endif
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
        public DateTime DataVencimento { get; set; }

        [XmlElement("dataVencimento")]
        public string DataVencimentoField
        {
            get => DataVencimento.ToString("yyyy-MM-dd");
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

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="valor">Elemento</param>
        public void AddValor(Valor valor)
        {
            if (Valor == null)
            {
                Valor = new List<Valor>();
            }

            Valor.Add(valor);
        }

        /// <summary>
        /// Retorna o elemento da lista Valor (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Valor</returns>
        public Valor GetValor(int index)
        {
            if ((Valor?.Count ?? 0) == 0)
            {
                return default;
            };

            return Valor[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Valor
        /// </summary>
        public int GetValorCount => (Valor != null ? Valor.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeDetalhamentoReceita() => !string.IsNullOrWhiteSpace(DetalhamentoReceita);
        public bool ShouldSerializeProduto() => !string.IsNullOrWhiteSpace(Produto);
        public bool ShouldSerializeDataVencimentoField() => DataVencimento > DateTime.MinValue;
        public bool ShouldSerializeConvenio() => !string.IsNullOrWhiteSpace(Convenio);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.DocumentoOrigem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class DocumentoOrigem
    {
        [XmlAttribute("tipo")]
        public string Tipo { get; set; }

        [XmlText()]
        public string Value { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Referencia")]
    [ComVisible(true)]
#endif
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
                if (value < 0 || value > 5)
                {
                    throw new Exception("Conteúdo da TAG <periodo>, filha da TAG <referencia>, inválido! Conteúdos aceitos: 0 a 5.");
                }

                PeriodoField = value;
            }
        }

        [XmlElement("mes")]
#if INTEROP
        public Meses Mes { get; set; } = (Meses)(-1);
#else
        public Meses? Mes { get; set; }
#endif

        [XmlElement("ano")]
        public string Ano { get; set; }

        [XmlElement("parcela")]
        public string Parcela { get; set; }

#region ShouldSerialize

        public bool ShouldSerializePeriodo() => Periodo != null;
        public bool ShouldSerializeMes() => Mes != null && Mes != (Meses)(-1);
        public bool ShouldSerializeAno() => !string.IsNullOrWhiteSpace(Ano) && Mes != null && Mes != (Meses)(-1);
        public bool ShouldSerializeParcela() => !string.IsNullOrWhiteSpace(Parcela);

#endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Valor")]
    [ComVisible(true)]
#endif
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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ContribuinteDestinatario")]
    [ComVisible(true)]
#endif
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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.CamposExtras")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class CamposExtras
    {
        [XmlElement("campoExtra")]
        public List<CampoExtra> CampoExtra { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="campoExtra">Elemento</param>
        public void AddCampoExtra(CampoExtra campoExtra)
        {
            if (CampoExtra == null)
            {
                CampoExtra = new List<CampoExtra>();
            }

            CampoExtra.Add(campoExtra);
        }

        /// <summary>
        /// Retorna o elemento da lista CampoExtra (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da CampoExtra</returns>
        public CampoExtra GetCampoExtra(int index)
        {
            if ((CampoExtra?.Count ?? 0) == 0)
            {
                return default;
            };

            return CampoExtra[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista CampoExtra
        /// </summary>
        public int GetCampoExtraCount => (CampoExtra != null ? CampoExtra.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.CampoExtra")]
    [ComVisible(true)]
#endif
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