using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{
    /// <summary>Informações de IBS/CBS relativas a operações de condomínio.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Condominios")]
    [ComVisible(true)]
#endif
    public class Condominios
    {
        /// <summary>Data de vencimento original das cobranças.</summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DVencOrig { get; set; }
#else
        public DateTimeOffset DVencOrig { get; set; }
#endif

        /// <summary>Representação XML da data de vencimento original.</summary>
        [XmlElement("dVencOrig")]
        public string DVencOrigField
        {
            get => DVencOrig.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture);
#if INTEROP
            set => DVencOrig = DateTime.ParseExact(value, "yyyy-MM-dd", CultureInfo.InvariantCulture);
#else
            set => DVencOrig = DateTimeOffset.ParseExact(value, "yyyy-MM-dd", CultureInfo.InvariantCulture);
#endif
        }

        /// <summary>Cobranças das taxas e demais valores condominiais.</summary>
        [XmlElement("gCobranca")]
        public List<CobrancaCondominio> GCobranca { get; set; }

        /// <summary>Descontos concedidos nas cobranças condominiais.</summary>
        [XmlElement("gDesconto")]
        public List<DescontoCondominio> GDesconto { get; set; }

#if INTEROP
        /// <summary>Adiciona uma cobrança.</summary>
        public void AddGCobranca(CobrancaCondominio item)
        {
            if (GCobranca == null)
            {
                GCobranca = new List<CobrancaCondominio>();
            }
            GCobranca.Add(item);
        }

        /// <summary>Obtém uma cobrança pelo índice.</summary>
        public CobrancaCondominio GetGCobranca(int index) => GCobranca == null || GCobranca.Count == 0 ? default : GCobranca[index];

        /// <summary>Quantidade de itens na lista.</summary>
        public int GetGCobrancaCount => GCobranca?.Count ?? 0;

        /// <summary>Adiciona um desconto.</summary>
        public void AddGDesconto(DescontoCondominio item)
        {
            if (GDesconto == null)
            {
                GDesconto = new List<DescontoCondominio>();
            }
            GDesconto.Add(item);
        }

        /// <summary>Obtém um desconto pelo índice.</summary>
        public DescontoCondominio GetGDesconto(int index) => GDesconto == null || GDesconto.Count == 0 ? default : GDesconto[index];

        /// <summary>Quantidade de itens na lista.</summary>
        public int GetGDescontoCount => GDesconto?.Count ?? 0;
#endif
    }

    /// <summary>Cobrança de taxa ou outro valor condominial.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.CobrancaCondominio")]
    [ComVisible(true)]
#endif
    public class CobrancaCondominio
    {
        /// <summary>Tipo de cobrança.</summary>
        [XmlElement("tpCobranca")]
        public TipoCobrancaCondominioNFSe TpCobranca { get; set; }

        /// <summary>Descrição para outra cobrança.</summary>
        [XmlElement("xTpCobranca")]
        public string XTpCobranca { get; set; }

        /// <summary>Valor da cobrança em reais.</summary>
        [XmlIgnore]
        public decimal VCobranca { get; set; }

        /// <summary>Representação XML de vCobranca.</summary>
        [XmlElement("vCobranca")]
        public string VCobrancaField
        {
            get => VCobranca.ToString("F2", CultureInfo.InvariantCulture);
            set => VCobranca = XmlConvert.ToDecimal(value);
        }

        /// <summary>Detalhamentos da cobrança.</summary>
        [XmlElement("gDetCobranca")]
        public List<DetalhamentoCobrancaCondominio> GDetCobranca { get; set; }

#if INTEROP
        /// <summary>Adiciona um detalhamento.</summary>
        public void AddGDetCobranca(DetalhamentoCobrancaCondominio item)
        {
            if (GDetCobranca == null)
            {
                GDetCobranca = new List<DetalhamentoCobrancaCondominio>();
            }
            GDetCobranca.Add(item);
        }

        /// <summary>Obtém um detalhamento pelo índice.</summary>
        public DetalhamentoCobrancaCondominio GetGDetCobranca(int index) => GDetCobranca == null || GDetCobranca.Count == 0 ? default : GDetCobranca[index];

        /// <summary>Quantidade de itens na lista.</summary>
        public int GetGDetCobrancaCount => GDetCobranca?.Count ?? 0;
#endif
    }

    /// <summary>Detalhamento de uma cobrança condominial.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.DetalhamentoCobrancaCondominio")]
    [ComVisible(true)]
#endif
    public class DetalhamentoCobrancaCondominio
    {
        /// <summary>Tipo de detalhamento.</summary>
        [XmlElement("tpDetCobranca")]
        public TipoDetalhamentoCobrancaCondominioNFSe TpDetCobranca { get; set; }

        /// <summary>Descrição para outro tipo de detalhamento.</summary>
        [XmlElement("xTpDetCobranca")]
        public string XTpDetCobranca { get; set; }

        /// <summary>Valor do detalhamento em reais.</summary>
        [XmlIgnore]
        public decimal VDetCobranca { get; set; }

        /// <summary>Representação XML de vDetCobranca.</summary>
        [XmlElement("vDetCobranca")]
        public string VDetCobrancaField
        {
            get => VDetCobranca.ToString("F2", CultureInfo.InvariantCulture);
            set => VDetCobranca = XmlConvert.ToDecimal(value);
        }
    }

    /// <summary>Desconto concedido pelo condomínio.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.DescontoCondominio")]
    [ComVisible(true)]
#endif
    public class DescontoCondominio
    {
        /// <summary>Tipo de desconto.</summary>
        [XmlElement("tpDesconto")]
        public TipoDescontoCondominioNFSe TpDesconto { get; set; }

        /// <summary>Valor do desconto em reais.</summary>
        [XmlIgnore]
        public decimal VDesconto { get; set; }

        /// <summary>Representação XML de vDesconto.</summary>
        [XmlElement("vDesconto")]
        public string VDescontoField
        {
            get => VDesconto.ToString("F2", CultureInfo.InvariantCulture);
            set => VDesconto = XmlConvert.ToDecimal(value);
        }

        /// <summary>Descrição do desconto.</summary>
        [XmlElement("xTpDesconto")]
        public string XTpDesconto { get; set; }
    }
}
