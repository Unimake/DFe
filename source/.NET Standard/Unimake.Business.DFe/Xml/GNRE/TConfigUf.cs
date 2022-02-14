#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.GNRE
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.TConfigUf")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("TConfigUf", Namespace = "http://www.gnre.pe.gov.br", IsNullable = false)]
    public class TConfigUf
    {
        [XmlElement("ambiente")]
        public TipoAmbiente Ambiente { get; set; }

        [XmlElement("uf")]
        public UFBrasil UF { get; set; }

        [XmlElement("situacaoConsulta")]
        public SituacaoConsulta SituacaoConsulta { get; set; }

        [XmlElement("exigeUfFavorecida")]
        public ExigeUfFavorecida ExigeUfFavorecida { get; set; }

        [XmlElement("exigeReceita")]
        public ExigeReceita ExigeReceita { get; set; }

        [XmlElement("receitas")]
        public List<Receitas> Receitas { get; set; }

        #region Add - Interop

        public void AddReceitas(Receitas receitas)
        {
            if(Receitas == null)
            {
                Receitas = new List<Receitas>();
            }

            Receitas.Add(receitas);
        }

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.SituacaoConsulta")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class SituacaoConsulta
    {
        [XmlElement("codigo")]
        public string Codigo { get; set; }

        [XmlElement("descricao")]
        public string Descricao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeUfFavorecida")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public virtual CamposGNRE Campo { get; set; } = CamposGNRE.c01_UfFavorecida;

        [XmlText()]
        public SimNaoLetra Value { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeReceita")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeReceita: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c02_receita;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Receitas")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Receitas
    {
        [XmlElement("receita")]
        public ReceitaConfigUF Receita { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ReceitaConfigUF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ReceitaConfigUF
    {
        [XmlAttribute("codigo")]
        public string Codigo { get; set; }

        [XmlAttribute("descricao")]
        public string Descricao { get; set; }

        [XmlElement("exigeContribuinteEmitente")]
        public SimNaoLetra ExigeContribuinteEmitente { get; set; }

        [XmlElement("exigeDetalhamentoReceita")]
        public ExigeDetalhamentoReceita ExigeDetalhamentoReceita { get; set; }

        [XmlElement("exigeProduto")]
        public ExigeProduto ExigeProduto { get; set; }

        [XmlElement("produtos")]
        public List<Produtos> Produtos { get; set; }

        [XmlElement("exigePeriodoReferencia")]
        public ExigePeriodoReferencia ExigePeriodoReferencia { get; set; }

        [XmlElement("exigePeriodoApuracao")]
        public ExigePeriodoApuracao ExigePeriodoApuracao { get; set; }

        [XmlElement("periodosApuracao")]
        public List<PeriodosApuracao> PeriodosApuracao { get; set; }

        [XmlElement("exigeParcela")]
        public ExigeParcela ExigeParcela { get; set; }

        [XmlElement("valorExigido")]
        public ValorExigido ValorExigido { get; set; }

        [XmlElement("exigeDocumentoOrigem")]
        public ExigeDocumentoOrigem ExigeDocumentoOrigem { get; set; }

        [XmlElement("tiposDocumentosOrigem")]
        public TiposDocumentosOrigem TiposDocumentosOrigem { get; set; }

        [XmlElement("versoesXmlDocOrigem")]
        public List<VersoesXmlDocOrigem> VersoesXmlDocOrigem { get; set; }

        [XmlElement("exigeContribuinteDestinatario")]
        public SimNaoLetra ExigeContribuinteDestinatario { get; set; }

        [XmlElement("exigeDataVencimento")]
        public ExigeDataVencimento ExigeDataVencimento { get; set; }

        [XmlElement("exigeDataPagamento")]
        public ExigeDataPagamento ExigeDataPagamento { get; set; }

        [XmlElement("exigeConvenio")]
        public ExigeConvenio ExigeConvenio { get; set; }

        [XmlElement("exigeValorFecp")]
        public ExigeValorFecp ExigeValorFecp { get; set; }

        [XmlElement("exigeCamposAdicionais")]
        public ExigeCamposAdicionais ExigeCamposAdicionais { get; set; }

        [XmlElement("camposAdicionais ")]
        public List<CamposAdicionais> CamposAdicionais { get; set; }

        #region Add - Interop

        public void AddProdutos(Produtos produtos)
        {
            if(Produtos == null)
            {
                Produtos = new List<Produtos>();
            }

            Produtos.Add(produtos);
        }

        public void AddPeriodosApuracao(PeriodosApuracao periodosApuracao)
        {
            if(PeriodosApuracao == null)
            {
                PeriodosApuracao = new List<PeriodosApuracao>();
            }

            PeriodosApuracao.Add(periodosApuracao);

        }

        public void AddVersoesXmlDocOrigem(VersoesXmlDocOrigem versoesXmlDocOrigem)
        {
            if(VersoesXmlDocOrigem == null)
            {
                VersoesXmlDocOrigem = new List<VersoesXmlDocOrigem>();
            }

            VersoesXmlDocOrigem.Add(versoesXmlDocOrigem);
        }

        public void AddCamposAdicionais(CamposAdicionais camposAdicionais)
        {
            if(CamposAdicionais == null)
            {
                CamposAdicionais = new List<CamposAdicionais>();
            }

            CamposAdicionais.Add(camposAdicionais);
        }

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeDetalhamentoReceita")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeDetalhamentoReceita
    {
        [XmlAttribute("campo")]
        public virtual CamposGNRE Campo { get; set; } = CamposGNRE.c25_detalhamentoReceita;

        [XmlAttribute("campo2_00")]
        public virtual CamposGNRE2 Campo_00 { get; set; } = CamposGNRE2.item_detalhamentoReceita;

        [XmlText()]
        public SimNaoLetra Value { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeProduto")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeProduto: ExigeDetalhamentoReceita
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c26_produto;

        [XmlAttribute("campo2_00")]
        public override CamposGNRE2 Campo_00 { get; set; } = CamposGNRE2.item_produto;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Produtos")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Produtos
    {
        [XmlElement("produto")]
        public Produto Produto { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Produto")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Produto: SituacaoConsulta { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigePeriodoReferencia")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigePeriodoReferencia: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c05_referencia;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigePeriodoApuracao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigePeriodoApuracao: ExigeDetalhamentoReceita
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.periodo;

        [XmlAttribute("campo2_00")]
        public override CamposGNRE2 Campo_00 { get; set; } = CamposGNRE2.item_referencia_periodo;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.PeriodosApuracao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class PeriodosApuracao
    {
        [XmlElement("periodoApuracao")]
        public PeriodoApuracao PeriodoApuracao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.PeriodoApuracao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class PeriodoApuracao: SituacaoConsulta { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeParcela")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeParcela: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.parcela;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ValorExigido")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ValorExigido: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeDocumentoOrigem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeDocumentoOrigem: ExigeDetalhamentoReceita
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c04_docOrigem;

        [XmlAttribute("campo2_00")]
        public override CamposGNRE2 Campo_00 { get; set; } = CamposGNRE2.item_documentoOrigem;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.TiposDocumentosOrigem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class TiposDocumentosOrigem
    {
        [XmlAttribute("campo")]
        public CamposGNRE Campo { get; set; } = CamposGNRE.c28_tipoDocOrigem;

        [XmlElement("tipoDocumentoOrigem")]
        public List<TipoDocumentoOrigem> TipoDocumentoOrigem { get; set; }

        #region Add - Interop

        public void AddPeriodosApuracao(TipoDocumentoOrigem tipoDocumentoOrigem)
        {
            if(TipoDocumentoOrigem == null)
            {
                TipoDocumentoOrigem = new List<TipoDocumentoOrigem>();
            }

            TipoDocumentoOrigem.Add(tipoDocumentoOrigem);

        }

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.TipoDocumentoOrigem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class TipoDocumentoOrigem: SituacaoConsulta { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.VersoesXmlDocOrigem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class VersoesXmlDocOrigem
    {
        private string VersaoField;

        [XmlElement("versao")]
        public string Versao
        {
            get => VersaoField;
            set
            {
                if(value != "2.00" || value != "1.00")
                {
                    throw new Exception("Conteúdo da tag <versoesXmlDocOrigem><versao> incorreta! Valores aceitos: 1.00 e 2.00");
                }

                VersaoField = value;
            }
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeDataVencimento")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeDataVencimento: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c14_dataVencimento;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeDataPagamento")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeDataPagamento: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c33_dataPagamento;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeConvenio")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeConvenio: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c15_convenio;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeValorFecp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeValorFecp
    {
        [XmlAttribute("campo")]
        public virtual CamposGNRE2 Campo { get; set; } = CamposGNRE2.item_valorPrincipalFecp;

        [XmlText()]
        public SimNaoOpcionalLetra Value { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeCamposAdicionais")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeCamposAdicionais: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c39_camposExtras;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.CamposAdicionais")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class CamposAdicionais
    {
        [XmlElement("campoAdicional")]
        public CampoAdicional CampoAdicional { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.CampoAdicional")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class CampoAdicional
    {
        [XmlAttribute("campo")]
        public virtual CamposGNRE Campo { get; set; } = CamposGNRE.campoExtra;

        [XmlElement("obrigatorio")]
        public SimNaoLetra Obrigatorio { get; set; }

        [XmlElement("codigo")]
        public Codigo Codigo { get; set; }

        [XmlElement("tipo")]
        public Tipo Tipo { get; set; }

        [XmlElement("tamanho")]
        public int Tamanho { get; set; }

        [XmlElement("casasDecimais")]
        public int CasasDecimais { get; set; }

        [XmlElement("titulo")]
        public string Titulo { get; set; }

        [XmlElement("versoesXmlCampoAdicional")]
        public List<VersoesXmlCampoAdicional> VersoesXmlCampoAdicional { get; set; }

        #region Add - Interop

        public void AddVersoesXmlCampoAdicional(VersoesXmlCampoAdicional versoesXmlCampoAdicional)
        {
            if(VersoesXmlCampoAdicional == null)
            {
                VersoesXmlCampoAdicional = new List<VersoesXmlCampoAdicional>();
            }

            VersoesXmlCampoAdicional.Add(versoesXmlCampoAdicional);
        }

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Codigo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Codigo
    {
        [XmlAttribute("campo")]
        public virtual CamposGNRE Campo { get; set; } = CamposGNRE.codigo;

        [XmlText()]
        public int Value { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Tipo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Tipo
    {
        [XmlAttribute("campo")]
        public virtual CamposGNRE Campo { get; set; } = CamposGNRE.tipo;

        [XmlText()]
        public TipoCampoExtraGNRE Value { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.VersoesXmlCampoAdicional")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class VersoesXmlCampoAdicional: VersoesXmlDocOrigem { }
}
