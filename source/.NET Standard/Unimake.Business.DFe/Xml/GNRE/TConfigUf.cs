#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.GNRE
{
    [ComVisible(true)]
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

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class SituacaoConsulta
    {
        [XmlElement("codigo")]
        public string Codigo { get; set; }

        [XmlElement("descricao")]
        public string Descricao { get; set; }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public virtual CamposGNRE Campo { get; set; } = CamposGNRE.c01_UfFavorecida;

        [XmlText()]
        public SimNaoLetra Value { get; set; }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeReceita: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c02_receita;
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Receitas
    {
        [XmlElement("receita")]
        public ReceitaConfigUF Receita { get; set; }
    }

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

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeProduto: ExigeDetalhamentoReceita
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c26_produto;

        [XmlAttribute("campo2_00")]
        public override CamposGNRE2 Campo_00 { get; set; } = CamposGNRE2.item_produto;
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Produtos
    {
        [XmlElement("produto")]
        public Produto Produto { get; set; }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Produto: SituacaoConsulta { }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigePeriodoReferencia: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c05_referencia;
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigePeriodoApuracao: ExigeDetalhamentoReceita
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.periodo;

        [XmlAttribute("campo2_00")]
        public override CamposGNRE2 Campo_00 { get; set; } = CamposGNRE2.item_referencia_periodo;
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class PeriodosApuracao
    {
        [XmlElement("periodoApuracao")]
        public PeriodoApuracao PeriodoApuracao { get; set; }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class PeriodoApuracao: SituacaoConsulta { }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeParcela: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.parcela;
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ValorExigido: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeDocumentoOrigem: ExigeDetalhamentoReceita
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c04_docOrigem;

        [XmlAttribute("campo2_00")]
        public override CamposGNRE2 Campo_00 { get; set; } = CamposGNRE2.item_documentoOrigem;
    }

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

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class TipoDocumentoOrigem: SituacaoConsulta { }

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

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeDataVencimento: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c14_dataVencimento;
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeDataPagamento: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c33_dataPagamento;
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeConvenio: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c15_convenio;
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeValorFecp
    {
        [XmlAttribute("campo")]
        public virtual CamposGNRE2 Campo { get; set; } = CamposGNRE2.item_valorPrincipalFecp;

        [XmlText()]
        public SimNaoOpcionalLetra Value { get; set; }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeCamposAdicionais: ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.c39_camposExtras;
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class CamposAdicionais
    {
        [XmlElement("campoAdicional")]
        public CampoAdicional CampoAdicional { get; set; }
    }

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

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Codigo
    {
        [XmlAttribute("campo")]
        public virtual CamposGNRE Campo { get; set; } = CamposGNRE.codigo;

        [XmlText()]
        public int Value { get; set; }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Tipo
    {
        [XmlAttribute("campo")]
        public virtual CamposGNRE Campo { get; set; } = CamposGNRE.tipo;

        [XmlText()]
        public TipoCampoExtraGNRE Value { get; set; }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class VersoesXmlCampoAdicional: VersoesXmlDocOrigem { }
}
