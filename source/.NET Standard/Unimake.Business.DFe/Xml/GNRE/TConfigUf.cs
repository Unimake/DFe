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
    public class TConfigUf : XMLBase
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

        [XmlElement("versoesXml")]
        public List<VersaoXml> VersoesXml { get; set; }

        [XmlElement("qtdMaximas")]
        public QtdMaximas QtdMaximas { get; set; }

        [XmlElement("tiposGnreDaUF")]
        public TiposGnreDaUF TiposGnreDaUF { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="receitas">Elemento</param>
        public void AddReceitas(Receitas receitas)
        {
            if (Receitas == null)
            {
                Receitas = new List<Receitas>();
            }

            Receitas.Add(receitas);
        }

        /// <summary>
        /// Retorna o elemento da lista Receitas (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Receitas</returns>
        public Receitas GetReceitas(int index)
        {
            if ((Receitas?.Count ?? 0) == 0)
            {
                return default;
            };

            return Receitas[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Receitas
        /// </summary>
        public int GetReceitasCount => (Receitas != null ? Receitas.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddVersoesXml(VersaoXml item)
        {
            if (VersoesXml == null)
            {
                VersoesXml = new List<VersaoXml>();
            }

            VersoesXml.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista VersoesXml (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da VersoesXml</returns>
        public VersaoXml GetVersoesXml(int index)
        {
            if ((Receitas?.Count ?? 0) == 0)
            {
                return default;
            };

            return VersoesXml[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista VersoesXml
        /// </summary>
        public int GetVersoesXmlCount => (VersoesXml != null ? VersoesXml.Count : 0);

#endif
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
    public class ExigeReceita : ExigeUfFavorecida
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

        [XmlAttribute("courier")]
        public SimNaoLetra Courier { get; set; }

        [XmlAttribute("descricao")]
        public string Descricao { get; set; }

        [XmlElement("exigeContribuinteEmitente")]
        public SimNaoLetra ExigeContribuinteEmitente { get; set; }

        [XmlElement("exigeDetalhamentoReceita")]
        public ExigeDetalhamentoReceita ExigeDetalhamentoReceita { get; set; }

        [XmlElement("detalhamentosReceita")]
        public DetalhamentosReceita DetalhamentosReceita { get; set; }

        [XmlElement("exigeProduto")]
        public ExigeProduto ExigeProduto { get; set; }

        [XmlElement("produtos")]
        public Produtos Produtos { get; set; }

        [XmlElement("exigePeriodoReferencia")]
        public ExigePeriodoReferencia ExigePeriodoReferencia { get; set; }

        [XmlElement("exigePeriodoApuracao")]
        public ExigePeriodoApuracao ExigePeriodoApuracao { get; set; }

        [XmlElement("periodosApuracao")]
        public PeriodosApuracao PeriodosApuracao { get; set; }

        [XmlElement("exigeParcela")]
        public ExigeParcela ExigeParcela { get; set; }

        [XmlElement("valorExigido")]
        public ValorExigido ValorExigido { get; set; }

        [XmlElement("exigeDocumentoOrigem")]
        public ExigeDocumentoOrigem ExigeDocumentoOrigem { get; set; }

        [XmlElement("tiposDocumentosOrigem")]
        public TiposDocumentosOrigem TiposDocumentosOrigem { get; set; }

        [XmlElement("versoesXmlDocOrigem")]
        public VersoesXmlDocOrigem VersoesXmlDocOrigem { get; set; }

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

        [XmlElement("camposAdicionais")]
        public CamposAdicionais CamposAdicionais { get; set; }

        [XmlElement("tiposGnre")]
        public TiposGnre TiposGnre { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCourier() => Courier == SimNaoLetra.Sim;

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
        public virtual CamposGNRE Campo { get; set; } = CamposGNRE.detalhamentoReceita;

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
    public class ExigeProduto : ExigeDetalhamentoReceita
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.produto;
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
        public List<Produto> Produto { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddProduto(Produto item)
        {
            if (Produto == null)
            {
                Produto = new List<Produto>();
            }

            Produto.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Produto (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Produto</returns>
        public Produto GetProduto(int index)
        {
            if ((Produto?.Count ?? 0) == 0)
            {
                return default;
            };

            return Produto[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Produto
        /// </summary>
        public int GetProdutoCount => (Produto != null ? Produto.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Produto")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Produto : SituacaoConsulta { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigePeriodoReferencia")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigePeriodoReferencia : ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.referencia;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigePeriodoApuracao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigePeriodoApuracao : ExigeDetalhamentoReceita
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.periodo;
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
        public List<PeriodoApuracao> PeriodoApuracao { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPeriodoApuracao(PeriodoApuracao item)
        {
            if (PeriodoApuracao == null)
            {
                PeriodoApuracao = new List<PeriodoApuracao>();
            }

            PeriodoApuracao.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PeriodoApuracao (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PeriodoApuracao</returns>
        public PeriodoApuracao GetPeriodoApuracao(int index)
        {
            if ((PeriodoApuracao?.Count ?? 0) == 0)
            {
                return default;
            };

            return PeriodoApuracao[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista PeriodoApuracao
        /// </summary>
        public int GetPeriodoApuracaoCount => (PeriodoApuracao != null ? PeriodoApuracao.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.PeriodoApuracao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class PeriodoApuracao : SituacaoConsulta { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeParcela")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeParcela : ExigeUfFavorecida
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
    public class ValorExigido
    {
        [XmlAttribute("campo")]
        public virtual CamposGNRE Campo { get; set; }

        [XmlText()]
        public string Value { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeDocumentoOrigem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeDocumentoOrigem : ExigeDetalhamentoReceita
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.documentoOrigem;
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
        public CamposGNRE Campo { get; set; } = CamposGNRE.documentoOrigem;

        [XmlElement("tipoDocumentoOrigem")]
        public List<TipoDocumentoOrigem> TipoDocumentoOrigem { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="tipoDocumentoOrigem">Elemento</param>
        public void AddTipoDocumentoOrigem(TipoDocumentoOrigem tipoDocumentoOrigem)
        {
            if (TipoDocumentoOrigem == null)
            {
                TipoDocumentoOrigem = new List<TipoDocumentoOrigem>();
            }

            TipoDocumentoOrigem.Add(tipoDocumentoOrigem);
        }

        /// <summary>
        /// Retorna o elemento da lista TipoDocumentoOrigem (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TipoDocumentoOrigem</returns>
        public TipoDocumentoOrigem GetTipoDocumentoOrigem(int index)
        {
            if ((TipoDocumentoOrigem?.Count ?? 0) == 0)
            {
                return default;
            };

            return TipoDocumentoOrigem[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TipoDocumentoOrigem
        /// </summary>
        public int GetTipoDocumentoOrigemCount => (TipoDocumentoOrigem != null ? TipoDocumentoOrigem.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.TipoDocumentoOrigem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class TipoDocumentoOrigem : SituacaoConsulta { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.VersaoXml")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class VersaoXml : VersoesXmlDocOrigem { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.VersoesXmlDocOrigem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class VersoesXmlDocOrigem
    {
        [XmlElement("versao")]
        public List<string> Versao { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddVersao(string item)
        {
            if (Versao == null)
            {
                Versao = new List<string>();
            }

            Versao.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Versao (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro</returns>
        public string GetVersao(int index)
        {
            if ((Versao?.Count ?? 0) == 0)
            {
                return default;
            };

            return Versao[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Versao
        /// </summary>
        public int GetVersaoCount => (Versao != null ? Versao.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeDataVencimento")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeDataVencimento : ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.dataVencimento;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeDataPagamento")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeDataPagamento : ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.dataPagamento;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.ExigeConvenio")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class ExigeConvenio : ValorExigido
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.convenio;
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
        public virtual CamposGNRE2 Campo { get; set; } = CamposGNRE2.valor;

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
    public class ExigeCamposAdicionais : ExigeUfFavorecida
    {
        [XmlAttribute("campo")]
        public override CamposGNRE Campo { get; set; } = CamposGNRE.camposExtras;
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
        public List<CampoAdicional> CampoAdicional { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddCampoAdicional(CampoAdicional item)
        {
            if (CampoAdicional == null)
            {
                CampoAdicional = new List<CampoAdicional>();
            }

            CampoAdicional.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista CampoAdicional (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da CampoAdicional</returns>
        public CampoAdicional GetCampoAdicional(int index)
        {
            if ((CampoAdicional?.Count ?? 0) == 0)
            {
                return default;
            };

            return CampoAdicional[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista CampoAdicional
        /// </summary>
        public int GetCampoAdicionalCount => (CampoAdicional != null ? CampoAdicional.Count : 0);

#endif
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
        public int? Tamanho { get; set; }

        [XmlElement("casasDecimais")]
        public int? CasasDecimais { get; set; }

        [XmlElement("titulo")]
        public string Titulo { get; set; }

        [XmlElement("versoesXmlCampoAdicional")]
        public VersoesXmlCampoAdicional VersoesXmlCampoAdicional { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCasasDecimais() => CasasDecimais != null;
        public bool ShouldSerializeTamanho() => Tamanho != null;
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
    public class VersoesXmlCampoAdicional : VersoesXmlDocOrigem { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.QtdMaximas")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class QtdMaximas
    {
        [XmlElement("guiasPorLote")]
        public string GuiasPorLote { get; set; }

        [XmlElement("itensPorGuia")]
        public string ItensPorGuia { get; set; }

        [XmlElement("itensPorLote")]
        public string ItensPorLote { get; set; }

        [XmlElement("qtdConsultas")]
        public string QtdConsultas { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.DetalhamentosReceita")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class DetalhamentosReceita
    {
        [XmlElement("detalhamentoReceita")]
        public List<DetalhamentoReceita> DetalhamentoReceita { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDetalhamentoReceita(DetalhamentoReceita item)
        {
            if (DetalhamentoReceita == null)
            {
                DetalhamentoReceita = new List<DetalhamentoReceita>();
            }

            DetalhamentoReceita.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DetalhamentoReceita (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetalhamentoReceita</returns>
        public DetalhamentoReceita GetDetalhamentoReceita(int index)
        {
            if ((DetalhamentoReceita?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetalhamentoReceita[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetalhamentoReceita
        /// </summary>
        public int GetDetalhamentoReceitaCount => (DetalhamentoReceita != null ? DetalhamentoReceita.Count : 0);


#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.DetalhamentoReceita")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class DetalhamentoReceita : SituacaoConsulta { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.TiposGnre")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class TiposGnre
    {
        [XmlElement("tipoGnre")]
        public TiposDeGNRE TipoGnre { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.TiposGnreDaUF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class TiposGnreDaUF
    {
        [XmlElement("TipoGnre")]
        private List<TiposDeGNRE> TipoGnre { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReceitas(TiposDeGNRE item)
        {
            if (TipoGnre == null)
            {
                TipoGnre = new List<TiposDeGNRE>();
            }

            TipoGnre.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TipoGnre (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TipoGnre</returns>
        public TiposDeGNRE GetReceitas(int index)
        {
            if ((TipoGnre?.Count ?? 0) == 0)
            {
                return default;
            };

            return TipoGnre[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TipoGnre
        /// </summary>
        public int GetReceitasCount => (TipoGnre != null ? TipoGnre.Count : 0);

#endif
    }
}
