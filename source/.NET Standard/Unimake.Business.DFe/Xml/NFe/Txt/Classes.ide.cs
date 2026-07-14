using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Ide
    /// </summary>
    internal class Ide
    {
        public int cUF { get; set; }
        public int cNF { get; set; }
        public string natOp { get; set; }
        public TpcnMod mod { get; set; }
        public int serie { get; set; }
        public int nNF { get; set; }
        public DateTime dEmi { get; set; }
        public DateTime dSaiEnt { get; set; }
        public DateTime hSaiEnt { get; set; }
        public DateTime dPrevEntrega { get; set; }
        public TpcnTipoNFe tpNF { get; set; }
        public int cMunFG { get; set; }
        public int cMunFGIBS { get; set; }
        public List<NFref> NFref { get; set; }
        public TpcnTipoImpressao tpImp { get; set; }
        public TipoEmissao tpEmis { get; set; }
        public int cDV { get; set; }
        public TipoAmbiente tpAmb { get; set; }
        public TpcnFinalidadeNFe finNFe { get; set; }
        public TpcnTipoNFDebito tpNFDebito {  get; set; }
        public TpcnTipoNFCredito tpNFCredito {  get; set; }
        public TpcnProcessoEmissao procEmi { get; set; }
        public string verProc { get; set; }
        public string dhCont { get; set; }  //mudado para 'string' pq na versao 3 (NFC-e) deve ter o time-zone
        public string xJust { get; set; }

        /// <summary>
        /// NFC-e
        /// </summary>
        public string dhEmi { get; set; }
        public string dhSaiEnt { get; set; }
        public TpcnDestinoOperacao idDest { get; set; }
        public TpcnConsumidorFinal indFinal { get; set; }
        public TpcnPresencaComprador indPres { get; set; }
        public TpcnIntermediario? indIntermed { get; set; }
        public GCompraGov gCompraGov { get; set; }
        public GPagAntecipado gPagAntecipado { get; set; }

        public Ide()
        {
            this.dhEmi =
                this.dhSaiEnt =
                this.dhCont = string.Empty;

            this.idDest = TpcnDestinoOperacao.doInterna;
            this.indFinal = TpcnConsumidorFinal.cfConsumidorFinal;
            this.indPres = TpcnPresencaComprador.pcPresencial;
            this.indIntermed = TpcnIntermediario.OperacaoSemIntermediador;
            this.tpAmb = TipoAmbiente.Homologacao;
            this.tpEmis = TipoEmissao.Normal;
            this.tpImp = TpcnTipoImpressao.tiRetrato;
            this.procEmi = TpcnProcessoEmissao.peAplicativoContribuinte;
            NFref = new List<NFref>();
            gCompraGov = new GCompraGov();
            gPagAntecipado = new GPagAntecipado();
        }
    }

    /// <summary>
    /// NFref
    /// </summary>
    internal class NFref
    {
        public string refNFe { get; set; }
        public string refCTe { get; set; }
        public refNF refNF { get; set; }
        public refNFP refNFP { get; set; }
        public refECF refECF { get; set; }

        public NFref()
        {
            refNF = null;
            refNFP = null;
            refECF = null;
            this.refCTe = this.refNFe = string.Empty;
        }
        public NFref(string refNFe, string refCTe)
        {
            this.refNFe = refNFe;
            this.refCTe = refCTe;
        }
    }

    /// <summary>
    /// RefECF
    /// </summary>
    internal class refECF
    {
        public string mod { get; set; }
        public int nECF { get; set; }
        public int nCOO { get; set; }
    }

    /// <summary>
    /// RefNF
    /// </summary>
    internal class refNF
    {
        public int cUF { get; set; }
        public string AAMM { get; set; }
        public string CNPJ { get; set; }
        public string mod { get; set; }
        public int serie { get; set; }
        public int nNF { get; set; }
    }

    /// <summary>
    /// RefNFP
    /// </summary>
    internal class refNFP
    {
        public int cUF { get; set; }
        public string AAMM { get; set; }
        public string CNPJ { get; set; }
        public string CPF { get; set; }
        public string IE { get; set; }
        public string mod { get; set; }
        public int serie { get; set; }
        public int nNF { get; set; }
        public refNFP()
        {
            this.CNPJ = this.CPF = string.Empty;
        }
    }

    /// <summary>
    /// gCompraGov
    /// </summary>
    internal class GCompraGov
    {
        public TpcnTipoEnteGovernamental tpEnteGov {  get; set; }

        public double pRedutor {  get; set; }

        public TpcnTipoOperacaoEnteGovernamental tpOperGov { get; set; }
    }

    internal class GPagAntecipado
    {
        public List<string> refNFe { get; set; } = new List<string>();
    }
}
