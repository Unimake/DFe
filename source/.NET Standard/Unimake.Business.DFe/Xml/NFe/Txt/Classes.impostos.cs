using System;
namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// COFINS
    /// </summary>
    internal struct COFINS
    {
        public string CST;
        public double vBC;
        public double pCOFINS;
        public double vCOFINS;
        public double vBCProd;
        public double vAliqProd;
        public double qBCProd;
    }

    /// <summary>
    /// COFINSST
    /// </summary>
    internal struct COFINSST
    {
        public double vBC;
        public double pCOFINS;
        public double qBCProd;
        public double vAliqProd;
        public double vCOFINS;
        public string indSomaCOFINSST;
    }

    /// <summary>
    /// ICMS
    /// </summary>
    internal struct ICMS
    {
        public TpcnOrigemMercadoria orig;
        public string CST;
        public int ICMSPart10;
        public int ICMSPart90;
        public int ICMSst;
        public TpcnDeterminacaoBaseIcms modBC;
        public double pRedBC;
        public double vBC;
        public double pICMS;
        public double vICMS;
        public TpcnDeterminacaoBaseIcmsST modBCST;
        public double pMVAST;
        public double pRedBCST;
        public double vBCST;
        public double pICMSST;
        public double vICMSST;
        public int motDesICMS;
        public string indDeduzDeson;
        public double pBCOp;
        public string UFST;
        public double vBCSTRet;
        public double vICMSSTRet;
        public double vBCSTDest;
        public double vICMSSTDest;
        public double vICMSDeson;
        public double vICMSDif;
        public double pDif;
        public double vICMSOp;
        public double pFCP;
        public double vFCP;
        public double pFCPDif;
        public double vFCPDif;
        public double vFCPEfet;
        public double vBCFCP;
        public double vBCFCPST;
        public double pFCPST;
        public double vFCPST;
        public double vICMSSTDeson;
        public int motDesICMSST;
        public double pST;
        public double vBCFCPSTRet;
        public double pFCPSTRet;
        public double vFCPSTRet;
        public double pRedBCEfet;
        public double vBCEfet;
        public double pICMSEfet;
        public double vICMSEfet;
        public double vICMSSubstituto;
        public double qBCMono;
        public double adRemICMS;
        public double vICMSMono;
        public double qBCMonoReten;
        public double adRemICMSReten;
        public double vICMSMonoReten;
        public double vICMSMonoOp;
        public double vICMSMonoDif;
        public double pRedAdRem;
        public int motRedAdRem;
        public string cBenefRBC;

        public decimal qBCMonoRet;
        public double adRemICMSRet;
        public double vICMSMonoRet;

        //-- CSON
        public int CSOSN;
        public double pCredSN;
        public double vCredICMSSN;

        public ICMSUFDest ICMSUFDest;
    }

    /// <summary>
    /// ICMSTot
    /// </summary>
    internal struct ICMSTot
    {
        public double vBC;
        public double vICMS;
        public double vICMSDeson;
        public double vICMSUFDest;
        public double vFCPUFDest;
        public double vICMSUFRemet;
        public double vBCST;
        public double vST;
        public double vProd;
        public double vFrete;
        public double vSeg;
        public double vDesc;
        public double vII;
        public double vIPI;
        public double vPIS;
        public double vCOFINS;
        public double vOutro;
        public double vNF;
        public double vTotTrib;
        public double vFCP;
        public double vFCPST;
        public double vFCPSTRet;
        public double vIPIDevol;
        public double qBCMono;
        public double vICMSMono;
        public double qBCMonoReten;
        public double vICMSMonoReten;
        public double qBCMonoRet;
        public double vICMSMonoRet;
    }

    internal struct ICMSUFDest
    {
        public double vBCUFDest;
        public double pFCPUFDest;
        public double pICMSUFDest;
        public double pICMSInter;
        public double pICMSInterPart; 
        public double vFCPUFDest;
        public double vICMSUFDest;
        public double vICMSUFRemet;
        public double vBCFCPUFDest;
    }

    /// <summary>
    /// II
    /// </summary>
    internal struct II
    {
        public double vBC;
        public double vDespAdu;
        public double vII;
        public double vIOF;
    }

    /// <summary>
    /// Imposto
    /// </summary>
    internal class Imposto
    {
        public double vTotTrib;
        public IPI IPI;
        public ICMS ICMS;
        public II II;
        public PIS PIS;
        public PISST PISST;
        public COFINS COFINS;
        public COFINSST COFINSST;
        public ISSQN ISSQN;
        public IS IS;
        public IBSCBS IBSCBS;
        public ICMSTot ICMSTot;
        public ISSQNtot ISSQNtot;
        public retTrib retTrib;

        public Imposto()
        {
            ISSQN.cListServ = string.Empty;
        }
    }

    internal class impostoDevol
    {
        public double pDevol;
        public double vIPIDevol;
    }

    /// <summary>
    /// IPI
    /// </summary>
    internal struct IPI
    {
        public string clEnq;
        public string CNPJProd;
        public string cSelo;
        public int qSelo;
        public string cEnq;
        public string CST;
        public double vBC;
        public double qUnid;
        public double vUnid;
        public double pIPI;
        public double vIPI;
    }

    /// <summary>
    /// ISSQN
    /// </summary>
    internal struct ISSQN
    {
        public double vBC;
        public double vAliq;
        public double vISSQN;
        public int cMunFG;
        public string cListServ;
        public string cSitTrib;
        // 3.10
        public double vDeducao;
        public double vOutro;
        public double vDescIncond;
        public double vDescCond;
        public double vISSRet;
        public TpcnindISS indISS;
        public string cServico;
        public int cMun;
        public int cPais;
        public string nProcesso;
        public bool indIncentivo;
    }

    /// <summary>
    /// ISSQNtot
    /// </summary>
    internal struct ISSQNtot
    {
        public double vServ;
        public double vBC;
        public double vISS;
        public double vPIS;
        public double vCOFINS;

        public DateTime dCompet;
        public double vDeducao;
        public double vOutro;
        public double vDescIncond;
        public double vDescCond;
        public double vISSRet;
        public TpcnRegimeTributario cRegTrib;
    }

    /// <summary>
    /// PIS
    /// </summary>
    internal struct PIS
    {
        public string CST;
        public double vBC;
        public double pPIS;
        public double vPIS;
        public double qBCProd;
        public double vAliqProd;
    }

    /// <summary>
    /// PISST
    /// </summary>
    internal struct PISST
    {
        public double vBC;
        public double pPis;
        public double qBCProd;
        public double vAliqProd;
        public double vPIS;
        public string indSomaPISST;
    }

    /// <summary>
    /// retTransp
    /// </summary>
    internal struct retTransp
    {
        public double vServ;
        public double vBCRet;
        public double pICMSRet;
        public double vICMSRet;
        public string CFOP;
        public int cMunFG;
    }

    /// <summary>
    /// retTrib
    /// </summary>
    internal struct retTrib
    {
        public double vRetPIS;
        public double vRetCOFINS;
        public double vRetCSLL;
        public double vBCIRRF;
        public double vIRRF;
        public double vBCRetPrev;
        public double vRetPrev;
    }

    #region Reforma Tributária

    /// <summary>
    /// IS (Imposto Seletivo)
    /// </summary>
    internal struct IS
    {
        public string CSTIS;
        public string cClassTribIS;
        public double vBCIS;
        public double pIS;
        public double pISEspec;
        public string uTrib;
        public double qTrib;
        public double vIS;
    }

    /// <summary>
    /// IBSCBS (Imposto de Bens e Serviços - IBS e da Contribuição de Bens e Serviços - CBS)
    /// </summary>
    internal struct IBSCBS
    {
        public string CST;
        public string cClassTrib;
        public GIBSCBS gIBSCBS;
        public GIBSCBSMono gIBSCBSMono;
        public GTransfCred gTransfCred;
        public GCredPresIBSZFM gCredPresIBSZFM;
        public string indDoacao;
        public GAjusteCompet gAjusteCompet;
        public GEstornoCred gEstornoCred;
        public GCredPresOper gCredPresOper;
    }


    internal struct GCredPresOper
    {
        public double vBCCredPres;
        public double cCredPres;
        public GIBSCredPres gIBSCredPres;
        public GCBSCredPres gCBSCredPres;
    }

    internal struct GIBSCredPres
    {
        public double pCredPres;
        public double vCredPres;
        public double vCredPresCondSus;
    }

    internal struct GCBSCredPres
    {
        public double pCredPres;
        public double vCredPres;
        public double vCredPresCondSus;
    }


    internal class GEstornoCred
    {
        public double vIBSEstCred;
        public double vCBSEstCred;
    }


    internal struct GAjusteCompet
    {
        public string competApur;
        public double vIBS;
        public double vCBS;
    }
    /// <summary>
    /// Grupo de informações do IBS e CBS
    /// </summary>
    internal struct GIBSCBS
    {
        public double vBC;
        public GIBSUF gIBSUF;
        public GIBSMun gIBSMun;
        public double vIBS;
        public GCBS gCBS;
        public GTribRegular gTribRegular;
        public GTribCompraGov gTribCompraGov;
    }

    /// <summary>
    /// Grupo de Informações do IBS para a UF
    /// </summary>
    internal struct GIBSUF
    {
        public double pIBSUF;
        public GDif gDif;
        public GDevTrib gDevTrib;
        public GRed gRed;
        public double vIBSUF;
    }

    /// <summary>
    /// Grupo de Informações do Diferimento
    /// </summary>
    internal struct GDif
    {
        public double pDif;
        public double vDif;
    }

    /// <summary>
    /// Grupo de Informações da devolução de tributos
    /// </summary>
    internal struct GDevTrib
    {
        public double vDevTrib;
    }

    /// <summary>
    /// Grupo de informações da redução da alíquota
    /// </summary>
    internal struct GRed
    {
        public double pRedAliq;
        public double pAliqEfet;
    }

    /// <summary>
    /// Grupo de Informações do IBS para o município
    /// </summary>
    internal struct GIBSMun
    {
        public double pIBSMun;
        public GDif gDif;
        public GDevTrib gDevTrib;
        public GRed gRed;
        public double vIBSMun;
    }

    /// <summary>
    /// Grupo de Informações da CBS
    /// </summary>
    internal struct GCBS
    {
        public double pCBS;
        public GDif gDif;
        public GDevTrib gDevTrib;
        public GRed gRed;
        public double vCBS;
    }

    /// <summary>
    /// Grupo de informações da Tributação Regular
    /// </summary>
    internal struct GTribRegular
    {
        public string CSTReg;
        public string cClassTribReg;
        public double pAliqEfetRegIBSUF;
        public double vTribRegIBSUF;
        public double pAliqEfetRegIBSMun;
        public double vTribRegIBSMun;
        public double pAliqEfetRegCBS;
        public double vTribRegCBS;
    }

    /// <summary>
    /// Grupo de informações da composição do valor do IBS e da CBS em compras governamental
    /// </summary>
    internal struct GTribCompraGov
    {
        public double pAliqIBSUF;
        public double vTribIBSUF;
        public double pAliqIBSMun;
        public double vTribIBSMun;
        public double pAliqCBS;
        public double vTribCBS;
    }

    /// <summary>
    /// Grupo de Informações do IBS e CBS em operações com imposto monofásico
    /// </summary>
    internal struct GIBSCBSMono
    {
        public GMonoPadrao gMonoPadrao;
        public GMonoReten gMonoReten;
        public GMonoRet gMonoRet;
        public GMonoDif gMonoDif;
        public double vTotIBSMonoItem;
        public double vTotCBSMonoItem;
    }

    /// <summary>
    /// Grupo de informações da Tributação Monofásica padrão
    /// </summary>
    internal struct GMonoPadrao
    {
        public double qBCMono;
        public double adRemIBS;
        public double adRemCBS;
        public double vIBSMono;
        public double vCBSMono;
    }

    /// <summary>
    /// Grupo de informações da Tributação Monofásica sujeita a retenção
    /// </summary>
    internal struct GMonoReten
    {
        public double qBCMonoReten;
        public double adRemIBSReten;
        public double vIBSMonoReten;
        public double adRemCBSReten;
        public double vCBSMonoReten;
    }

    /// <summary>
    /// Grupo de informações da Tributação Monofásica retida anteriormente
    /// </summary>
    internal struct GMonoRet
    {
        public double qBCMonoRet;
        public double adRemIBSRet;
        public double vIBSMonoRet;
        public double adRemCBSRet;
        public double vCBSMonoRet;
    }

    /// <summary>
    /// Grupo de informações do diferimento da Tributação Monofásica
    /// </summary>
    internal struct GMonoDif
    {
        public double pDifIBS;
        public double vIBSMonoDif;
        public double pDifCBS;
        public double vCBSMonoDif;
    }

    /// <summary>
    /// Informar essa opção da Choice para o CST 800
    /// </summary>
    internal struct GTransfCred
    {
        public double vIBS;
        public double vCBS;
    }

    /// <summary>
    /// Tipo Informações do crédito presumido de IBS para fornecimentos a partir da ZFM
    /// </summary>
    internal struct GCredPresIBSZFM
    {
        public string competApur;
        public string tpCredPresIBSZFM { get; set; }
        public double vCredPresIBSZFM { get; set; }
    }

    /// <summary>
    /// ISTot
    /// </summary>
    internal struct ISTot
    {
        public double vIS;
    }

    /// <summary>
    /// IBSCBSTot
    /// </summary>
    internal struct IBSCBSTot
    {
        public double vBCIBSCBS;
        public GIBSTot gIBS;
        public GCBSTot gCBS;
        public GMonoTot gMono;
        public GEstornoCred gEstornoCred;
    }

    /// <summary>
    /// GIBSTot
    /// </summary>
    internal struct GIBSTot
    {
        public GIBSUFTot gIBSUF;
        public GIBSMunTot gIBSMun;
        public double vIBS;
        public double vCredPres;
        public double vCredPresCondSus;
    }

    /// <summary>
    /// GIBSUFTot
    /// </summary>
    internal struct GIBSUFTot
    {
        public double vDif;
        public double vDevTrib;
        public double vIBSUF;
    }

    /// <summary>
    /// GIBSMunTot
    /// </summary>
    internal struct GIBSMunTot
    {
        public double vDif;
        public double vDevTrib;
        public double vIBSMun;
    }

    /// <summary>
    /// GCBSTot
    /// </summary>
    internal struct GCBSTot
    {
        public double vCredPres;
        public double vCredPresCondSus;
        public double vDif;
        public double vDevTrib;
        public double vCBS;
    }

    /// <summary>
    /// GMonoTot
    /// </summary>
    internal struct GMonoTot
    {
        public double vIBSMono;
        public double vCBSMono;
        public double vIBSMonoReten;
        public double vCBSMonoReten;
        public double vIBSMonoRet;
        public double vCBSMonoRet;
    }

    #endregion Reforma Tributária

    /// <summary>
    /// Total
    /// </summary>
    internal struct Total
    {
        public ICMSTot ICMSTot;
        public ISSQNtot ISSQNtot;
        public retTrib retTrib;
        public ISTot ISTot;
        public IBSCBSTot IBSCBSTot;
        public double vNFTot;
    }

}
