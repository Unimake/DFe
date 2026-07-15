using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    internal static class NFeTxtLayoutCatalog
    {
        internal static Dictionary<string, string> Criar()
        {
            const string prefix = "§";

                    var layouts = new Dictionary<string, string>();

                    /// "A"
                    layouts.Add("A", prefix + "A|versao|Id|");
                    /// "B"
                    layouts.Add("B_23", prefix + "B|cUF|cNF|NatOp|mod|serie|nNF|dhEmi|dhSaiEnt|tpNF|idDest|cMunFG|TpImp|TpEmis|cDV|TpAmb|FinNFe|indFinal|indPres|ProcEmi|VerProc|dhCont|xJust|");
                    layouts.Add("B_24", prefix + "B|cUF|cNF|NatOp|mod|serie|nNF|dhEmi|dhSaiEnt|tpNF|idDest|cMunFG|TpImp|TpEmis|cDV|TpAmb|FinNFe|indFinal|indPres|indIntermed|ProcEmi|VerProc|dhCont|xJust|");
                    layouts.Add("B_27", prefix + "B|cUF|cNF|NatOp|mod|serie|nNF|dhEmi|dhSaiEnt|tpNF|idDest|cMunFG|cMunFGIBS|TpImp|TpEmis|cDV|TpAmb|FinNFe|tpNFDebito|tpNFCredito|indFinal|indPres|indIntermed|ProcEmi|VerProc|dhCont|xJust|");
                    layouts.Add("B_28", prefix + "B|cUF|cNF|NatOp|mod|serie|nNF|dhEmi|dhSaiEnt|dPrevEntrega|tpNF|idDest|cMunFG|cMunFGIBS|TpImp|TpEmis|cDV|TpAmb|FinNFe|tpNFDebito|tpNFCredito|indFinal|indPres|indIntermed|ProcEmi|VerProc|dhCont|xJust|");
                    layouts.Add("B13", prefix + "B13|refNFe|");
                    layouts.Add("BA02", prefix + "BA02|refNFe|");
                    layouts.Add("BA03", prefix + "BA03|cUF|AAMM|CNPJ|mod|serie|nNF|");
                    layouts.Add("BA10", prefix + "BA10|cUF|AAMM|IE|mod|serie|nNF|refCTe|");
                    layouts.Add("B20A", prefix + "B20a|cUF|AAMM|IE|mod|serie|nNF|");
                    layouts.Add("B20D", prefix + "B20d|CNPJ|");
                    layouts.Add("BA13", prefix + "BA13|CNPJ|");
                    layouts.Add("B20E", prefix + "B20e|CPF|");
                    layouts.Add("BA14", prefix + "BA14|CPF|");
                    layouts.Add("B20I", prefix + "B20i|refCTe|");
                    layouts.Add("BA19", prefix + "BA19|refCTe|");
                    layouts.Add("B20J", prefix + "B20j|mod|nECF|nCOO|");
                    layouts.Add("BA20", prefix + "BA20|mod|nECF|nCOO|");
                    layouts.Add("B31", prefix + "B31|tpEnteGov|pRedutor|tpOperGov|"); // ide->gCompraGov
                    layouts.Add("BB01", prefix + "BB01|refNFe|"); // ide->gPagAntecipado
                    /// "C"
                    layouts.Add("C", prefix + "C|xNome|xFant|IE|IEST|IM|CNAE|CRT|");
                    layouts.Add("C02", prefix + "C02|CNPJ|");
                    layouts.Add("C02A", prefix + "C02a|CPF|");
                    layouts.Add("C05", prefix + "C05|xLgr|nro|xCpl|xBairro|cMun|xMun|UF|CEP|cPais|xPais|fone|");
                    /// "D"
                    layouts.Add("D", prefix + "D|CNPJ|xOrgao|matr|xAgente|fone|UF|nDAR|dEmi|vDAR|repEmi|dPag|");
                    /// "E"
                    layouts.Add("E_400", prefix + "E|xNome|indIEDest|IE|ISUF|IM|email|");
                    layouts.Add("E02", prefix + "E02|CNPJ|");
                    layouts.Add("E03", prefix + "E03|CPF|");
                    layouts.Add("E03A", prefix + "E03a|idEstrangeiro|");
                    layouts.Add("E05", prefix + "E05|xLgr|nro|xCpl|xBairro|cMun|xMun|UF|CEP|cPais|xPais|fone|");
                    /// "F"
                    layouts.Add("F", prefix + "F|xLgr|nro|xCpl|xBairro|cMun|xMun|UF|");
                    layouts.Add("F_16", prefix + "F|CNPJ_CPF|xNome|xLgr|nro|xCpl|xBairro|cMun|xMun|UF|CEP|cPais|xPais|fone|email|IE|");
                    layouts.Add("F02", prefix + "F02|CNPJ|");
                    layouts.Add("F02A", prefix + "F02a|CPF|");
                    /// "G"
                    layouts.Add("G", prefix + "G|xLgr|nro|xCpl|xBairro|cMun|xMun|UF|");
                    layouts.Add("G_16", prefix + "G|CNPJ_CPF|xNome|xLgr|nro|xCpl|xBairro|cMun|xMun|UF|CEP|cPais|xPais|fone|email|IE|");
                    layouts.Add("G02", prefix + "G02|CNPJ|");
                    layouts.Add("G02A", prefix + "G02a|CPF|");
                    layouts.Add("G51", prefix + "G51|CNPJ|");
                    layouts.Add("GA02", prefix + "GA02|CNPJ|");
                    layouts.Add("G52", prefix + "G52|CPF|");
                    layouts.Add("GA03", prefix + "GA03|CPF|");
                    /// "H"
                    layouts.Add("H", prefix + "H|nItem|infAdProd|");
                    /// "I"
                    layouts.Add("I_28", prefix + "I|cProd|cEAN|XProd|NCM|NVE|CEST|indEscala|CNPJFab|cBenef|EXTIPI|CFOP|UCom|QCom|VUnCom|VProd|CEANTrib|UTrib|QTrib|VUnTrib|VFrete|VSeg|VDesc|vOutro|indTot|xPed|nItemPed|nFCI|");
                    layouts.Add("I_30", prefix + "I|cProd|cEAN|cBarra|XProd|NCM|NVE|CEST|indEscala|CNPJFab|cBenef|EXTIPI|CFOP|UCom|QCom|VUnCom|VProd|CEANTrib|cBarraTrib|UTrib|QTrib|VUnTrib|VFrete|VSeg|VDesc|vOutro|indTot|xPed|nItemPed|nFCI|");

                    layouts.Add("I05G", prefix + "I05g|cCredPresumido|pCredPresumido|vCredPresumido|");

                    layouts.Add("I05A", prefix + "I05a|NVE|");
                    layouts.Add("I05K", prefix + "I05k|tpCredPresIBSZFM|"); // prod->tpCredPresIBSZFM (IBSCBS)
                    layouts.Add("I05W", prefix + "I05w|CEST|");
                    layouts.Add("I05W_4", prefix + "I05w|CEST|indEscala|CNPJFab|");
                    layouts.Add("I05C", prefix + "I05c|CEST|");
                    layouts.Add("I05C_4", prefix + "I05c|CEST|indEscala|CNPJFab|");
                    layouts.Add("I17", prefix + "I17|indBemMovelUsado|"); // prod->indBemMovelUsado (IBSCBS)
                    layouts.Add("I18_400_12", prefix + "I18|nDI|dDI|xLocDesemb|UFDesemb|dDesemb|tpViaTransp|vAFRMM|tpIntermedio|CNPJ|UFTerceiro|cExportador|");
                    layouts.Add("I18_400_13", prefix + "I18|nDI|dDI|xLocDesemb|UFDesemb|dDesemb|tpViaTransp|vAFRMM|tpIntermedio|CNPJ|CPF|UFTerceiro|cExportador|");
                    layouts.Add("I25_400", prefix + "I25|NAdicao|NSeqAdic|CFabricante|VDescDI|nDraw|");
                    layouts.Add("I50", prefix + "I50|nDraw|");
                    layouts.Add("I52", prefix + "I52|nRE|chNFe|qExport|");
                    layouts.Add("I80", prefix + "I80|nLote|qLote|dFab|dVal|cAgreg|");
                    layouts.Add("IRT", prefix + "IRT|CNPJ|xContato|email|fone|idCSRT|hashCSRT|");
                    /// "J"
                    layouts.Add("J", prefix + "J|tpOp|Chassi|CCor|XCor|Pot|cilin|pesoL|pesoB|NSerie|TpComb|NMotor|CMT|Dist|anoMod|anoFab|tpPint|tpVeic|espVeic|VIN|condVeic|cMod|cCorDENATRAN|lota|tpRest|");
                    layouts.Add("JA", prefix + "JA|tpOp|Chassi|CCor|XCor|Pot|cilin|pesoL|pesoB|NSerie|TpComb|NMotor|CMT|Dist|anoMod|anoFab|tpPint|tpVeic|espVeic|VIN|condVeic|cMod|cCorDENATRAN|lota|tpRest|");
                    /// "K"
                    layouts.Add("K", prefix + "K|nLote|qLote|dFab|dVal|vPMC|");
                    layouts.Add("K_3", prefix + "K|cProdANVISA|vPMC|");
                    layouts.Add("K_4", prefix + "K|cProdANVISA|xMotivoIsencao|vPMC|");
                    /// "L"
                    layouts.Add("L", prefix + "L|tpArma|nSerie|nCano|descr|");

                    layouts.Add("LA_10", prefix + "LA|cProdANP|descANP|pGLP|pGNn|pGNi|vPart|CODIF|qTemp|UFCons|");
                    layouts.Add("L01_10", prefix + "L01|cProdANP|descANP|pGLP|pGNn|pGNi|vPart|CODIF|qTemp|UFCons|");
                    layouts.Add("LA_11", prefix + "LA|cProdANP|descANP|pGLP|pGNn|pGNi|vPart|CODIF|qTemp|UFCons|pBio|");
                    layouts.Add("L01_11", prefix + "L01|cProdANP|descANP|pGLP|pGNn|pGNi|vPart|CODIF|qTemp|UFCons|pBio|");

                    layouts.Add("LA1", prefix + "LA1|nBico|nBomba|nTanque|vEncIni|vEncFin|");
                    layouts.Add("LA07", prefix + "LA07|qBCProd|vAliqProd|vCIDE|");
                    layouts.Add("LA18", prefix + "LA18|indImport|cUFOrig|pOrig|");

                    layouts.Add("L105", prefix + "L105|qBCProd|vAliqProd|vCIDE|");
                    layouts.Add("LB", prefix + "LB|nRECOPI|");
                    layouts.Add("L109", prefix + "L109|nRECOPI|");
                    /// "M"
                    layouts.Add("M", prefix + "M|vTotTrib|");
                    /// "N"
                    layouts.Add("N02_400", prefix + "N02|Orig|CST|modBC|vBC|pICMS|vICMS|pFCP|vFCP|");

                    layouts.Add("N02A", prefix + "N02A|Orig|CST|qBCMono|adRemICMS|vICMSMono|");

                    layouts.Add("N03_19", prefix + "N03|Orig|CST|modBC|vBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|");
                    layouts.Add("N03_21", prefix + "N03|Orig|CST|modBC|vBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSSTDeson|motDesICMSST|");

                    layouts.Add("N03A", prefix + "N03A|Orig|CST|qBCMono|adRemICMS|vICMSMono|qBCMonoReten|adRemICMSReten|vICMSMonoReten|pRedAdRem|motRedAdRem|");

                    layouts.Add("N04_400_13", prefix + "N04|orig|CST|modBC|pRedBC|vBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|vICMSDeson|motDesICMS|");
                    layouts.Add("N04_400_14", prefix + "N04|orig|CST|modBC|pRedBC|vBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|vICMSDeson|motDesICMS|indDeduzDeson|");
                    layouts.Add("N05_400_14", prefix + "N05|orig|CST|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|");
                    layouts.Add("N05_400_15", prefix + "N05|orig|CST|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|indDeduzDeson|");

                    layouts.Add("N06_400_3", prefix + "N06|orig|CST|");
                    layouts.Add("N06_400_5", prefix + "N06|orig|CST|vICMSDeson|motDesICMS|");
                    layouts.Add("N06_400_6", prefix + "N06|orig|CST|vICMSDeson|motDesICMS|indDeduzDeson|");

                    layouts.Add("N07_14", prefix + "N07|orig|CST|modBC|pRedBC|vBC|pICMS|vICMSOp|pDif|vICMSDif|vICMS|vBCFCP|pFCP|vFCP|");
                    layouts.Add("N07_17", prefix + "N07|orig|CST|modBC|pRedBC|vBC|pICMS|vICMSOp|pDif|vICMSDif|vICMS|vBCFCP|pFCP|vFCP|pFCPDif|vFCPDif|vFCPEfet|");
                    layouts.Add("N07_18", prefix + "N07|orig|CST|modBC|pRedBC|cBenefRBC|vBC|pICMS|vICMSOp|pDif|vICMSDif|vICMS|vBCFCP|pFCP|vFCP|pFCPDif|vFCPDif|vFCPEfet|");
                    layouts.Add("N07A", prefix + "N07A|orig|CST|qBCMono|adRemICMS|vICMSMonoOp|pDif|vICMSMonoDif|vICMSMono|");

                    layouts.Add("N08_400_9", prefix + "N08|Orig|CST|vBCSTRet|pST|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|");
                    layouts.Add("N08_400_10", prefix + "N08|Orig|CST|vBCSTRet|pST|vICMSSubstituto|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|");
                    layouts.Add("N08_400_13", prefix + "N08|Orig|CST|vBCSTRet|pST|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|pRedBCEfet|vBCEfet|pICMSEfet|vICMSEfet|");
                    layouts.Add("N08_400_14", prefix + "N08|Orig|CST|vBCSTRet|pST|vICMSSubstituto|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|pRedBCEfet|vBCEfet|pICMSEfet|vICMSEfet|");
                    layouts.Add("N08A", prefix + "N08A|Orig|CST|qBCMonoRet|adRemICMSRet|vICMSMonoRet|");

                    layouts.Add("N09_22", prefix + "N09|orig|CST|modBC|pRedBC|vBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|");
                    layouts.Add("N09_24", prefix + "N09|orig|CST|modBC|pRedBC|vBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|vICMSSTDeson|motDesICMSST|");
                    layouts.Add("N09_25", prefix + "N09|orig|CST|modBC|pRedBC|vBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|vICMSSTDeson|motDesICMSST|indDeduzDeson|");

                    layouts.Add("N10_22", prefix + "N10|orig|CST|modBC|vBC|pRedBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|");
                    layouts.Add("N10_24", prefix + "N10|orig|CST|modBC|vBC|pRedBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|vICMSSTDeson|motDesICMSST|");
                    layouts.Add("N10_25", prefix + "N10|orig|CST|modBC|vBC|pRedBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|vICMSSTDeson|motDesICMSST|indDeduzDeson|");

                    layouts.Add("N10A_400_16", prefix + "N10a|orig|CST|modBC|vBC|pRedBC|pICMS|vICMS|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|pBCOp|UFST|");
                    layouts.Add("N10A_400_19", prefix + "N10a|orig|CST|modBC|vBC|pRedBC|pICMS|vICMS|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|pBCOp|UFST|");

                    layouts.Add("N10B", prefix + "N10b|orig|CST|vBCSTRet|vICMSSTRet|vBCSTDest|vICMSSTDest|");
                    layouts.Add("N10B_16", prefix + "N10b|orig|CST|vBCSTRet|pST|vICMSSubstituto|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|vBCSTDest|vICMSSTDest|pRedBCEfet|vBCEfet|pICMSEfet|vICMSEfet|");
                    layouts.Add("N10C", prefix + "N10c|orig|CSOSN|pCredSN|vCredICMSSN|");
                    layouts.Add("N10D", prefix + "N10d|orig|CSOSN|");
                    layouts.Add("N10E_400", prefix + "N10e|orig|CSOSN|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|pCredSN|vCredICMSSN|");
                    layouts.Add("N10F_400", prefix + "N10f|orig|CSOSN|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|");
                    layouts.Add("N10G_400_5", prefix + "N10g|orig|CSOSN|vBCSTRet|vICMSSTRet|");
                    layouts.Add("N10G_400_9", prefix + "N10g|orig|CSOSN|vBCSTRet|pST|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|");
                    layouts.Add("N10G_400_10", prefix + "N10g|orig|CSOSN|vBCSTRet|pST|vICMSSubstituto|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|");
                    layouts.Add("N10G_400_13", prefix + "N10g|orig|CSOSN|vBCSTRet|pST|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|pRedBCEfet|vBCEfet|pICMSEfet|vICMSEfet|");
                    layouts.Add("N10G_400_14", prefix + "N10g|orig|CSOSN|vBCSTRet|pST|vICMSSubstituto|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|pRedBCEfet|vBCEfet|pICMSEfet|vICMSEfet|");
                    layouts.Add("N10H_400", prefix + "N10h|orig|CSOSN|modBC|vBC|pRedBC|pICMS|vICMS|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|pCredSN|vCredICMSSN|");
                    layouts.Add("NA_400", prefix + "NA|vBCUFDest|vBCFCPUFDest|pFCPUFDest|pICMSUFDest|pICMSInter|pICMSInterPart|vFCPUFDest|vICMSUFDest|vICMSUFRemet|");
                    /// "O"
                    layouts.Add("O_400", prefix + "O|CNPJProd|cSelo|qSelo|cEnq|");
                    layouts.Add("O07", prefix + "O07|CST|vIPI|");

                    layouts.Add("O08", prefix + "O08|CST|");
                    layouts.Add("O10", prefix + "O10|vBC|pIPI|");
                    layouts.Add("O11_400", prefix + "O11|qUnid|vUnid|vIPI|");
                    /// "P":
                    layouts.Add("P", prefix + "P|vBC|vDespAdu|vII|vIOF|");
                    /// "Q":
                    layouts.Add("Q02", prefix + "Q02|CST|VBC|PPIS|VPIS|");
                    layouts.Add("Q03", prefix + "Q03|CST|QBCProd|VAliqProd|VPIS|");
                    layouts.Add("Q04", prefix + "Q04|CST|");
                    layouts.Add("Q05", prefix + "Q05|CST|vPIS|");
                    layouts.Add("Q07_400", prefix + "Q07|vBC|pPIS|vPIS|");
                    layouts.Add("Q10", prefix + "Q10|qBCProd|vAliqProd|");
                    /// "R":
                    layouts.Add("R", prefix + "R|vPIS|"); //ok
                    layouts.Add("R02", prefix + "R02|vBC|pPIS|");

                    layouts.Add("R04_4", prefix + "R04|qBCProd|vAliqProd|vPIS|");
                    layouts.Add("R04_5", prefix + "R04|qBCProd|vAliqProd|vPIS|indSomaPISST|");

                    /// "S"
                    layouts.Add("S02", prefix + "S02|CST|vBC|pCOFINS|vCOFINS|");
                    layouts.Add("S03", prefix + "S03|CST|QBCProd|VAliqProd|VCOFINS|");
                    layouts.Add("S04", prefix + "S04|CST|");
                    layouts.Add("S05", prefix + "S05|CST|VCOFINS|");
                    layouts.Add("S07", prefix + "S07|VBC|PCOFINS|");
                    layouts.Add("S09", prefix + "S09|QBCProd|VAliqProd|");
                    /// "T":
                    layouts.Add("T", prefix + "T|VCOFINS|"); //ok
                    layouts.Add("T02", prefix + "T02|VBC|PCOFINS|");

                    layouts.Add("T04_4", prefix + "T04|QBCProd|VAliqProd|vCOFINS|");
                    layouts.Add("T04_5", prefix + "T04|QBCProd|VAliqProd|vCOFINS|indSomaCOFINSST|");

                    /// "U":
                    layouts.Add("U_400", prefix + "U|VBC|VAliq|VISSQN|CMunFG|CListServ|vDeducao|vOutro|vDescIncond|vDescCond|vISSRet|indISS|cServico|cMun|cPais|nProcesso|indIncentivo|");
                    layouts.Add("UA", prefix + "UA|pDevol|vIPIDevol|");
                    layouts.Add("UB01", prefix + "UB01|CSTIS|cClassTribIS|vBCIS|pIS|pISEspec|uTrib|qTrib|vIS|"); //IS
                    layouts.Add("UB12", prefix + "UB12|CST|cClassTrib|"); //IBSCBS
                    layouts.Add("UB15", prefix + "UB15|vBC|vIBS|"); //IBSCBS->gIBSCBS

                    layouts.Add("UB17", prefix + "UB17|pIBSUF|pDif|vDif|vDevTrib|pRedAliq|pAliqEfet|vIBSUF|"); //IBSCBS->gIBSCBS->gIBSUF
                    layouts.Add("UB36", prefix + "UB36|pIBSMun|pDif|vDif|vDevTrib|pRedAliq|pAliqEfet|vIBSMun|"); //IBSCBS->gIBSCBS->gIBSMun
                    layouts.Add("UB55", prefix + "UB55|pCBS|pDif|vDif|vDevTrib|pRedAliq|pAliqEfet|vCBS|"); //IBSCBS->gIBSCBS->gCBS

                    layouts.Add("UB68", prefix + "UB68|CSTReg|cClassTribReg|pAliqEfetRegIBSUF|vTribRegIBSUF|pAliqEfetRegIBSMun|vTribRegIBSMun|pAliqEfetRegCBS|vTribRegCBS|"); //IBSCBS->gIBSCBS->gTribRegular

                    layouts.Add("UB82", prefix + "UB82|pAliqIBSUF|vTribIBSUF|pAliqIBSMun|vTribIBSMun|pAliqCBS|vTribCBS|"); //IBSCBS->gIBSCBS->gTribCompraGov

                    layouts.Add("UB84", prefix + "UB84|vTotIBSMonoItem|vTotCBSMonoItem|"); //IBSCBS->gIBSCBSMono

                    layouts.Add("UB85", prefix + "UB85|qBCMono|adRemIBS|adRemCBS|vIBSMono|vCBSMono|"); //IBSCBS->gIBSCBSMono->gMonoPadrao
                    layouts.Add("UB91", prefix + "UB91|qBCMonoReten|adRemIBSReten|vIBSMonoReten|adRemCBSReten|vCBSMonoReten|"); //IBSCBS->gIBSCBSMono->gMonoReten
                    layouts.Add("UB95", prefix + "UB95|qBCMonoRet|adRemIBSRet|vIBSMonoRet|adRemCBSRet|vCBSMonoRet|"); //IBSCBS->gIBSCBSMono->gMonoRet
                    layouts.Add("UB100", prefix + "UB100|pDifIBS|vIBSMonoDif|pDifCBS|vCBSMonoDif|"); //IBSCBS->gIBSCBSMono->gMonoDif

                    layouts.Add("UB106", prefix + "UB106|vIBS|vCBS|"); //IBSCBS->gTransfCred

                    layouts.Add("UB14A", prefix + "UB14a|indDoacao|");//IBSCBS->indDoacao

                    layouts.Add("UB112", prefix + "UB112|competApur|vIBS|vCBS|");

                    layouts.Add("UB116", prefix + "UB116|vIBSEstCred|vCBSEstCred|");

                    layouts.Add("UB120", prefix + "UB120|vBCCredPres|cCredPres|");

                    layouts.Add("UB123", prefix + "UB123|pCredPres|vCredPres|vCredPresCondSus|");

                    layouts.Add("UB127", prefix + "UB127|pCredPres|vCredPres|vCredPresCondSus|");

                    layouts.Add("UB131", prefix + "UB131|competApur|tpCredPresIBSZFM|vCredPresIBSZFM|");


                    /// "V":
                    layouts.Add("VA02", prefix + "VA02|XCampo|XTexto|");
                    layouts.Add("VA05", prefix + "VA05|XCampo|XTexto|");
                    layouts.Add("VB01", prefix + "VB01|vItem|");
                    layouts.Add("VC01", prefix + "VC01|chaveAcesso|nItem|");

                    /// "W"
                    layouts.Add("W02_400_17", prefix + "W02|vBC|vICMS|vICMSDeson|vBCST|vST|vProd|vFrete|vSeg|vDesc|vII|vIPI|vPIS|vCOFINS|vOutro|vNF|vTotTrib|");
                    layouts.Add("W02_400_20", prefix + "W02|vBC|vICMS|vICMSDeson|vFCPUFDest|vICMSUFDest|vICMSUFRemet|vBCST|vST|vProd|vFrete|vSeg|vDesc|vII|vIPI|vPIS|vCOFINS|vOutro|vNF|vTotTrib|");
                    layouts.Add("W02_400_21", prefix + "W02|vBC|vICMS|vICMSDeson|vFCPUFDest|vICMSUFDest|vICMSUFRemet||vBCST|vST|vProd|vFrete|vSeg|vDesc|vII|vIPI|vPIS|vCOFINS|vOutro|vNF|vTotTrib|");
                    layouts.Add("W02_400_24", prefix + "W02|vBC|vICMS|vICMSDeson|vFCP|vFCPUFDest|vICMSUFDest|vICMSUFRemet|vBCST|vST|vFCPST|vFCPSTRet|vProd|vFrete|vSeg|vDesc|vII|vIPI|vIPIDevol|vPIS|vCOFINS|vOutro|vNF|vTotTrib|");
                    layouts.Add("W02_400_30", prefix + "W02|vBC|vICMS|vICMSDeson|vFCP|vFCPUFDest|vICMSUFDest|vICMSUFRemet|vBCST|vST|vFCPST|vFCPSTRet|qBCMono|vICMSMono|qBCMonoReten|vICMSMonoReten|qBCMonoRet|vICMSMonoRet|vProd|vFrete|vSeg|vDesc|vII|vIPI|vIPIDevol|vPIS|vCOFINS|vOutro|vNF|vTotTrib|");
                    ///
                    /// criada duas entradas porque acho que a Sefaz cometeu um erro, colocando um pipe em branco
                    layouts.Add("W04", prefix + "W04|vICMSUFDest|vICMSUFRemet|vFCPUFDest|");
                    layouts.Add("W17_400", prefix + "W17|VServ|VBC|VISS|VPIS|VCOFINS|dCompet|vDeducao|vOutro|vDescIncond|vDescCond|vISSRet|cRegTrib|");
                    layouts.Add("W23", prefix + "W23|VRetPIS|VRetCOFINS|VRetCSLL|VBCIRRF|VIRRF|VBCRetPrev|VRetPrev|");

                    layouts.Add("W31", prefix + "W31|vIS|"); //ISTot -> vIS
                    layouts.Add("W34", prefix + "W34|vBCIBSCBS|"); //IBSCBSTot -> vBCIBSCBS
                    layouts.Add("W36", prefix + "W36|vIBS|vCredPres|vCredPresCondSus|"); //IBSCBSTot -> gIBS
                    layouts.Add("W37", prefix + "W37|vDif|vDevTrib|vIBSUF|"); //IBSCBSTot -> gIBS -> gIBSUF
                    layouts.Add("W42", prefix + "W42|vDif|vDevTrib|vIBSMun|"); //IBSCBSTot -> gIBS -> gIBSMun
                    layouts.Add("W50", prefix + "W50|vDif|vDevTrib|vCBS|vCredPres|vCredPresCondSus|"); //IBSCBSTot -> gCBS
                    layouts.Add("W57", prefix + "W57|vIBSMono|vCBSMono|vIBSMonoReten|vCBSMonoReten|vIBSMonoRet|vCBSMonoRet|"); //IBSCBSTot -> gMono
                    layouts.Add("W59E", prefix + "W59e|vIBSEstCred|vCBSEstCred|");
                    layouts.Add("W60", prefix + "W60|vNFTot|"); //Total -> vNFTot

                    /// "X":
                    layouts.Add("X", prefix + "X|modFrete|");
                    layouts.Add("X03", prefix + "X03|xNome|IE|xEnder|xMun|UF|");
                    layouts.Add("X04", prefix + "X04|CNPJ|");
                    layouts.Add("X05", prefix + "X05|CPF|");
                    layouts.Add("X11", prefix + "X11|VServ|VBCRet|PICMSRet|VICMSRet|CFOP|CMunFG|");
                    layouts.Add("X18", prefix + "X18|Placa|UF|RNTC|");
                    layouts.Add("X22_400", prefix + "X22|Placa|UF|RNTC|vagao|balsa|");
                    layouts.Add("X26", prefix + "X26|QVol|Esp|Marca|NVol|PesoL|PesoB|");
                    layouts.Add("X33", prefix + "X33|NLacre|");
                    /// "Y":
                    layouts.Add("Y02", prefix + "Y02|NFat|VOrig|VDesc|VLiq|");
                    layouts.Add("Y07", prefix + "Y07|NDup|DVenc|VDup|");

                    layouts.Add("YA_9", prefix + "YA|indPag|tPag|xPag|vPag|CNPJ|tBand|cAut|tpIntegra|");
                    layouts.Add("YA_14", prefix + "YA|indPag|tPag|xPag|vPag|dPag|CNPJPag|UFPag|CNPJ|tBand|cAut|tpIntegra|CNPJReceb|idTermPag|");

                    layouts.Add("YA04", prefix + "YA04|tpIntegra|");
                    layouts.Add("YA04A", prefix + "YA04a|tpIntegra|");
                    layouts.Add("YA09", prefix + "YA09|vTroco|");

                    layouts.Add("YB", prefix + "YB|CNPJ|idCadIntTran|");

                    /// "Z":
                    layouts.Add("Z", prefix + "Z|InfAdFisco|InfCpl|");
                    layouts.Add("Z04", prefix + "Z04|XCampo|XTexto|");
                    layouts.Add("Z07", prefix + "Z07|XCampo|XTexto|");
                    layouts.Add("Z10", prefix + "Z10|NProc|IndProc|");
                    layouts.Add("Z10_4", prefix + "Z10|NProc|IndProc|tpAto|");
                    layouts.Add("ZA_400", prefix + "ZA|UFSaidaPais|xLocExporta|xLocDespacho|");
                    layouts.Add("ZA01_400", prefix + "ZA01|UFSaidaPais|xLocExporta|xLocDespacho|");
                    layouts.Add("ZB", prefix + "ZB|XNEmp|XPed|XCont|");
                    layouts.Add("ZC", prefix + "ZC|safra|ref|qTotMes|qTotAnt|qTotGer|vFor|vTotDed|vLiqFor|");
                    layouts.Add("ZC01", prefix + "ZC01|safra|ref|qTotMes|qTotAnt|qTotGer|vFor|vTotDed|vLiqFor|");
                    layouts.Add("ZC04", prefix + "ZC04|dia|qtde|");
                    layouts.Add("ZC10", prefix + "ZC10|xDed|vDed|");
                    layouts.Add("ZD", prefix + "ZD|CNPJ|xContato|email|fone|idCSRT|hashCSRT|");

                    layouts.Add("ZF02", prefix + "ZF02|nReceituario|CPFRespTec|"); // agropecuario->defensivo
                    layouts.Add("ZF04", prefix + "ZF04|tpGuia|UFGuia|serieGuia|nGuia|"); // agropecuario->guiaTransito
                
            return layouts;
        }
    }
}
