# Layout TXT da NF-e/NFC-e

Fonte: catálogo atual `NFeTxtLayoutCatalog` da DLL `Unimake.DFe`. Cada linha mostra o segmento seguido dos campos na ordem aceita pelo conversor; o marcador interno `§` foi omitido. Nas variantes, o identificador após `#` corresponde à chave usada pelo resolvedor do layout.

## A

```text
A|versao|Id|
```

## B

```text
# B_23
B|cUF|cNF|NatOp|mod|serie|nNF|dhEmi|dhSaiEnt|tpNF|idDest|cMunFG|TpImp|TpEmis|cDV|TpAmb|FinNFe|indFinal|indPres|ProcEmi|VerProc|dhCont|xJust|
# B_24
B|cUF|cNF|NatOp|mod|serie|nNF|dhEmi|dhSaiEnt|tpNF|idDest|cMunFG|TpImp|TpEmis|cDV|TpAmb|FinNFe|indFinal|indPres|indIntermed|ProcEmi|VerProc|dhCont|xJust|
# B_26
B|cUF|cNF|NatOp|mod|serie|nNF|dhEmi|dhSaiEnt|tpNF|idDest|cMunFG|TpImp|TpEmis|cDV|TpAmb|FinNFe|tpNFDebito|tpNFCredito|indFinal|indPres|indIntermed|ProcEmi|VerProc|dhCont|xJust|
# B_27
B|cUF|cNF|NatOp|mod|serie|nNF|dhEmi|dhSaiEnt|tpNF|idDest|cMunFG|cMunFGIBS|TpImp|TpEmis|cDV|TpAmb|FinNFe|tpNFDebito|tpNFCredito|indFinal|indPres|indIntermed|ProcEmi|VerProc|dhCont|xJust|
# B_28
B|cUF|cNF|NatOp|mod|serie|nNF|dhEmi|dhSaiEnt|dPrevEntrega|tpNF|idDest|cMunFG|cMunFGIBS|TpImp|TpEmis|cDV|TpAmb|FinNFe|tpNFDebito|tpNFCredito|indFinal|indPres|indIntermed|ProcEmi|VerProc|dhCont|xJust|
B13|refNFe|
BA02|refNFe|
BA03|cUF|AAMM|CNPJ|mod|serie|nNF|
BA10|cUF|AAMM|IE|mod|serie|nNF|refCTe|
B20a|cUF|AAMM|IE|mod|serie|nNF|
B20d|CNPJ|
BA13|CNPJ|
B20e|CPF|
BA14|CPF|
B20i|refCTe|
BA19|refCTe|
B20j|mod|nECF|nCOO|
BA20|mod|nECF|nCOO|
BB01|tpEnteGov|pRedutor|tpOperGov|refDFeAnt|
BB05|refDFeAnt|
BC01|refNFe|
```

## C

```text
C|xNome|xFant|IE|IEST|IM|CNAE|CRT|ISUFEmit|
C02|CNPJ|
C02a|CPF|
C05|xLgr|nro|xCpl|xBairro|cMun|xMun|UF|CEP|cPais|xPais|fone|
```

## D

```text
D|CNPJ|xOrgao|matr|xAgente|fone|UF|nDAR|dEmi|vDAR|repEmi|dPag|
```

## E

```text
# E_400
E|xNome|indIEDest|IE|ISUF|IM|email|
E02|CNPJ|
E03|CPF|
E03a|idEstrangeiro|
E05|xLgr|nro|xCpl|xBairro|cMun|xMun|UF|CEP|cPais|xPais|fone|
```

## F

```text
F|xLgr|nro|xCpl|xBairro|cMun|xMun|UF|
# F_16
F|CNPJ_CPF|xNome|xLgr|nro|xCpl|xBairro|cMun|xMun|UF|CEP|cPais|xPais|fone|email|IE|
F02|CNPJ|
F02a|CPF|
```

## G

```text
G|xLgr|nro|xCpl|xBairro|cMun|xMun|UF|
# G_16
G|CNPJ_CPF|xNome|xLgr|nro|xCpl|xBairro|cMun|xMun|UF|CEP|cPais|xPais|fone|email|IE|
G02|CNPJ|
G02a|CPF|
G51|CNPJ|
GA02|CNPJ|
G52|CPF|
GA03|CPF|
```

## H

```text
H|nItem|infAdProd|
```

## I

```text
# I_28
I|cProd|cEAN|XProd|NCM|NVE|CEST|indEscala|CNPJFab|cBenef|EXTIPI|CFOP|UCom|QCom|VUnCom|VProd|CEANTrib|UTrib|QTrib|VUnTrib|VFrete|VSeg|VDesc|vOutro|indTot|xPed|nItemPed|nFCI|
# I_30
I|cProd|cEAN|cBarra|XProd|NCM|NVE|CEST|indEscala|CNPJFab|cBenef|EXTIPI|CFOP|UCom|QCom|VUnCom|VProd|CEANTrib|cBarraTrib|UTrib|QTrib|VUnTrib|VFrete|VSeg|VDesc|vOutro|indTot|xPed|nItemPed|nFCI|
I05g|cCredPresumido|pCredPresumido|vCredPresumido|
I05a|NVE|
I05k|tpCredPresIBSZFM|
I05w|CEST|
# I05W_4
I05w|CEST|indEscala|CNPJFab|
I05c|CEST|
# I05C_4
I05c|CEST|indEscala|CNPJFab|
I17|indBemMovelUsado|
# I18_400_12
I18|nDI|dDI|xLocDesemb|UFDesemb|dDesemb|tpViaTransp|vAFRMM|tpIntermedio|CNPJ|UFTerceiro|cExportador|
# I18_400_13
I18|nDI|dDI|xLocDesemb|UFDesemb|dDesemb|tpViaTransp|vAFRMM|tpIntermedio|CNPJ|CPF|UFTerceiro|cExportador|
# I25_400
I25|NAdicao|NSeqAdic|CFabricante|VDescDI|nDraw|
I50|nDraw|
I52|nRE|chNFe|qExport|
I80|nLote|qLote|dFab|dVal|cAgreg|
IRT|CNPJ|xContato|email|fone|idCSRT|hashCSRT|
```

## J

```text
J|tpOp|Chassi|CCor|XCor|Pot|cilin|pesoL|pesoB|NSerie|TpComb|NMotor|CMT|Dist|anoMod|anoFab|tpPint|tpVeic|espVeic|VIN|condVeic|cMod|cCorDENATRAN|lota|tpRest|
JA|tpOp|Chassi|CCor|XCor|Pot|cilin|pesoL|pesoB|NSerie|TpComb|NMotor|CMT|Dist|anoMod|anoFab|tpPint|tpVeic|espVeic|VIN|condVeic|cMod|cCorDENATRAN|lota|tpRest|
```

## K

```text
K|nLote|qLote|dFab|dVal|vPMC|
# K_3
K|cProdANVISA|vPMC|
# K_4
K|cProdANVISA|xMotivoIsencao|vPMC|
```

## L

```text
L|tpArma|nSerie|nCano|descr|
# LA_10
LA|cProdANP|descANP|pGLP|pGNn|pGNi|vPart|CODIF|qTemp|UFCons|
# L01_10
L01|cProdANP|descANP|pGLP|pGNn|pGNi|vPart|CODIF|qTemp|UFCons|
# LA_11
LA|cProdANP|descANP|pGLP|pGNn|pGNi|vPart|CODIF|qTemp|UFCons|pBio|
# L01_11
L01|cProdANP|descANP|pGLP|pGNn|pGNi|vPart|CODIF|qTemp|UFCons|pBio|
LA1|nBico|nBomba|nTanque|vEncIni|vEncFin|
LA07|qBCProd|vAliqProd|vCIDE|
LA18|indImport|cUFOrig|pOrig|
L105|qBCProd|vAliqProd|vCIDE|
LB|nRECOPI|
L109|nRECOPI|
```

## M

```text
M|vTotTrib|
```

## N

```text
# N02_400
N02|Orig|CST|modBC|vBC|pICMS|vICMS|pFCP|vFCP|
N02A|Orig|CST|qBCMono|adRemICMS|vICMSMono|
# N03_19
N03|Orig|CST|modBC|vBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|
# N03_21
N03|Orig|CST|modBC|vBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSSTDeson|motDesICMSST|
N03A|Orig|CST|qBCMono|adRemICMS|vICMSMono|qBCMonoReten|adRemICMSReten|vICMSMonoReten|pRedAdRem|motRedAdRem|
# N04_400_13
N04|orig|CST|modBC|pRedBC|vBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|vICMSDeson|motDesICMS|
# N04_400_14
N04|orig|CST|modBC|pRedBC|vBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|vICMSDeson|motDesICMS|indDeduzDeson|
# N05_400_14
N05|orig|CST|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|
# N05_400_15
N05|orig|CST|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|indDeduzDeson|
# N06_400_3
N06|orig|CST|
# N06_400_5
N06|orig|CST|vICMSDeson|motDesICMS|
# N06_400_6
N06|orig|CST|vICMSDeson|motDesICMS|indDeduzDeson|
# N07_14
N07|orig|CST|modBC|pRedBC|vBC|pICMS|vICMSOp|pDif|vICMSDif|vICMS|vBCFCP|pFCP|vFCP|
# N07_17
N07|orig|CST|modBC|pRedBC|vBC|pICMS|vICMSOp|pDif|vICMSDif|vICMS|vBCFCP|pFCP|vFCP|pFCPDif|vFCPDif|vFCPEfet|
# N07_18
N07|orig|CST|modBC|pRedBC|cBenefRBC|vBC|pICMS|vICMSOp|pDif|vICMSDif|vICMS|vBCFCP|pFCP|vFCP|pFCPDif|vFCPDif|vFCPEfet|
N07A|orig|CST|qBCMono|adRemICMS|vICMSMonoOp|pDif|vICMSMonoDif|vICMSMono|
# N08_400_9
N08|Orig|CST|vBCSTRet|pST|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|
# N08_400_10
N08|Orig|CST|vBCSTRet|pST|vICMSSubstituto|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|
# N08_400_13
N08|Orig|CST|vBCSTRet|pST|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|pRedBCEfet|vBCEfet|pICMSEfet|vICMSEfet|
# N08_400_14
N08|Orig|CST|vBCSTRet|pST|vICMSSubstituto|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|pRedBCEfet|vBCEfet|pICMSEfet|vICMSEfet|
N08A|Orig|CST|qBCMonoRet|adRemICMSRet|vICMSMonoRet|
# N09_22
N09|orig|CST|modBC|pRedBC|vBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|
# N09_24
N09|orig|CST|modBC|pRedBC|vBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|vICMSSTDeson|motDesICMSST|
# N09_25
N09|orig|CST|modBC|pRedBC|vBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|vICMSSTDeson|motDesICMSST|indDeduzDeson|
# N10_22
N10|orig|CST|modBC|vBC|pRedBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|
# N10_24
N10|orig|CST|modBC|vBC|pRedBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|vICMSSTDeson|motDesICMSST|
# N10_25
N10|orig|CST|modBC|vBC|pRedBC|pICMS|vICMS|vBCFCP|pFCP|vFCP|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|vICMSDeson|motDesICMS|vICMSSTDeson|motDesICMSST|indDeduzDeson|
# N10A_400_16
N10a|orig|CST|modBC|vBC|pRedBC|pICMS|vICMS|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|pBCOp|UFST|
# N10A_400_19
N10a|orig|CST|modBC|vBC|pRedBC|pICMS|vICMS|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|pBCOp|UFST|
N10b|orig|CST|vBCSTRet|vICMSSTRet|vBCSTDest|vICMSSTDest|
# N10B_16
N10b|orig|CST|vBCSTRet|pST|vICMSSubstituto|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|vBCSTDest|vICMSSTDest|pRedBCEfet|vBCEfet|pICMSEfet|vICMSEfet|
N10c|orig|CSOSN|pCredSN|vCredICMSSN|
N10d|orig|CSOSN|
# N10E_400
N10e|orig|CSOSN|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|pCredSN|vCredICMSSN|
# N10F_400
N10f|orig|CSOSN|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|
# N10G_400_5
N10g|orig|CSOSN|vBCSTRet|vICMSSTRet|
# N10G_400_9
N10g|orig|CSOSN|vBCSTRet|pST|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|
# N10G_400_10
N10g|orig|CSOSN|vBCSTRet|pST|vICMSSubstituto|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|
# N10G_400_13
N10g|orig|CSOSN|vBCSTRet|pST|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|pRedBCEfet|vBCEfet|pICMSEfet|vICMSEfet|
# N10G_400_14
N10g|orig|CSOSN|vBCSTRet|pST|vICMSSubstituto|vICMSSTRet|vBCFCPSTRet|pFCPSTRet|vFCPSTRet|pRedBCEfet|vBCEfet|pICMSEfet|vICMSEfet|
# N10H_400
N10h|orig|CSOSN|modBC|vBC|pRedBC|pICMS|vICMS|modBCST|pMVAST|pRedBCST|vBCST|pICMSST|vICMSST|vBCFCPST|pFCPST|vFCPST|pCredSN|vCredICMSSN|
# NA_400
NA|vBCUFDest|vBCFCPUFDest|pFCPUFDest|pICMSUFDest|pICMSInter|pICMSInterPart|vFCPUFDest|vICMSUFDest|vICMSUFRemet|
```

## O

```text
# O_400
O|CNPJProd|cSelo|qSelo|cEnq|
O07|CST|vIPI|
O08|CST|
O10|vBC|pIPI|
# O11_400
O11|qUnid|vUnid|vIPI|
```

## P

```text
P|vBC|vDespAdu|vII|vIOF|
```

## Q

```text
Q02|CST|VBC|PPIS|VPIS|
Q03|CST|QBCProd|VAliqProd|VPIS|
Q04|CST|
Q05|CST|vPIS|
# Q07_400
Q07|vBC|pPIS|vPIS|
Q10|qBCProd|vAliqProd|
```

## R

```text
R|vPIS|
R02|vBC|pPIS|
# R04_4
R04|qBCProd|vAliqProd|vPIS|
# R04_5
R04|qBCProd|vAliqProd|vPIS|indSomaPISST|
```

## S

```text
S02|CST|vBC|pCOFINS|vCOFINS|
S03|CST|QBCProd|VAliqProd|VCOFINS|
S04|CST|
S05|CST|VCOFINS|
S07|VBC|PCOFINS|
S09|QBCProd|VAliqProd|
```

## T

```text
T|VCOFINS|
T02|VBC|PCOFINS|
# T04_4
T04|QBCProd|VAliqProd|vCOFINS|
# T04_5
T04|QBCProd|VAliqProd|vCOFINS|indSomaCOFINSST|
```

## U

```text
# U_400
U|VBC|VAliq|VISSQN|CMunFG|CListServ|vDeducao|vOutro|vDescIncond|vDescCond|vISSRet|indISS|cServico|cMun|cPais|nProcesso|indIncentivo|
UA|pDevol|vIPIDevol|
UB01|CSTIS|cClassTribIS|vBCIS|pIS|adRemIS|uTrib|qTrib|vIS|
UB12|CST|cClassTrib|
UB15|vBC|vIBS|
UB17|pIBSUF|pDif|vDif|pDevTrib|vDevTrib|pRedAliq|pAliqEfet|vIBSUF|
# UB17_8
UB17|pIBSUF|pDif|vDif|vDevTrib|pRedAliq|pAliqEfet|vIBSUF|
UB36|pIBSMun|pDif|vDif|pDevTrib|vDevTrib|pRedAliq|pAliqEfet|vIBSMun|
# UB36_8
UB36|pIBSMun|pDif|vDif|vDevTrib|pRedAliq|pAliqEfet|vIBSMun|
UB55|pCBS|pDif|vDif|pDevTrib|vDevTrib|pRedAliq|pAliqEfet|vCBS|
# UB55_8
UB55|pCBS|pDif|vDif|vDevTrib|pRedAliq|pAliqEfet|vCBS|
UB66a|tpALCZFMCBS|nProcSuframa|pAliqEfetRegCBS|vTribRegCBS|
UB68|CSTReg|cClassTribReg|pAliqEfetRegIBSUF|vTribRegIBSUF|pAliqEfetRegIBSMun|vTribRegIBSMun|pAliqEfetRegCBS|vTribRegCBS|
UB82|pAliqIBSUF|vTribIBSUF|pAliqIBSMun|vTribIBSMun|pAliqCBS|vTribCBS|
UB84|vTotIBSMonoItem|vTotCBSMonoItem|
UB85|qBCMono|adRemIBS|adRemCBS|vIBSMono|vCBSMono|
UB91|qBCMonoReten|adRemIBSReten|vIBSMonoReten|adRemCBSReten|vCBSMonoReten|
UB95|qBCMonoRet|adRemIBSRet|vIBSMonoRet|adRemCBSRet|vCBSMonoRet|
UB100|pDifIBS|vIBSMonoDif|pDifCBS|vCBSMonoDif|
UB85IAR|qBCMono|adRemIBS|vIBSMono|
UB91IAR|qBCMonoReten|adRemIBSReten|vIBSMonoReten|
UB95IAR|vIBSMonoRet|
UB100IAR|qBCBioComb|vIBSDiferenca|
UB85IAV|vBCMono|pAliqMonoUF|vIBSMonoUF|pAliqMonoMun|vIBSMonoMun|vIBSMono|
UB91IAV|vBCMonoReten|pAliqMonoReten|vIBSMonoReten|
UB95IAV|vIBSMonoRet|
UB100IAV|qBCBioComb|vIBSDiferenca|
UB85CAR|qBCMono|adRemCBS|vCBSMono|
UB91CAR|qBCMonoReten|adRemCBSReten|vCBSMonoReten|
UB95CAR|vCBSMonoRet|
UB100CAR|qBCBioComb|vCBSDiferenca|
UB85CAV|vBCMono|pAliqMonoCBS|vCBSMono|
UB91CAV|vBCMonoReten|pAliqMonoReten|vCBSMonoReten|
UB95CAV|vCBSMonoRet|
UB100CAV|qBCBioComb|vCBSDiferenca|
UB106|vIBS|vCBS|
UB14a|indDoacao|
UB112|competApur|vIBS|vCBS|
UB116|vIBSEstCred|vCBSEstCred|
UB120|vBCCredPres|cCredPres|
UB123|pCredPres|vCredPres|vCredPresCondSus|
UB127|pCredPres|vCredPres|vCredPresCondSus|
UB131|competApur|tpCredPresIBSZFM|vCredPresIBSZFM|
```

## V

```text
VA02|XCampo|XTexto|
VA05|XCampo|XTexto|
VB01|vItem|
VC01|chaveAcesso|nItem|
```

## W

```text
# W02_400_17
W02|vBC|vICMS|vICMSDeson|vBCST|vST|vProd|vFrete|vSeg|vDesc|vII|vIPI|vPIS|vCOFINS|vOutro|vNF|vTotTrib|
# W02_400_20
W02|vBC|vICMS|vICMSDeson|vFCPUFDest|vICMSUFDest|vICMSUFRemet|vBCST|vST|vProd|vFrete|vSeg|vDesc|vII|vIPI|vPIS|vCOFINS|vOutro|vNF|vTotTrib|
# W02_400_21
W02|vBC|vICMS|vICMSDeson|vFCPUFDest|vICMSUFDest|vICMSUFRemet||vBCST|vST|vProd|vFrete|vSeg|vDesc|vII|vIPI|vPIS|vCOFINS|vOutro|vNF|vTotTrib|
# W02_400_24
W02|vBC|vICMS|vICMSDeson|vFCP|vFCPUFDest|vICMSUFDest|vICMSUFRemet|vBCST|vST|vFCPST|vFCPSTRet|vProd|vFrete|vSeg|vDesc|vII|vIPI|vIPIDevol|vPIS|vCOFINS|vOutro|vNF|vTotTrib|
# W02_400_30
W02|vBC|vICMS|vICMSDeson|vFCP|vFCPUFDest|vICMSUFDest|vICMSUFRemet|vBCST|vST|vFCPST|vFCPSTRet|qBCMono|vICMSMono|qBCMonoReten|vICMSMonoReten|qBCMonoRet|vICMSMonoRet|vProd|vFrete|vSeg|vDesc|vII|vIPI|vIPIDevol|vPIS|vCOFINS|vOutro|vNF|vTotTrib|
W04|vICMSUFDest|vICMSUFRemet|vFCPUFDest|
# W17_400
W17|VServ|VBC|VISS|VPIS|VCOFINS|dCompet|vDeducao|vOutro|vDescIncond|vDescCond|vISSRet|cRegTrib|
W23|VRetPIS|VRetCOFINS|VRetCSLL|VBCIRRF|VIRRF|VBCRetPrev|VRetPrev|
W31|vIS|
W34|vBCIBSCBS|
W36|vIBS|vCredPres|vCredPresCondSus|
W37|vDif|vDevTrib|vIBSUF|
W42|vDif|vDevTrib|vIBSMun|
W50|vDif|vDevTrib|vCBS|vCredPres|vCredPresCondSus|
W57|vIBSMono|vCBSMono|vIBSMonoReten|vCBSMonoReten|vIBSMonoRet|vCBSMonoRet|
W59e|vIBSEstCred|vCBSEstCred|
W60|vNFTot|
```

## X

```text
X|modFrete|
X03|xNome|IE|xEnder|xMun|UF|
X04|CNPJ|
X05|CPF|
X11|VServ|VBCRet|PICMSRet|VICMSRet|CFOP|CMunFG|
X18|Placa|UF|RNTC|
# X22_400
X22|Placa|UF|RNTC|vagao|balsa|
X26|QVol|Esp|Marca|NVol|PesoL|PesoB|
X33|NLacre|
```

## Y

```text
Y02|NFat|VOrig|VDesc|VLiq|
Y07|NDup|DVenc|VDup|
# YA_9
YA|indPag|tPag|xPag|vPag|CNPJ|tBand|cAut|tpIntegra|
# YA_14
YA|indPag|tPag|xPag|vPag|dPag|CNPJPag|UFPag|CNPJ|tBand|cAut|tpIntegra|CNPJReceb|idTermPag|
YA04|tpIntegra|
YA04a|tpIntegra|
YA09|vTroco|
YB|CNPJ|idCadIntTran|
```

## Z

```text
Z|InfAdFisco|InfCpl|
Z04|XCampo|XTexto|
Z07|XCampo|XTexto|
Z10|NProc|IndProc|
# Z10_4
Z10|NProc|IndProc|tpAto|
# ZA_400
ZA|UFSaidaPais|xLocExporta|xLocDespacho|
# ZA01_400
ZA01|UFSaidaPais|xLocExporta|xLocDespacho|
ZB|XNEmp|XPed|XCont|
ZC|safra|ref|qTotMes|qTotAnt|qTotGer|vFor|vTotDed|vLiqFor|
ZC01|safra|ref|qTotMes|qTotAnt|qTotGer|vFor|vTotDed|vLiqFor|
ZC04|dia|qtde|
ZC10|xDed|vDed|
ZD|CNPJ|xContato|email|fone|idCSRT|hashCSRT|
ZF02|nReceituario|CPFRespTec|
ZF04|tpGuia|UFGuia|serieGuia|nGuia|
```

