*IF !FILE('cadastro.DBF')
   CREATE TABLE empresa (CNPJ C(14),;
	XNome C(100),;
	XFant C(100),;
	IE  C(50),;
	IM  C(50),;
	CNAE  C(50),;
	CRT C( 1),;
	XLgr C(100),;
	Nro C (20),;
	XBairro C(50),;
	CMun C(8),;
	XMun C(50),;
	UF N(2),;
	CEP C(8),;
	Fone C(50))
  CREATE TABLE cliente (CNPJ C(14),;
    XNome C(100),;
    IndIEDest N(1),;
    IE C(50),;
    Email C(100),;
    XLgr C(100),;
    Nro C(50),;
    XBairro C(100),;
   CMun N (8),;
   XMun C (50),;
   UF N(2),;
   CEP C(8),;
   Fone C(100))

CREATE TABLE totais (VBC N(10,2),;
	VICMS N(10,2),;
	VICMSDeson N(10,2),;
	VFCP N(10,2),;
	VBCST N(10,2),;
	VST N(10,2),;
    VFCPST N(10,2),;
 	VFCPSTRet N(10,2),;
 	VProd N(10,2),;
 	VFrete N(10,2),;
	VSeg N(10,2),;
	VDesc N(10,2),;
	VII N(10,2),;
	VIPI N(10,2),;
	VIPIDevol N(10,2),;
	VPIS N(10,2),;
	VCOFINS N(10,2),;
	VOutro N(10,2),;
	VNF N(10,2),; &&= 140.3
	VTotTrib N(10,2)) &&12.63
    
    CREATE TABLE TRANSP (XNome C(100),;
    XEnder C(100),;
    XMun C(100),;
    UF N(2),;
    QVol N(2),;
    Esp C(100),;
    Marca C(100),;
    PesoL N(2),;
    PesoB N(2),;
    ModFrete N(2))

    CREATE TABLE FATURA (NFat C(50),;
    VOrig N(10,2),; &&140.3
	VDesc N(10,2),;
	VLiq N(10,2),;&&	 140.3
	Fat C(50),; 
	NDup C(3),;
	DVenc D(8),;
	VDup N(10,2))


    CREATE TABLE TIPOPG  (TPag N(2),;
    VPag N(10,2))
    
    CREATE TABLE INFORMA  (InfCpl C(254))


    CREATE TABLE RESP (CNPJ C(14),;
    XContato C(100),;
    Email C(100),;
    Fone C(50))
    
    CREATE TABLE PROUTOS;
    (CProd C(15),;
    CEAN C(50),;
    XProd C(100),;
    NCM C(9),;
    CFOP C(4),;
    UCom C(5),;
    QCom C(5),;
    VUnCom N(10,2),;&&= 140.3
	VProd N(10,2),;&&= 140.3
	CEANTrib C(50),;&&= "SEM GTIN"
	UTrib C(5),;&&= "LU"
	QTrib N(10),;
	VUnTrib N(10,2),;
	IndTot N(10,2),;
	XPed C(50),;
	NItemPed N(2),;
    VTotTrib N(10,2),; &&= 12.63
    ICMS C(3),;
    Orig N(10,2),;
    PCredSN N(10,2),;
    CST C(3),;
    VBCPIS N(10,2),;
    PPIS N(10,2),;
    VPIS N(10,2),;   
    PISOutr N(10,2),;
    PIS C(3),;
    VBCCOFINS N(10,2),;
    PCOFINS N(10,2),;
    VCOFINS N(10,2),;
    COFINSOutr N(10,2),;
    COFINS C(2))
    
    CREATE TABLE nfe;
    (CUF n (2),;
    NatOp c(100),;
    Mod N(2),;
    Serie N(10),;
    NNF N(10),;
    DhEmi D(8),;
    HORAE C(8),;
    DhSaiEnt D(8),;
    horat C(8),;
    TpNF N(1),;
    IdDest N(1),;
    CMunFG N(10),;
    TpImp N(1),;
    TpEmis N(1),;
	TpAmb N(1),;
	FinNFe N(1),;
	IndFinal N(1),;
	IndPres N(1),;
	ProcEmi N(1),;
	VerProc N(1))
	