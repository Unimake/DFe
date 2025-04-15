* ---------------------------------------------------------------------------------
* Validar XML com o Unimake.Validator
* ---------------------------------------------------------------------------------
Function UnimakeValidator()
   Local oXMLUtilityInterop, oErro, oExceptionInterop
   Local xmlstring, xmlstringerro
   
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
   

 * Xml Sem erro   
   xmlstring = [<?xml version="1.0" encoding="UTF-8" ?><NFe xmlns="http://www.portalfiscal.inf.br/nfe"><infNFe Id="NFe43230492797901000174650200000000021000000577" versao="4.00"><ide><cUF>43</cUF><cNF>00000057</cNF>]
   xmlstring = xmlstring + [<natOp>VENDA DE MERCADORIA</natOp><mod>65</mod><serie>20</serie><nNF>2</nNF><dhEmi>2023-04-26T10:08:03-03:00</dhEmi><tpNF>1</tpNF><idDest>1</idDest><cMunFG>4314902</cMunFG><tpImp>4</tpImp><tpEmis>1</tpEmis>]
   xmlstring = xmlstring + [<cDV>7</cDV><tpAmb>2</tpAmb><finNFe>1</finNFe><indFinal>1</indFinal><indPres>1</indPres><procEmi>0</procEmi><verProc>1.4.229</verProc></ide><emit><CNPJ>92797901000174</CNPJ>]
   xmlstring = xmlstring + [<xNome>GREMIO FOOT-BALL PORTO ALEGRENSE</xNome><xFant>GREMIOMANIA ARENA</xFant><enderEmit><xLgr>AV PADRE LEOPOLDO BRENTANO</xLgr><nro>110/2100</nro><xBairro>HUMAITA</xBairro>]
   xmlstring = xmlstring + [<cMun>4314902</cMun><xMun>PORTO ALEGRE</xMun><UF>RS</UF><CEP>90250590</CEP><cPais>1058</cPais><xPais>BRASIL</xPais><fone>5132182000</fone></enderEmit><IE>0960578617</IE><CRT>3</CRT></emit>]
   xmlstring = xmlstring + [<det nItem="1"><prod><cProd>2050000017377</cProd><cEAN>SEM GTIN</cEAN><xProd>NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL</xProd><NCM>61052000</NCM><CFOP>5102</CFOP>]
   xmlstring = xmlstring + [<uCom>UN</uCom><qCom>1.0000</qCom><vUnCom>169.9000</vUnCom><vProd>169.90</vProd>]
   xmlstring = xmlstring + [<cEANTrib>SEM GTIN</cEANTrib><uTrib>UN</uTrib><qTrib>1.0000</qTrib><vUnTrib>169.9000</vUnTrib><indTot>1</indTot></prod><imposto><vTotTrib>37.72</vTotTrib><ICMS><ICMS00><orig>0</orig>]
   xmlstring = xmlstring + [<CST>00</CST><modBC>0</modBC><vBC>169.90</vBC><pICMS>17.00</pICMS><vICMS>28.88</vICMS></ICMS00></ICMS><PIS><PISNT><CST>07</CST></PISNT></PIS><COFINS><COFINSNT><CST>07</CST></COFINSNT>]
   xmlstring = xmlstring + [</COFINS></imposto></det><total><ICMSTot><vBC>169.90</vBC><vICMS>28.88</vICMS><vICMSDeson>0.00</vICMSDeson><vFCP>0.00</vFCP><vBCST>0.00</vBCST><vST>0.00</vST><vFCPST>0.00</vFCPST>]
   xmlstring = xmlstring + [<vFCPSTRet>0.00</vFCPSTRet><vProd>169.90</vProd><vFrete>0.00</vFrete><vSeg>0.00</vSeg><vDesc>0.00</vDesc><vII>0.00</vII><vIPI>0.00</vIPI><vIPIDevol>0.00</vIPIDevol><vPIS>0.00</vPIS>]
   xmlstring = xmlstring + [<vCOFINS>0.00</vCOFINS><vOutro>0.00</vOutro><vNF>169.90</vNF><vTotTrib>37.72</vTotTrib></ICMSTot></total><transp><modFrete>9</modFrete></transp><pag><detPag><tPag>01</tPag><vPag>169.90</vPag>]
   xmlstring = xmlstring + [</detPag></pag><infAdic><infCpl>OPERADOR: EDUARDO PERES CX-020;VENDEDOR: MEGASTORE *ARENA;</infCpl></infAdic></infNFe></NFe>]

   xmlstringerro = [<?xml version="1.0" encoding="UTF-8" ?><NFe xmlns="http://www.portalfiscal.inf.br/nfe"><infNFe Id="NFe43230492797901000174650200000000021000000577" versao="4.00"><ide><cUF>43</cUF><cNF>00000057</cNF>]
   xmlstringerro = xmlstringerro + [<natOp>VENDA DE MERCADORIA</natOp><mod>65</mod><serie>20</serie><nNF>2</nNF><dhEmi>2023-04-26T10:08:03-03:00</dhEmi><tpNF>1</tpNF><idDest>1</idDest><cMunFG>4314902</cMunFG><tpImp>4</tpImp><tpEmis>1</tpEmis>]
   xmlstringerro = xmlstringerro + [<cDV>7</cDV><tpAmb>2</tpAmb><finNFe>1</finNFe><indFinal>1</indFinal><indPres>1</indPres><procEmi>0</procEmi><verProc>1.4.229</verProc></ide><emit><CNPJ>92797901000174</CNPJ>]
   xmlstringerro = xmlstringerro + [<xNome>GREMIO FOOT-BALL PORTO ALEGRENSE</xNome><xFant>GREMIOMANIA ARENA</xFant><enderEmit><xLgr>AV PADRE LEOPOLDO BRENTANO</xLgr><nro>110/2100</nro><xBairro>HUMAITA</xBairro>]
   xmlstringerro = xmlstringerro + [<cMun>4314902</cMun><xMun>PORTO ALEGRE</xMun><UF>RS</UF><CEP>90250590</CEP><cPais>1058</cPais><xPais>BRASIL</xPais><fone>5132182000</fone></enderEmit><IE>0960578617</IE><CRT>3</CRT></emit>]
   xmlstringerro = xmlstringerro + [<det nItem="1"><prod><cProd>2050000017377</cProd><cEAN>0000000</cEAN><xProd>NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL</xProd><NCM>61052000</NCM><CFOP>5102</CFOP>]
   xmlstringerro = xmlstringerro + [<uCom>UN</uCom><qCom>1.0000</qCom><vUnCom>169.9000</vUnCom><vProd>169.90</vProd>]
   xmlstringerro = xmlstringerro + [<cEANTrib>SEM GTIN</cEANTrib><uTrib>UN</uTrib><qTrib>1.0000</qTrib><vUnTrib>169.9000</vUnTrib><indTot>1</indTot></prod><imposto><vTotTrib>37.72</vTotTrib><ICMS><ICMS00><orig>0</orig>]
   xmlstringerro = xmlstringerro + [<CST>00</CST><modBC>0</modBC><vBC>169.90</vBC><pICMS>17.00</pICMS><vICMS>28.88</vICMS></ICMS00></ICMS><PIS><PISNT><CST>07</CST></PISNT></PIS><COFINS><COFINSNT><CST>07</CST></COFINSNT>]
   xmlstringerro = xmlstringerro + [</COFINS></imposto></det><total><ICMSTot><vBC>169.90</vBC><vICMS>28.88</vICMS><vICMSDeson>0.00</vICMSDeson><vFCP>0.00</vFCP><vBCST>0.00</vBCST><vST>0.00</vST><vFCPST>0.00</vFCPST>]
   xmlstringerro = xmlstringerro + [<vFCPSTRet>0.00</vFCPSTRet><vProd>169.90</vProd><vFrete>0.00</vFrete><vSeg>0.00</vSeg><vDesc>0.00</vDesc><vII>0.00</vII><vIPI>0.00</vIPI><vIPIDevol>0.00</vIPIDevol><vPIS>0.00</vPIS>]
   xmlstringerro = xmlstringerro + [<vCOFINS>0.00</vCOFINS><vOutro>0.00</vOutro><vNF>169.90</vNF><vTotTrib>37.72</vTotTrib></ICMSTot></total><transp><modFrete>9</modFrete></transp><pag><detPag><tPag>01</tPag><vPag>169.90</vPag>]
   xmlstringerro = xmlstringerro + [</detPag></pag><infAdic><infCpl>OPERADOR: EDUARDO PERES CX-020;VENDEDOR: MEGASTORE *ARENA;</infCpl></infAdic></infNFe></NFe>]
   
   Try
      oXMLUtilityInterop = CreateObject("Unimake.Business.DFe.Utility.XMLUtilityInterop")
    * Este vai validar com sucesso
	  If oXMLUtilityInterop.Validate(xmlstring)
	     MessageBox("XML 1 Validado com sucesso!!!")
      Else
         * Se a validação ocorrer algum erro vai tudo para exceção, por isso este código tem que estar obrigatoriamente em um Try Catch, pois a mensagem de erro vai estar na exceção.	  
	  Endif

    * Este vai gerar a exceção
	  If oXMLUtilityInterop.Validate(xmlstringerro)
	     MessageBox("XML 2 Validado com sucesso!!!")
      Else
         * Se a validação ocorrer algum erro vai tudo para exceção, por isso este código tem que estar obrigatoriamente em um Try Catch, pois a mensagem de erro vai estar na exceção.	  
	  Endif
	  
    Catch To oErro
    * Exceção do FOXPRO
	* Mais sobre exceção em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MessageBox(oErro.ErrorNo)
	  MessageBox("Exceção foxpro: " + oErro.Message)
	  
    * Exceção do CSHARP
      MessageBox(oExceptionInterop.GetMessage())
      MessageBox(oExceptionInterop.GetErrorCode())
   EndTry   
Return