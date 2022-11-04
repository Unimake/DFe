* ---------------------------------------------------------------------------------
* Desserializar o XML de Distribuição de uma NFe de compra para dar entrada 
* no ERP automaticamente (B2B)
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function DesserializarXMLNFeCompra()
   Local oErro, oExceptionInterop
   Local oNfeProc, oNFe, oInfNFe, I, oAutXML, oDet, B, oICMS, oVol, oDup, oDetPag

   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
   
   Cls
   
   Try 
    * Criar o objeto NfeProc e Desserializar o XML de distribuição da NFe
      oNfeProc = CreateObject("Unimake.Business.DFe.Xml.NFe.NfeProc")   
      oNfeProc = oNfeProc:LoadFromFile("D:\testenfe\xharbour\Unimake.DFe\35190932267992000104550010000011721160462508-procNFe.xml")      

    * Chave da NFe
	  oInfNFe = oNfeProc:Nfe:GetInfNFe(0)
	  
	  ? "Chave NFe:", oInfNFe:Chave
	  ?
	  Inkey(0)
	  
    * Resgatar dados do grupo de tag <ide>		  
	  ? "cUF:", oInfNFe:ide:cUF 
      ? "cNF:", oInfNFe:ide:cNF 
	  ? "natOp:", oInfNFe:ide:natOp
	  ? "mod:", oInfNFe:ide:mod
	  ? "serie:", oInfNFe:ide:serie
	  ? "nNF:", oInfNFe:ide:nNF
	  ? "dhEmi:", oInfNFe:ide:dhEmi
	  ? "dhSaiEnt:",oInfNFe:ide:dhSaiEnt
	  ? "tpNF:", oInfNFe:ide:tpNF
	  ? "idDest:", oInfNFe:ide:idDest
	  ? "cMunFG:", oInfNFe:ide:cMunFG
	  ? "tpImp:", oInfNFe:ide:tpImp
	  ? "tpEmis:", oInfNFe:ide:tpEmis
	  ? "cDV:", oInfNFe:ide:cDV
	  ? "tpAmb:", oInfNFe:ide:tpAmb
	  ? "finNFe:", oInfNFe:ide:finNFe
	  ? "indFinal:",oInfNFe:ide:indFinal
	  ? "indPres:", oInfNFe:ide:indPres
	  ? "procEmi:", oInfNFe:ide:procEmi
	  ? "verProc:", oInfNFe:ide:verProc
	  ?
	  Inkey(0)
	  
	* Resgatar dados do grupo de tag <emit>
	  ? "CNPJ:", oInfNFe:emit:CNPJ
	  ? "xNome:", oInfNFe:emit:xNome
	  ? "xFant:", oInfNFe:emit:xFant
	  ? "EnderEmit:xLgr:", oInfNFe:emit:EnderEmit:xLgr
	  ? "EnderEmit:nro:", oInfNFe:emit:EnderEmit:nro
	  ? "EnderEmit:xBairro:",oInfNFe:emit:EnderEmit:xBairro
	  ? "EnderEmit:cMun:", oInfNFe:emit:EnderEmit:cMun
	  ? "EnderEmit:xMun :", oInfNFe:emit:EnderEmit:xMun
	  ? "EnderEmit:UF:", oInfNFe:emit:EnderEmit:UF
	  ? "EnderEmit:CEP:", oInfNFe:emit:EnderEmit:CEP
	  ? "EnderEmit:cPais:", oInfNFe:emit:EnderEmit:cPais
	  ? "EnderEmit:xPais:", oInfNFe:emit:EnderEmit:xPais
	  ? "EnderEmit:fone:", oInfNFe:emit:EnderEmit:fone	  
 	  ? "IE:", oInfNFe:emit:IE
	  ? "CRT:", oInfNFe:emit:CRT
	  ?
	  Inkey(0)
	  
	* Resgatar dados do grupo de tag <dest>  
	  ? "CNPJ:", oInfNFe:dest:CNPJ
	  ? "xNome:", oInfNFe:dest:xNome
	  ? "enderDest:xLgr:", oInfNFe:dest:enderDest:xLgr
	  ? "enderDest:nro:", oInfNFe:dest:enderDest:nro
	  ? "enderDest:xCpl:", oInfNFe:dest:enderDest:xCpl
	  ? "enderDest:xBairro:",oInfNFe:dest:enderDest:xBairro
	  ? "enderDest:cMun:", oInfNFe:dest:enderDest:cMun
	  ? "enderDest:xMun:", oInfNFe:dest:enderDest:xMun
	  ? "enderDest:UF:", oInfNFe:dest:enderDest:UF
	  ? "enderDest:CEP:", oInfNFe:dest:enderDest:CEP
	  ? "enderDest:cPais:", oInfNFe:dest:enderDest:cPais
	  ? "enderDest:xPais:", oInfNFe:dest:enderDest:xPais
	  ? "enderDest:fone:", oInfNFe:dest:enderDest:fone
	  ? "indIEDest:", oInfNFe:dest:indIEDest
	  ? "IE:", oInfNFe:dest:IE
	  ? "email:", oInfNFe:dest:email
	  ?
	  Inkey(0)
    * Resgatar dados do grupo de tag <autXML> 
	  For I = 1 To oInfNfe:GetAutXMLCount()
	      oAutXML = oInfNFe:GetAutXML(I-1)
		  
		  If oAutXML:CNPJ != NIL
		     ? "CNPJ:", oAutXML:CNPJ
	      Else
             ? "CPF:", oAutXML:CPF
		  Endif	
          ? 		  
          Inkey(0)
	  Next I	  
	  
	* Resgatar dados do grupo de tag <det>	  
	  For I = 1 To oInfNFe:GetDetCount()
	     oDet := oInfNfe:GetDet(I-1)
		 ? "cProd:", oDet:prod:cProd
		 ? "cEAN:", oDet:Prod:cEAN
		 ? "xProd:", oDet:Prod:xProd
		 ? "NCM:", oDet:Prod:NCM
		 ? "CEST:", oDet:Prod:CEST
		 ? "CFOP:", oDet:Prod:CFOP
		 ? "uCom:", oDet:Prod:uCom
		 ? "qCom:", oDet:Prod:qCom
		 ? "vUnCom:", oDet:Prod:vUnCom
		 ? "vProd:", oDet:Prod:vProd
		 ? "cEANTrib:", oDet:Prod:cEANTrib
		 ? "uTrib:", oDet:Prod:uTrib
		 ? "qTrib:", oDet:Prod:qTrib
		 ? "vUnTrib:", oDet:Prod:vUnTrib
		 ? "indProd:", oDet:Prod:indTot	
         ? "nItem:", oDet:NItem
		 ?
         Inkey(0)

       * Resgatar dados do grupo de tag <ICMS>
		 For B = 1 To oDet:Imposto:GetICMSCount()
		    oICMS = oDet:Imposto:GetICMS(B-1)
			
			If oICMS:ICMS00 != NIL
			   ? "ICMS00:Orig:", oICMS:ICMS00:Orig
			   ? "ICMS00:Orig:", oICMS:ICMS00:CST
			Endif
			
			If oICMS:ICMS10 != NIL
			   ? "ICMS10:Orig:", oICMS:ICMS10:Orig
			   ? "ICMS10:Orig:", oICMS:ICMS10:CST
			Endif

			If oICMS:ICMSPart != NIL
			   ? "ICMSPart:Orig:", oICMS:ICMSPart:Orig
			   ? "ICMSPart:Orig:", oICMS:ICMSPart:CST
			Endif
			
			If oICMS:ICMSSN101 != NIL
			   ? "ICMSSN101:Orig:", oICMS:ICMSSN101:Orig
			   ? "ICMSSN101:Orig:", oICMS:ICMSSN101:CSOSN
			Endif

            If oICMS:ICMSSN102 != NIL
			   ? "ICMSSN102:Orig:", oICMS:ICMSSN102:Orig
			   ? "ICMSSN102:Orig:", oICMS:ICMSSN102:CSOSN
			Endif

            If oICMS:ICMSSN202 != NIL
			   ? "ICMSSN202:Orig:", oICMS:ICMSSN202:Orig
			   ? "ICMSSN202:Orig:", oICMS:ICMSSN202:CSOSN
			   ? "ICMSSN202:Orig:", oICMS:ICMSSN202:modBCST
			   ? "ICMSSN202:Orig:", oICMS:ICMSSN202:vBCST
			   ? "ICMSSN202:Orig:", oICMS:ICMSSN202:pICMSST
			   ? "ICMSSN202:Orig:", oICMS:ICMSSN202:vICMSST
			Endif
			?
			Inkey(0)
		 Next B 
		 
 	   * Resgatar dados do grupo de tag <IPI>
	     ? "Imposto:IPI:cEnq:", oDet:Imposto:IPI:cEnq
		 
		 If oDet:Imposto:IPI:IPINT != NIL
		    ? "Imposto:IPI:IPINT:CST:", oDet:Imposto:IPI:IPINT:CST
		 Endif  
		 
		 If oDet:Imposto:IPI:IPITrib != NIL
		    ? "oDet:Imposto:IPI:IPITrib:CST:", oDet:Imposto:IPI:IPITrib:CST
		    ? "oDet:Imposto:IPI:IPITrib:vBC:", oDet:Imposto:IPI:IPITrib:vBC
		    ? "oDet:Imposto:IPI:IPITrib:pIPI:", oDet:Imposto:IPI:IPITrib:pIPI
		    ? "oDet:Imposto:IPI:IPITrib:vIPI:", oDet:Imposto:IPI:IPITrib:vIPI
		 Endif		 
		 
		 ?
		 Inkey(0)
		 
	   * Resgatar dados do grupo de tag <PIS>
	     If  oDet:Imposto:PIS:PISAliq != NIL
		     ? "oDet:Imposto:PIS:PISAliq:CST:", oDet:Imposto:PIS:PISAliq:CST
		 Endif

	     If  oDet:Imposto:PIS:PISOutr != NIL
		     ? "oDet:Imposto:PIS:PISOutr:CST:", oDet:Imposto:PIS:PISOutr:CST
			 ? "oDet:Imposto:PIS:PISOutr:vBC:", oDet:Imposto:PIS:PISOutr:vBC
			 ? "oDet:Imposto:PIS:PISOutr:pPIS:", oDet:Imposto:PIS:PISOutr:pPIS
			 ? "oDet:Imposto:PIS:PISOutr:vPIS:", oDet:Imposto:PIS:PISOutr:vPIS
		 Endif
		 ?
		 Inkey(0)		 

	   * Resgatar dados do grupo de tag <COFINS>
	     If  oDet:Imposto:COFINS:COFINSAliq != NIL
		     ? "oDet:Imposto:COFINS:COFINSAliq:CST:", oDet:Imposto:COFINS:COFINSAliq:CST
		 Endif

	     If  oDet:Imposto:COFINS:COFINSOutr != NIL
		     ? "oDet:Imposto:COFINS:COFINSOutr:CST:", oDet:Imposto:COFINS:COFINSOutr:CST
			 ? "oDet:Imposto:COFINS:COFINSOutr:vBC:", oDet:Imposto:COFINS:COFINSOutr:vBC
			 ? "oDet:Imposto:COFINS:COFINSOutr:pCOFINS:", oDet:Imposto:COFINS:COFINSOutr:pCOFINS
			 ? "oDet:Imposto:COFINS:COFINSOutr:vCOFINS:", oDet:Imposto:COFINS:COFINSOutr:vCOFINS
		 Endif
		 ?
		 Inkey(0)		 		 
      Next I
	  
	  
	* Resgatar dados do grupo de tag <total><ICMSTot>
	  ? "oInfNFe:Total:ICMSTot:vBC:", oInfNFe:Total:ICMSTot:vBC
	  ? "oInfNFe:Total:ICMSTot:vICMS:", oInfNFe:Total:ICMSTot:vICMS
	  ? "oInfNFe:Total:ICMSTot:vICMSDeson:", oInfNFe:Total:ICMSTot:vICMSDeson
	  ? "oInfNFe:Total:ICMSTot:vFCP:", oInfNFe:Total:ICMSTot:vFCP
	  ? "oInfNFe:Total:ICMSTot:vBCST:", oInfNFe:Total:ICMSTot:vBCST
	  ? "oInfNFe:Total:ICMSTot:vST:", oInfNFe:Total:ICMSTot:vST
	  ? "oInfNFe:Total:ICMSTot:vFCPST:", oInfNFe:Total:ICMSTot:vFCPST
	  ? "oInfNFe:Total:ICMSTot:vFCPSTRet:", oInfNFe:Total:ICMSTot:vFCPSTRet
	  ? "oInfNFe:Total:ICMSTot:vProd:", oInfNFe:Total:ICMSTot:vProd
	  ? "oInfNFe:Total:ICMSTot:vFrete:", oInfNFe:Total:ICMSTot:vFrete
	  ? "oInfNFe:Total:ICMSTot:vSeg:", oInfNFe:Total:ICMSTot:vSeg
	  ? "oInfNFe:Total:ICMSTot:vDesc:", oInfNFe:Total:ICMSTot:vDesc
	  ? "oInfNFe:Total:ICMSTot:vII:", oInfNFe:Total:ICMSTot:vII
	  ? "oInfNFe:Total:ICMSTot:vIPI:", oInfNFe:Total:ICMSTot:vIPI
	  ? "oInfNFe:Total:ICMSTot:vIPIDevol:", oInfNFe:Total:ICMSTot:vIPIDevol
	  ? "oInfNFe:Total:ICMSTot:vPIS:", oInfNFe:Total:ICMSTot:vPIS
	  ? "oInfNFe:Total:ICMSTot:vCOFINS:", oInfNFe:Total:ICMSTot:vCOFINS
	  ? "oInfNFe:Total:ICMSTot:vOutro:", oInfNFe:Total:ICMSTot:vOutro
	  ? "oInfNFe:Total:ICMSTot:vNF:", oInfNFe:Total:ICMSTot:vNF
	  ?
	  Inkey(0)
	  
	* Resgatar dados do grupo de tag <transp>
      ? "oInfNFe:Transp:ModFrete:", oInfNFe:Transp:ModFrete	
	  ?
	  Inkey(0)

	  For I = 1 To oInfNFe:Transp:GetVolCount()
	     oVol := oInfNFe:Transp:GetVol(I-1)
		 
		 ? "oInfNFe:Transp:Vol:PesoL:", oVol:pesoL
		 ? "oInfNFe:Transp:Vol:pesoB:", oVol:pesoB
		 ?
		 Inkey(0)
	  Next I

	* Resgatar dados do grupo de tag <cobr>  
	  ? "oInfNfe:Cobr:Fat:NFat", oInfNfe:Cobr:Fat:NFat
	  ? "oInfNfe:Cobr:Fat:vOrig", oInfNfe:Cobr:Fat:vOrig
	  ? "oInfNfe:Cobr:Fat:vDesc", oInfNfe:Cobr:Fat:vDesc
	  ? "oInfNfe:Cobr:Fat:vLiq", oInfNfe:Cobr:Fat:vLiq
	  ?
	  Inkey(0)

      For I = 1 To oInfNFe:Cobr:GetDupCount()
	     oDup := oInfNFe:Cobr:GetDup(I-1)
		 
		  ? "oInfNfe:Cobr:Dup:NDup:", oDup:NDup
		  ? "oInfNfe:Cobr:Dup:DVenc:", oDup:DVenc
		  ? "oInfNfe:Cobr:Dup:VDup:", oDup:VDup
		  ?
		  ? Inkey(0)		 
      Next I

	* Resgatar dados do grupo de tag <pag>
      For I = 1 To oInfNFe:Pag:GetDetPagCount()	
	      oDetPag = oInfNFe:Pag:GetDetPag(I-1)	
		 
		  ? "oInfNFe:Pag:DetPag:indPag:", oDetPag:indPag
		  ? "oInfNFe:Pag:DetPag:tPag:", oDetPag:tPag
		  ? "oInfNFe:Pag:DetPag:vPag:", oDetPag:vPag
		  ?
		  Inkey(0)
	  Next I

	  ? "oInfNFe:Pag:VTroco:", oInfNFe:Pag:VTroco
	  ?
	  Inkey(0)
	  
	* Resgatar dados do grupo de tag <infAdic>
	  If oInfNFe:InfAdic:InfAdFisco != NIL	  
	     ? "oInfNFe:InfAdic:InfAdFisco:", oInfNFe:InfAdic:InfAdFisco
      Endif
      If oInfNFe:InfAdic:InfCpl != NIL	  
	     ? "oInfNFe:InfAdic:InfCpl:", oInfNFe:InfAdic:InfCpl
      Endif		 
	  ?
	  Inkey(0)
	  
	* Resgatar dados do grupo de tag <infRespTec>
      ? "oInfNFe:InfRespTec:CNPJ:", oInfNFe:InfRespTec:CNPJ
	  ? "oInfNFe:InfRespTec:xContato:", oInfNFe:InfRespTec:xContato
	  ? "oInfNFe:InfRespTec:email:", oInfNFe:InfRespTec:email
	  ? "oInfNFe:InfRespTec:fone:", oInfNFe:InfRespTec:fone
	  ?
	  Inkey(0)	  
	
	* Resgatar o <DigestValue> e comparar com o <digVal> do protocolo de autorização
	  ? "oNFeProc:NFe:Signature:SignedInfo:Reference:DigestValue:", oNfeProc:NFe:Signature:SignedInfo:Reference:DigestValue
	  ? "oNFeProc:ProtNFe:InfProt:digVal:", oNFeProc:ProtNFe:InfProt:digVal
	  ?

	  If oNFeProc:NFe:Signature:SignedInfo:Reference:DigestValue == oNfeProc:ProtNFe:InfProt:digVal
	     ? "Digest value correto!!!"
         ?		 
	     ? "Digest Value da assinatura igual ao do protocolo de autorizacao."
	  Else
         ? "Digest Value incorreto!!!"
		 ?
	     ? "Digest Value da assinatura diferente ao do protocolo de autorizacao."
	  Endif
      ?
	  ? Inkey(0)
	
	* Protocolo de autorização  
	  ? "TpAmb:", oNFeProc:ProtNFe:InfProt:TpAmb
	  ? "verAplic:", oNFeProc:ProtNFe:InfProt:verAplic
	  ? "chNFe:", oNFeProc:ProtNFe:InfProt:chNFe
	  ? "dhRecbto:", oNFeProc:ProtNFe:InfProt:dhRecbto
	  ? "nProt:", oNFeProc:ProtNFe:InfProt:nProt
	  ? "digVal:", oNFeProc:ProtNFe:InfProt:digVal
	  ? "cStat:", oNFeProc:ProtNFe:InfProt:cStat
	  ? "xMotivo:", oNFeProc:ProtNFe:InfProt:xMotivo	  
	  ?
	  Inkey(0)	
	  
      Cls
	  
   Catch oErro
      //Demonstrar exceções geradas no proprio Harbour, se existir.
	  ? "ERRO"
	  ? "===="
	  ? "Falha ao tentar consultar o status do servico."
      ? oErro:Description
      ? oErro:Operation
	  
      //Demonstrar a exceção do CSHARP
	  ?
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
      ?     
	  
	  Wait
	  cls   
   End	   
Return