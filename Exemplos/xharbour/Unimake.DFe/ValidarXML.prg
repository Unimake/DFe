* ---------------------------------------------------------------------------------
* Validar XMLs
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function ValidarXML()
   Local oExceptionInterop, oErro
   Local oValidarSchema, schema

   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
   
   Try
      oValidarSchema = CreateObject("Unimake.Business.DFe.ValidarSchema")
	  
	  ? "Vamos validar o XML NFe..."
	  ?
	  Wait	  
	  
	  schema = "NFe.nfe_v4.00.xsd" 
	  
	  oValidarSchema:Validar("D:\testenfe\notateste-nfe.xml", schema, "http://www.portalfiscal.inf.br/nfe")
	  
	  if (oValidarSchema:Success)
	     ?
		 ?
         ? "XML validado com sucesso."
		 ?
		 ?	  
	  else
	     ?
		 ?
         ? "Code:", oValidarSchema:ErrorCode 
		 ? "Message:",  oValidarSchema:ErrorMessage
		 ?
		 ?	  
	  endif
	  Wait
	  Cls
	  
      ? "Vamos validar o XML de distribuicao da NFe..."
	  ?
	  Wait
	  
	  schema = "NFe.procNFe_v4.00.xsd"
      oValidarSchema:Validar("d:\testenfe\41220606117473000150550010000580071051443444-procnfe.xml", schema, "http://www.portalfiscal.inf.br/nfe")
	  
      if (oValidarSchema:Success)
	     ?
		 ?
         ? "XML validado com sucesso."
		 ?
		 ?
      else
	     ?
		 ?
         ? "Code:", oValidarSchema:ErrorCode 
		 ? "Message:",  oValidarSchema:ErrorMessage
		 ?
		 ?
	  Endif
      Wait
	  cls

	  ? "Vamos validar o XML do MDFe..."
	  ?
	  Wait

	  schema = "MDFe.mdfe_v3.00.xsd"
      oValidarSchema:Validar("D:\testenfe\xharbour\Unimake.DFe\mdfe.xml", schema, "http://www.portalfiscal.inf.br/mdfe")
	  
      if (oValidarSchema:Success)
	     ?
		 ?
         ? "XML validado com sucesso."
		 ?
		 ?
      else
	     ?
		 ?
         ? "Code:", oValidarSchema:ErrorCode 
		 ? "Message:",  oValidarSchema:ErrorMessage
		 ?
		 ?
	  Endif
      Wait
	  cls	  
	  
  	  ? "Vamos validar o XML de consulta Status do serviço da NFe..."
	  ?
	  Wait

	  schema = "NFe.consStatServ_v4.00.xsd"
      oValidarSchema:Validar("D:\testenfe\20100222T222310-ped-sta.xml", schema, "http://www.portalfiscal.inf.br/nfe")
	  
      if (oValidarSchema:Success)
	     ?
		 ?
         ? "XML validado com sucesso."
		 ?
		 ?
      else
	     ?
		 ?
         ? "Code:", oValidarSchema:ErrorCode 
		 ? "Message:",  oValidarSchema:ErrorMessage
		 ?
		 ?
	  Endif
      Wait
	  cls


  	  ? "Vamos validar o XML de consulta Status do serviço da NFe, mas passando a string do XML..."
	  ?
	  Wait	

  	  schema = "NFe.consStatServ_v4.00.xsd"

	  conteudoXML = [<?xml version="1.0" encoding="utf-8"?><consStatServ versao="4.00" xmlns="http://www.portalfiscal.inf.br/nfe"><tpAmb>2</tpAmb><cUF>88</cUF><xServ>STATUS</xServ></consStatServ>]

      oValidarSchema:ValidarString(conteudoXML, schema, "http://www.portalfiscal.inf.br/nfe")
	  
      if (oValidarSchema:Success)
	     ?
		 ?
         ? "XML validado com sucesso."
		 ?
		 ?
      else
	     ?
		 ?
         ? "Code:", oValidarSchema:ErrorCode 
		 ? "Message:",  oValidarSchema:ErrorMessage
		 ?
		 ?
	  Endif
      Wait
	  cls	  
   
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