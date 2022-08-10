* ---------------------------------------------------------------------------------
* Enviar evento de cancelamento da NFCe
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function ConsultarGTIN()
   Local oErro, oExceptionInterop
   Local oConfiguracao, oConsGTIN, oCcgConsGTIN
   
 * Criar configuraçao básica para consumir o serviço
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDfe = 10 // 10=CCG
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

 * Criar o XML da Consulta GTIN
   oConsGTIN = CreateObject("Unimake.Business.DFe.Xml.CCG.ConsGTIN")
   oConsGTIN:Versao = "1.00"
   oConsGTIN:GTIN = "7896015516031"

   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
   
   Try 
    * Enviar o XML de consulta
	  oCcgConsGTIN = CreateObject("Unimake.Business.DFe.Servicos.CCG.CcgConsGTIN")
	  oCcgConsGTIN:Executar(oConsGTIN, oConfiguracao) 
	  
      ? "XML retornado pela SEFAZ:"
	  ?
      ? oCcgConsGTIN:RetornoWSString
	  ?
	  ?
	  wait
	  cls

      if oCcgConsGTIN:Result:CStat == 9490 //Consulta realizada com sucesso
         ? "GTIN:"
		 ? oCcgConsGTIN:Result:GTIN
		 ?
         ? "NCM:"
		 ? oCcgConsGTIN:Result:NCM 
		 ?
         ? "Tipo GTIN:"
		 ? oCcgConsGTIN:Result:TpGTIN
		 ?
         ? "Descricao Produto:"
		 ? oCcgConsGTIN:Result:XProd
		 ?
         ? "Motivo:"
		 ? oCcgConsGTIN:Result:XMotivo
		 ?
         ? "CEST 1:"
		 ? If(oCcgConsGTIN:Result:GetCESTCount >= 1, oCcgConsGTIN:Result:GetCEST(0), "SEM CEST") 
		 ?
         ? "CEST 2:"
		 ? If(oCcgConsGTIN:Result:GetCESTCount >= 2, oCcgConsGTIN:Result:GetCEST(1), "SEM CEST") 
		 ?
         ? "CEST 3:"
		 ? If(oCcgConsGTIN:Result:GetCESTCount >= 3, oCcgConsGTIN:Result:GetCEST(2), "SEM CEST") 
      Else
	     //Consulta foi rejeitada, fazer devidos tratamentos.
      EndIf 
	  ?
	  ?
	  Wait	  

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