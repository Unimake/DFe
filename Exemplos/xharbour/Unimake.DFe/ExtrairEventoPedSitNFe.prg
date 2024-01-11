* ---------------------------------------------------------------------------------
* Como extrair eventos retornados na consulta da situação da NFe/NFCe
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function ExtrairEventoPedSitNFe()
   Local oErro, oExceptionInterop
   Local oConfiguracao, oConsSitNfe, oProcEventoNFe, X, oConsultaProtocolo, xmlEvento, nHandle, nomeArqDistribEvento
   
 * Criar configuraçao básica para consumir o serviço
   oConfig = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfig:TipoDfe = 0 // 0=nfe
   oConfig:CertificadoSenha = "12345678"
   oConfig:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

 * Criar XML da consulta protocolo
   oConsSitNfe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsSitNfe")
   oConsSitNfe:Versao = "4.00"
   oConsSitNfe:TpAmb  = 2  // Homologação
   oConsSitNfe:ChNfe  = "35240110654122000155550010000085161700218900" // Chave da NFE 

   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
   
   Try 
    * Enviar a consulta
      oConsultaProtocolo = CreateObject("Unimake.Business.DFe.Servicos.NFe.ConsultaProtocolo")
      oConsultaProtocolo:Executar(oConsSitNfe, oConfig)
	  
      ? "XML retornado pela SEFAZ:"
	  ?
      ? oConsultaProtocolo:RetornoWSString
	  ?
	  ?
	  wait
	  cls
	  
	  
	  if oConsultaProtocolo:Result:GetProcEventoNFeCount() > 0
   	     ? "Extrair eventos retornados na consulta "
	     ?
	     For X := 1 To oConsultaProtocolo:Result:GetProcEventoNFeCount()
	         oProcEventoNFe = oConsultaProtocolo:Result:GetProcEventoNFe(X-1)

		   * Pegar o evento e salvar em um arquivo no HD 
 		     xmlEvento := oProcEventoNFe:GerarXMLString() //String do xml do evento
							 
			 ? xmlEvento
							 
			 nomeArqDistribEvento := "d:\testenfe\" + oProcEventoNFe:NomeArquivoDistribuicao
			 ? nomeArqDistribEvento

             nHandle := fCreate(nomeArqDistribEvento)
             fwrite(nHandle, xmlEvento)
             fClose(nHandle)
		  
	       * Se quiser pegar um evento específico, só comparar o tipo de evento, conforme segue
		     If oProcEventoNFe:Evento:InfEvento:TpEvento == 110111 //Cancelamento
			    * Aqui dentro do IF só fazer o que foi feito fora dele, nas linhas anteriores, mas só vai fazer se for um cancelamento.
	         Endif
	     Next X
	     ?
	     ?
	     Wait
	     Cls
      Endif		 

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