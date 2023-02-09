* ---------------------------------------------------------------------------------
* Enviar Nfe de forma síncrona - Deserializando XML
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function GerarXmlDistribuicaoNomeDif()
   Local oInicializarConfiguracao
   Local oXml, oNfe
   Local oAutorizacao
   Local I, oErro
   Local oConteudoNFe, oConteudoInfNFe, chaveNFe
   Local NomeArqDistribuicao, nHandle

 * Criar configuraçao básica para consumir o serviço
   oInicializarConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   
   oInicializarConfiguracao:TipoDfe = 0 // 0=nfe
   oInicializarConfiguracao:Servico = 6 // 6=Autorização Nfe
   oInicializarConfiguracao:TipoEmissao = 1 // 1=Normal
   oInicializarConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oInicializarConfiguracao:CertificadoSenha = "12345678"

 * Criar XML (Tag EnviNFe)  
   oXml = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
   oXml:Versao = "4.00"
   oXml:IdLote = "000000000000001"
   oXml:IndSinc = 1 // 1=Sim 0=Nao
   
 * Criar a tag NFe e deserializar o XML já gravado no HD para já preencher o objeto para envio
   onfe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
   oXml:AddNFe(oNFe:LoadFromFile("D:\testenfe\notateste-nfe.xml"))
   
 * Consumir o serviço (Enviar NFE para SEFAZ)
   oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Autorizacao")
   
   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")

   Try
      oAutorizacao:SetXMLConfiguracao(oXml, oInicializarConfiguracao)      
	  
	  oAutorizacao:Executar(oXml, oInicializarConfiguracao) 
	  
	  ? "XML Retornado pela SEFAZ"
      ? "========================"
      ? oAutorizacao:RetornoWSString
      ?
      ? "Codigo de Status e Motivo"
      ? "========================="
      ? AllTrim(Str(oAutorizacao:Result:CStat,5)), oAutorizacao:Result:XMotivo
	  ?
	  ?
	  Wait
	  cls
	  
	  if oAutorizacao:Result:ProtNFe <> NIL
         if oAutorizacao:Result:ProtNFe:InfProt:CStat == 100 //100 = Autorizado o uso da NF-e
            // Gravar XML de distribuição em uma pasta (NFe com o protocolo de autorização anexado)
            oAutorizacao:GravarXmlDistribuicao("d:\testenfe")
			
			//Para envio sincrono
		    docProcNFe = oAutorizacao:NfeProcResult:GerarXMLString();
			
			? docProcNFe			
			?
			?
			wait

			oConteudoNFe = oXml:GetNFe(0)
			oConteudoInfNFe = oConteudoNFe:GetInfNFe(0)

			// Nome do XML de distribuição gerado pela DLL segue o o seguinte padrão:
			//    41220606117473000150550010000580151230845956-procnfe.xml
			//
			// Vamos mudar e deixar ele assim:
			//    NFe41220606117473000150550010000580151230845956-ProcNFe.xml
		    //
			NomeArqDistribuicao = "D:\testenfe\NFe" + oConteudoInfNFe:Chave + "-ProcNFe.xml"
			?
			? NomeArqDistribuicao
			?
			?
			Wait
				
 			nHandle := fCreate(NomeArqDistribuicao)
 	 		FWrite(nHandle, docProcNFe)
 			FClose(nHandle)

			//Para envio assincrono vide o prg EnviarNFeAssincrono.PRG
         End		 		 
	  endif
	  
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