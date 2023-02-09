* ---------------------------------------------------------------------------------
* Finalizar a nota fiscal através da consulta situação
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function FinalizarNFePelaConsultaSituacao()
   Local oConfig, oExceptionInterop, oXml, oNFe, oAutorizacao
   Local oErro, oConsultaProtocolo
   
 * Criar configuraçao básica para consumir o serviço
   oConfig = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfig:TipoDfe = 0 // 1=nfe
   oConfig:TipoEmissao = 1 // 1=Normal
   oConfig:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfig:CertificadoSenha = "12345678"
   
 * Criar XML   
   oXml = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
   oXml:Versao = "4.00"
   oXml:IdLote = "000000000000001"
   oXml:IndSinc = 1 // 1=Sim 0=Nao
   
 * Criar a tag NFe e deserializar o XML já gravado no HD para já preencher o objeto para envio
   oNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
   
   oXml:AddNFe(oNFe:LoadFromFile("d:\testenfe\nota_41220806117473000150550010000734461101759379-nfe.xml"))
   
 * Resgatar a chave da NFe
   oConteudoNFe = oXml:GetNFe(0)
   oConteudoInfNFe = oConteudoNFe:GetInfNFe(0)
   chaveNFe = oConteudoInfNFe:Chave 
   ? chaveNFe
   ?
   ?
   Wait 
   cls

 * Consumir o serviço (Enviar NFE para SEFAZ)
   oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Autorizacao") 
   
   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try
      oAutorizacao:SetXMLConfiguracao(oXml, oConfig)
	  
      notaAssinada = oAutorizacao:GetConteudoNFeAssinada(0)

      ? notaAssinada //Demonstrar o XML da nota assinada na tela
	  ?
	  ?
	  
      Wait
	  cls
	  
	  //Como não vou finalizar pela consulta recibo, vou zerar o objeto
	  oAutorizacao:SetNullRetConsReciNFe()
	  
      oConsSitNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsSitNFe")
	  oConsSitNFe:ChNFe = chaveNFe
	  oConsSitNFe:TpAmb = 1 //Producao
	  oConsSitNFe:Versao = "4.00"
	  
      oConsultaProtocolo = CreateObject("Unimake.Business.DFe.Servicos.NFe.ConsultaProtocolo")
      oConsultaProtocolo:Executar(oConsSitNFe, oConfig)
	  
	  ? oConsultaProtocolo:RetornoWSString
	  ?
	  ?
	  Wait
	  Cls
	  
      If oConsultaProtocolo:Result:CStat == 100
	     //Nota esta autorizaca, vou somente gerar o XML de distribuição
 	     oAutorizacao:AddRetConsSitNFes(oConsultaProtocolo:Result)

         oAutorizacao:GravarXmlDistribuicao("d:\testenfe")		 
      Else
         ? oConsultaProtocolo:Result:CStat	  
         ? oConsultaProtocolo:Result:XMotivo
         ?
         ?		 
		 Wait		 
		 Cls
		 
		 //Se não está autorizada, posso enviar novamente, começando do zero.
	  Endif

      ? "Fim!!!"
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