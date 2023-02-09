* ---------------------------------------------------------------------------------
* Enviar Nfe de forma síncrona
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarNFCeSincronoDesserializando()
   Local oConfig
   Local oXml, oNfe, oInfNFe, oIde, oEmit, oEnderEmit, oDest, oEnderDest
   Local oDet, oProd
   Local oImposto, oICMS, oICMSSN102, oPIS, oPISOutr, oCOFINS, oCOFINSOutr
   Local oTotal, oICMSTot
   Local oTransp, oVol
   Local oCobr, oFat, oDup
   Local oPag, oDetPag
   Local oInfAdic, oInfRespTec
   Local oAutorizacao, oRetAutorizacao, oXmlRec, oConfigRec
   Local I, oErro, notaAssinada
   Local oXmlConsSitNFe, oConteudoNFe, oConteudoInfNFe, chaveNFe, oConfigConsSitNFe, oConsultaProtocolo

 * Criar configuraçao básica para consumir o serviço
   oConfig = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfig:TipoDfe = 1 // 1=nfce
   oConfig:TipoEmissao = 1 // 1=Normal
   oConfig:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfig:CertificadoSenha = "12345678"
   oConfig:CSC = "HCJBIRTWGCQ3HVQN7DCA0ZY0P2NYT6FVLPJG"
   oConfig:CSCIDToken = 2   
   
 * Criar XML   
   oXml = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
   oXml:Versao = "4.00"
   oXml:IdLote = "000000000000001"
   oXml:IndSinc = 1 // 1=Sim 0=Nao
   
 * Criar a tag NFe e deserializar o XML já gravado no HD para já preencher o objeto para envio
   onfe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
   
   oXml:AddNFe(oNFe:LoadFromFile("D:\testenfe\notateste-nfce.xml")) 

   //Como deserializar partindo da string do XML
   //oXml:AddNFe(oNFe:LoadFromXML("asldkjaslkdjasldjaslkdjasldkjasldksjadas"))   
   
 * Recuperar a chave da NFe:
   oConteudoNFe = oXml:GetNFe(0)
   oConteudoInfNFe = oConteudoNFe:GetInfNFe(0)
   chaveNFe = oConteudoInfNFe:Chave
		 
   ? "Chave da NFe:", chaveNFe
   Wait
   Cls

 * Consumir o serviço (Enviar NFE para SEFAZ)
   oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFCe.Autorizacao") //###
   
   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try
      oAutorizacao:SetXMLConfiguracao(oXml, oConfig)
	  
      // Pode-se gravar o conteudo do XML assinado na base de dados antes do envio, caso queira recuperar para futuro tratamento, isso da garantias
      notaAssinada = oAutorizacao:GetConteudoNFeAssinada(0) //###
      ? notaAssinada //Demonstrar o XML da nota assinada na tela
	  
      Wait
	  cls
	  
	  oAutorizacao:Executar(oXml, oConfig) 
	  
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

            //Como pegar o numero do protocolo de autorização para gravar na base
		    ? oAutorizacao:Result:ProtNFe:InfProt:NProt
		 else
            //Rejeitada ou Denegada - Fazer devidos tratamentos		 
         End		 
		 
		 Wait
		 Cls	  
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