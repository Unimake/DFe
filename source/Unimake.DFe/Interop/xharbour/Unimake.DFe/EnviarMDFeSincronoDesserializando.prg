* ---------------------------------------------------------------------------------
* Enviar Nfe de forma assincrona
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarMDFeSincronoDesserializando()
   Local oConfiguracao, oErro
   Local oMDFe, oInfMDFe, oIDE, oInfMunCarrega, oEmit, oEnderEmit
   Local oInfModal, oRodo, oInfANTT, oInfContratante, oVeicTracao, oCondutor
   Local oInfDoc, oInfMunDescarga, oInfCTe, oInfNFe, oInfUnidTransp, oLacUnidTransp
   Local oInfUnidCarga, oLacUnidCarga, oSeg, oInfResp, oInfSeg, oProdPred
   Local oInfLotacao, oInfLocalCarrega, oInfLocalDescarrega, oTot, oLacres, oLacre
   Local oInfAdic, oInfRespTec, oExceptionInterop, oAutorizacao, oConfigRec, oConsReciMDFe
   Local oRetAutorizacao, oProtMDFe
   
 * Criar o objeto de configuração mínima
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 4 //4=MDFe
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"   
  
 * Criar o XML do MDFe
   oMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.MDFe")
   oMDFe = oMDFe:LoadFromFile("D:\testenfe\xharbour\Unimake.DFe\mdfe.xml")
   
   //oMDFe = oMDFe:LoadFromXML("string_do_xml...")      
   
 * Resgatar alguns dados do Objeto do XML para demostrar como funciona
   ? "CNPJ Emitente:", oMDFe:InfMDFe:Emit:CNPJ
   ? "Razao Emitente:", oMDFe:InfMDFe:Emit:XNome
   ? "Data Emissao:", oMDFe:InfMDFe:IDE:DhEmi
   ? "Chave do MDFe:", oMDFe:InfMDFe:Chave
   ?
   ? 
   Wait
   ?
   ?
   ?
   
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try
    * Criar o objeto para consumir o serviço de autorização do MDFe
      oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.MDFe.AutorizacaoSinc")
	  oAutorizacao:SetXMLConfiguracao(oMDFe, oConfiguracao)

	  //O conteúdo do XML assinado deve ser gravado na base de dados para ser recuperado 
	  //caso seja necessário. Imagine que de um problema no envio do MDFe e vc precise resgatar para enviar novamente.
	  ? "Demonstrar o XML do MDFe assinadom: "
	  ?
	  ? oAutorizacao:GetConteudoMDFeAssinado()
	  ?
	  ?
	  Wait
	  ?
	  ?
	  ?

      oAutorizacao:Executar(oMDFe, oConfiguracao)   
   
	  ? "XML retornado pela SEFAZ no envio do XML de MDFe:"
	  ?
	  ? oAutorizacao:RetornoWSString
	  ?
	  ?
	  Wait
	  ?
	  ?
	  ?
   
	  If oAutorizacao:Result <> NIL
	     ? "Status envio:", oAutorizacao:Result:CStat, oAutorizacao:Result:XMotivo
		 ?
		 ?
		 Wait
		 ?
		 ?
		 ?

         If oAutorizacao:Result:CStat == 104 //Lote processado
            If oAutorizacao:Result:ProtMDFe:InfProt:CStat == 100 //MDFe autorizado
  		       ? "Status da de autorizacao/rejeicao do MDFe:", oAutorizacao:Result:ProtMDFe:InfProt:CStat, oAutorizacao:Result:ProtMDFe:InfProt:CStat
			   ? "Protocolo de autorizacao: ", oAutorizacao:Result:ProtMDFe:InfProt:NProt
			   ?
			   Wait
			   ?
			   ?
			   ?
  	           oAutorizacao:GravarXmlDistribuicao("d:\testenfe")			   			   
			Else   
               //Rejeitado, fazer devidos tratamentos
			Endif
		 Else
             //Rejeitado, fazer devidos tratamentos		 
		 Endif		 
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