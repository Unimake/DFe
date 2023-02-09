* ---------------------------------------------------------------------------------
* Enviar CTe de forma assincrona
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarCTeAssincronoDesserializando()
   Local oExceptionInterop, oErro, oConfiguracao
   Local oEnviCTe, oCTe
   
 * Criar o objeto de configuração mínima
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 2 //2=CTe
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"   

 * Criar o XML do CTe
 
   //Criar grupo de tag <enviCTe> 
   oEnviCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.EnviCTe")
   oEnviCTe:Versao = "3.00"
   oEnviCTe:IdLote = "000000000000001"
   
   //Criar grupo de tag <CTe>
   oCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.CTe")   
   oEnviCTe:AddCTe(oCTe:LoadFromFile("c:\projetos\uninfe\exemplos\CTe 3.00\43120178408960000182570010000000041000000047-cte.xml"))
   
   //Como deserializar partindo da string do XML
   //oEnviCTe:AddCTe(oCTe:LoadFromXML([conteudo do xml aqui....]))      
   
 * Resgatar alguns dados do Objeto do XML para demostrar como funciona
   oTagCTe = oEnviCTe:GetCTe(0)
 
   ? "CNPJ Emitente:", oTagCTe:InfCTe:Emit:CNPJ
   ? "Razao Emitente:", oTagCTe:InfCTe:Emit:XNome
   ? "Data Emissao:", oTagCTe:InfCTe:Ide:DhEmi
   ? "Chave do CTe:", oTagCTe:InfCTe:Chave
   ?
   ? 
   Wait
   ?
   ?
   ?
   
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
   
   Try 
    * Criar o objeto para consumir o serviço de autorização do CTe
      oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.CTe.Autorizacao")
      oAutorizacao:SetXMLConfiguracao(oEnviCTe, oConfiguracao)
	  
	  //O conteúdo do XML assinado deve ser gravado na base de dados para ser recuperado 
	  //caso seja necessário. Imagine que de um problema no envio do CTe e vc precise resgatar para enviar novamente.
	  ? "Demonstrar o XML do CTe assinado: "
	  ?
	  ? oAutorizacao:GetConteudoCTeAssinado(0)
	  ?
	  ?
	  Wait
	  ?
	  ?
	  ?
	  
	  oAutorizacao:Executar(oEnviCTe, oConfiguracao)   
	  
      ? "XML retornado pela SEFAZ no envio do XML de CTe:"
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
		 
		 //Tratar o retorno do envio do XML do CTe
		 if oAutorizacao:Result:CStat = 103 //Lote Recebido com Sucesso
            //Fazer a consulta do Recibo para ver se a(s) nota(s) foram autorizadas
            ? "Consultando o recibo...."
            ?
			
			//Criar a configuração minima para realizar a consulta recibo
            oConfigRec = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
            oConfigRec:TipoDFe = 2 // TipoDFe.CTe
            oConfigRec:CertificadoSenha = "12345678"
            oConfigRec:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

            //Criar o XML de consulta recibo
            oConsReciCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.ConsReciCTe")
            oConsReciCTe:Versao = "3.00"
            oConsReciCTe:TpAmb = 2 // TipoAmbiente.Homologacao
            oConsReciCTe:NRec = oAutorizacao:Result:InfRec:NRec
			
			//Enviar a consulta do Recibo
            oRetAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.CTe.RetAutorizacao")
            oRetAutorizacao:Executar(oConsReciCTe, oConfigRec)
			
			//Tratar o retorno da consulta do recibo
            If oRetAutorizacao:Result <> NIL
			   oAutorizacao:RetConsReciCTe = oRetAutorizacao:Result
			   
             * Modelo para buscar o resultade de cada CTe do lote enviado
               ? "Codigo de Status e Motivo da Consulta Recibo"
               ? "============================================"
			   For I = 1 TO oRetAutorizacao:Result:GetProtCteCount()
			       oProtCte = oRetAutorizacao:Result:GetProtCte(I-1)
				   
				   ? AllTrim(Str(oProtCte:InfProt:CStat,5)), oProtCte:InfProt:XMotivo
				   ?
				   ?
				   Wait
				   Cls
				   
				   //Demonstrar o XML retornado
				   ? "XML retornado na consulta recibo:"
				   ?
				   ? oRetAutorizacao:RetornoWSString
				   ?
				   ?				   
				   Wait
				   Cls

				   ? "Proximo passo eh gravar o XML de distribuicao"
				   ?
				   ?				   
				   Wait
				   Cls
				   
				 * Salvar XML de distribuicao das notas enviadas na pasta informada  
				   if oProtCTe:InfProt:CStat == 100 //100=CTe Autorizada 
				      oAutorizacao:GravarXmlDistribuicao("d:\testenfe")					  
				   else
                       //CTe rejeitado, fazer devidos tratamentos
				   endif

                   ? "Fim!!!"
				   ?
				   ?
				   Wait 				 
			   Next I			   
			Endif
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