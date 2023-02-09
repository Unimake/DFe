* ---------------------------------------------------------------------------------
* Enviar Nfe de forma assincrona
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarMDFeAssincronoDesserializando()
   Local oConfiguracao, oErro
   Local oEnviMDFe, oMDFe
   Local oExceptionInterop, oAutorizacao, oConfigRec, oConsReciMDFe
   Local oRetAutorizacao, oProtMDFe
   
 * Criar o objeto de configuração mínima
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 4 //4=MDFe
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"   
  
 * Criar o XML do MDFe   
   //Criar a tag <enviMDFe>
   oEnviMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.EnviMDFe")
   oEnviMDfe:Versao = "3.00"
   oEnviMDFe:IdLote = "000000000000001"
   
   //Criar a tag <mdfe> e suas filhas a partir de uma desserialização do XML já existe em uma pasta
   oMDFe := CreateObject("Unimake.Business.DFe.Xml.MDFe.MDFe")
   oEnviMDFe:MDFe = oMDFe:LoadFromFile("D:\testenfe\xharbour\Unimake.DFe\mdfe.xml")
   
   //Criar a tag <mdfe> e suas filhas a partir de uma desserialização de uma string de XML
   //oMDFe := CreateObject("Unimake.Business.DFe.Xml.MDFe.MDFe")
   //oEnviMDFe:MDFe = oMDFe:LoadFromXML("string_do_xml...")     
   
 * Resgatar alguns dados do Objeto do XML para demostrar como funciona
   ? "CNPJ Emitente:", oEnviMDFe:MDFe:InfMDFe:Emit:CNPJ
   ? "Razao Emitente:", oEnviMDFe:MDFe:InfMDFe:Emit:XNome
   ? "Data Emissao:", oEnviMDFe:MDFe:InfMDFe:IDE:DhEmi
   ? "Chave do MDFe:", oEnviMDFe:MDFe:InfMDFe:Chave
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
      oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.MDFe.Autorizacao")
	  oAutorizacao:SetXMLConfiguracao(oEnviMDFe, oConfiguracao)

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

      oAutorizacao:Executar(oEnviMDFe, oConfiguracao)   
   
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
		 
		 If oAutorizacao:Result:CStat == 103 //Lotel Recebido com Sucesso
          * Criar objeto de configuração minima para consumir o serviço
            oConfigRec = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
            oConfigRec:TipoDFe = 4 //4=MDFe
            oConfigRec:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
            oConfigRec:CertificadoSenha = "12345678"

	      * Criar XML de consulta Recibo do MDFe
		    oConsReciMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.ConsReciMDFe")
			oConsReciMDFe:Versao = "3.00"
			oConsReciMDFe:TpAmb = 2 //Homologação
			oConsReciMDFe:NRec = oAutorizacao:Result:InfRec:NRec          
			
	      * Criar o objeto para consumir o serviço de consulta recibo		
		    oRetAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.MDFe.RetAutorizacao")
			oRetAutorizacao:Executar(oConsReciMDFe, oConfigRec)			
			
			? "XML retornado pela SEFAZ na consulta recibo:"
			?
			? oRetAutorizacao:RetornoWSString
			?
			?
			Wait
			?
			?
			?
			
			If oRetAutorizacao:Result <> NIL
             * Pegar a parte do objeto que tem o protocolo de autorizacao
			   
			   ? "Status da consulta recibo:", oRetAutorizacao:Result:CStat, oRetAutorizacao:Result:XMotivo
			   ?
			   ?
			   Wait
			   ?
			   ?
			   ?
			   
			   oProtMDFe = oRetAutorizacao:Result:GetProtMDFe(0)

			   ? "Status da de autorizacao/rejeicao do MDFe:", oProtMDFe:InfProt:CStat, oProtMDFe:InfProt:XMotivo
			   ?
			   ?
			   Wait
			   ?
			   ?
			   ?
			   
			   If oProtMDFe:InfProt:CStat == 100 //MDFe autorizado
			      oAutorizacao:RetConsReciMDFe = oRetAutorizacao:Result
			      oAutorizacao:GravarXmlDistribuicao("d:\testenfe")			   
			   Endif	  
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