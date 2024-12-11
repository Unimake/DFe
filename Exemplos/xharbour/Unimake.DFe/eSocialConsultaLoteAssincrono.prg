* ---------------------------------------------------------------------------------
* eSocial - Consultar lote assincrono
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function eSocialConsultaLoteAssincrono()
   Local oExceptionInterop, oErro
   Local oConfiguracao
   Local oConsultarLoteEventos
   Local oConsultaLoteAssincrono
   Local nStatusResposta, nHandle, cArquivo
   Local oEvento, nrRecibo, Hash, i, x

 * Criar o objeto de configuração mínima
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 12 //12=eSocial
   oConfiguracao:TipoEmissao = 1 //1=Normal
   oConfiguracao:CertificadoArquivo = "D:\projetos\certificados\DosClientes\esocial.pfx"
   oConfiguracao:CertificadoSenha = "1234"
   oConfiguracao:Servico = 70 //Servico.ESocialConsultaEvts
   oConfiguracao:TipoAmbiente = 1 //TipoAmbiente.Producao

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
   
 * Criar o XML de ConsultaLoteEventos
   oConsultarLoteEventos = CreateObject("Unimake.Business.DFe.Xml.ESocial.ConsultarLoteEventos")
   oConsultarLoteEventos:Versao = "1.0.0"
   
   oConsultaLoteEventos = CreateObject("Unimake.Business.DFe.Xml.ESocial.ConsultaLoteEventos")
   oConsultaLoteEventos:ProtocoloEnvio = "1.1.202410.0000000010705741120"
  
   oConsultarLoteEventos:ConsultaLoteEventos = oConsultaLoteEventos   
  
   Try        
    * Enviar a consulta e pegar o retorno
	  oConsultaLoteAssincrono := CreateObject("Unimake.Business.DFe.Servicos.ESocial.ConsultaLoteAssincrono")
      oConsultaLoteAssincrono:Executar(oConsultarLoteEventos, oConfiguracao)

    * Gravar o XML retornado no HD
	  cArquivo := "d:\testenfe\" + oConsultaLoteEventos:ProtocoloEnvio + "-eSocial-ret.xml"
      nHandle := fCreate(cArquivo)
 	  FWrite(nHandle, oConsultaLoteAssincrono:RetornoWSString)
	  FClose(nHandle)

    * Demonstrar na tela o XML retornado	  
	  ? oConsultaLoteAssincrono:RetornoWSString
	  ?
	  ?
	  Wait	  
	  
	  nStatusResposta := oConsultaLoteAssincrono:Result:RetornoProcessamentoLoteEventos:Status:CdResposta
      ? "StatusRespostaLote: " + Trim(Str(nStatusResposta,3))	  	  	  	  
	  
	  If nStatusResposta == 201		 
	   * Tem que passar para a consulta o XML do lote de eventos enviados para que ela tenha os eventos para montar o XML de distribuição de cada um deles.
	     if File("d:\testenfe\monit_teste-esocial-loteevt.xml")
            oESocialEnvioLoteEventos = CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")
   	        oESocialEnvioLoteEventos = oESocialEnvioLoteEventos:LoadFromFile("d:\testenfe\monit_teste-esocial-loteevt.xml")
	        oConsultaLoteAssincrono:ESocialEnvioLoteEventos = oESocialEnvioLoteEventos
			?
			? "Oi"
			?
			?
			Wait
	     Endif		

	     For i = 1 To oConsultaLoteAssincrono:Result:RetornoProcessamentoLoteEventos:RetornoEventos:GetEventoCount()
		    oEvento = oConsultaLoteAssincrono:Result:RetornoProcessamentoLoteEventos:RetornoEventos:GetEvento(i-1)
			
			? "ID Evento:", oEvento:ID
			
			If oEvento:RetornoEvento:eSocial:RetornoEvento:Processamento:CdResposta == 201 //Sucesso
			   nrRecibo := oEvento:RetornoEvento:eSocial:RetornoEvento:Recibo:NRRecibo
			   Hash := oEvento:RetornoEvento:eSocial:RetornoEvento:Recibo:Hash
			   
			   ? "Recibo autorizacao:", nrRecibo
			   ? "Hash assinatura evento:", Hash
			   ?
			   ?
			   ?			   			   

   			   oConsultaLoteAssincrono:GravarXmlDistribuicao("D:\testenfe\esocial\Enviado\Autorizados", oEvento:ID)
			   
			   Wait
			Else
   			   For x = 1 To oEvento:RetornoEvento:eSocial:RetornoEvento:Processamento:Ocorrencias:GetOcorrenciaCount()
			       oOcorrencia = oEvento:RetornoEvento:eSocial:RetornoEvento:Processamento:Ocorrencias:GetOcorrencia(x-1)
				   
				   ? "<tipo>", oOcorrencia:Tipo
				   ? "<codigo>", oOcorrencia:codigo
				   ? "<descricao>", oOcorrencia:descricao
				   ? "<localizacao>", oOcorrencia:localizacao
				   ?
				   ?
				   ?
    			   Wait
			   Next x
			Endif						
		 Next i		 
		 Cls
		 
      Else //Aconteceu algum erro no Retorno
		 hb_MemoWrit("d:\testenfe\" + oConsultaLoteEventos:ProtocoloEnvio + "-eSocial-erro-ret.xml", oConsultaLoteAssincrono:RetornoWSString)
         DO CASE
			CASE oConsultaLoteAssincrono:Result:RetornoProcessamentoLoteEventos:Status:CdResposta == 101
				 ? "Aguarde alguns minutos e tente novamente!"
				 ?
				 ?
  		         Wait
				 
			CASE oConsultaLoteAssincrono:Result:RetornoProcessamentoLoteEventos:Status:CdResposta == 301
				 ? "Erro no servidor do eSocial. Aguarde alguns minutos e envie novamente!"
				 ?
				 ?
  		         Wait
				 
			CASE oConsultaLoteAssincrono:Result:RetornoProcessamentoLoteEventos:Status:CdResposta >= 401 .AND. oConsultaLoteAssincrono:Result:RetornoProcessamentoLoteEventos:Status:CdResposta <= 411
				 ? "Codigo: " + hb_Ntos(oConsultaLoteAssincrono:Result:RetornoProcessamentoLoteEventos:Status:CdResposta)
				 ? "Descricao: " + oConsultaLoteAssincrono:Result:RetornoProcessamentoLoteEventos:Status:descResposta
				 ?
				 ?
  		         Wait
				 
			CASE oConsultaLoteAssincrono:Result:RetornoProcessamentoLoteEventos:Status:CdResposta >= 501 .AND. oConsultaLoteAssincrono:Result:RetornoProcessamentoLoteEventos:Status:CdResposta <= 505
   			     For x = 1 To oConsultaLoteAssincrono:Result:RetornoProcessamentoLoteEventos:Status:Ocorrencias:GetOcorrenciaCount()
			         oOcorrencia = oConsultaLoteAssincrono:Result:RetornoProcessamentoLoteEventos:Status:Ocorrencias:GetOcorrencia(x-1)
				   
  				     ? "<tipo>", oOcorrencia:Tipo
				     ? "<codigo>", oOcorrencia:codigo
				     ? "<descricao>", oOcorrencia:descricao
				     ? "<localizacao>", oOcorrencia:localizacao
				     ?
				     ?
				     ?
    			     Wait
			     Next x
				 
			OTHERWISE
				 ? "Erro nao catalogado!!"
				 ?
				 ?
  		         Wait
         ENDCASE
	  
		 Cls
	  Endif	  
   
   Catch oErro
      //Demonstrar exceções geradas no proprio Harbour, se existir.
	  ? "ERRO"
	  ? "===="
	  ? "Falha ao tentar enviar o XML"
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