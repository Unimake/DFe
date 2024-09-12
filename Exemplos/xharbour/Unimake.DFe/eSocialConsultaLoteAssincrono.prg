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
   Local oEvento, nrRecibo, Hash

 * Criar o objeto de configuração mínima
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 12 //12=eSocial
   oConfiguracao:CertificadoArquivo = "C:\Users\Wandrey\Downloads\Telegram Desktop\CERT_DIG_AGAPE_MEDICINA_DO_TRABALHO_LTDA_15527739000123_1702988563264876200.pfx"
   oConfiguracao:CertificadoSenha = "1234"
   oConfiguracao:Servico = 70 //Servico.ESocialConsultaEvts
   oConfiguracao:TipoAmbiente = 1 //TipoAmbiente.Producao

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
   
 * Criar o XML de ConsultaLoteEventos
   oConsultarLoteEventos = CreateObject("Unimake.Business.DFe.Xml.ESocial.ConsultarLoteEventos")
   oConsultarLoteEventos:Versao = "1.0.0"
   
   oConsultaLoteEventos = CreateObject("Unimake.Business.DFe.Xml.ESocial.ConsultaLoteEventos")
   oConsultaLoteEventos:ProtocoloEnvio = "1.1.202409.0000000010584057648"
  
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
			   Wait
			Endif						
		 Next i
		 Cls
      Else
	     ? "Consultar novamente, lote não foi processado, ainda."
		 ?
		 ?
		 Wait
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