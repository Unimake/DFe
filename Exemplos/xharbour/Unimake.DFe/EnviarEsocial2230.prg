* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 2230
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial2230()
   Local oConfiguracao, oExceptionInterop, oErro
   Local nomeArqLoteEvento, stringXMLLoteAssinado
   Local oESocialEnvioLoteEventos
   Local oEnviarLoteEventosESocial

 * Criar o objeto de configuração mínima
   oConfiguracao := CreateObject("Unimake.Business.DFe.Servicos.Configuracao")

   oConfiguracao:TipoDFe            := 12 // 12 = eSocial
   oConfiguracao:Servico            := 69 // Servico.ESocialEnviarLoteEventos
   oConfiguracao:CertificadoArquivo := "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha   := "12345678"
   oConfiguracao:TipoAmbiente       := 2  // TipoAmbiente.Homologação

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop:= CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try         
    * Montando o XML do lote de eventos
      oESocialEnvioLoteEventos := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")      

      oEnvioLoteEventosESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EnvioLoteEventosESocial")
      oEnvioLoteEventosESocial:Grupo := "1"
      
      oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
      oIdeEmpregador:tpInsc := 1 //TiposInscricao.CNPJ
      oIdeEmpregador:nrInsc := "11111111"
      oEnvioLoteEventosESocial:IdeEmpregador := oIdeEmpregador
      
      oIdeTransmissor:= CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeTransmissor")
      oIdeTransmissor:tpInsc := 1 //TiposInscricao.CNPJ
      oIdeTransmissor:nrInsc := "11111111111111"
      oEnvioLoteEventosESocial:IdeTransmissor := oIdeTransmissor

      oEventosESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EventosESocial")
      
      oEventoESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EventoESocial")
      oEventoESocial:Id := "ID1111111111111111111111111111111111"
      
      oESocial2230 := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocial2230")
	 
      oEvtAfastTemp := CreateObject("Unimake.Business.DFe.Xml.ESocial.EvtAfastTemp")
      oEvtAfastTemp:Id := "ID1111111111111111111111111111111111"
      
      oIdeEvento := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEvento2230")
      oIdeEvento:IndRetif := 1 // IndicativoRetificacao.ArquivoOriginal
      oIdeEvento:TpAmb    := 2 // TipoAmbiente.Homologacao
      oIdeEvento:ProcEmi  := 1 // ProcEmiESocial.AppDoEmpregador 
      oIdeEvento:VerProc  := "str1234"
      oEvtAfastTemp:IdeEvento  := oIdeEvento
      
      oEvtAfastTemp:IdeEmpregador := oIdeEmpregador
	  
	  oIdeVinculo := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeVinculo2230")
      oIdeVinculo:CpfTrab     := "11111111111"
      oIdeVinculo:Matricula   := "000111"
      oEvtAfastTemp:IdeVinculo := oIdeVinculo
	  
	  oInfoAfastamento := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoAfastamento")
	  
	  oIniAfastamento := CreateObject("Unimake.Business.DFe.Xml.ESocial.IniAfastamento")
	  oIniAfastamento:DtIniAfastField := "2024-07-11"
	  oIniAfastamento:CodMotAfast := "01"
	  oInfoAfastamento:IniAfastamento := oIniAfastamento
	  
	  oEvtAfastTemp:InfoAfastamento := oInfoAfastamento	  	  	  
     	  
      oESocial2230:EvtAfastTemp := oEvtAfastTemp

      oEventoESocial:ESocial2230 := oESocial2230
      oEventosESocial:AddEvento(oEventoESocial)
      oEnvioLoteEventosESocial:Eventos:= oEventosESocial
      oESocialEnvioLoteEventos:EnvioLoteEventos:= oEnvioLoteEventosESocial
	  

 	* Aqui vou abrir um LOOP para ver todos os eventos que estão no lote e pegar o tipo de cada um deles.
	  ?
	  ?
	  FOR i := 1 TO oESocialEnvioLoteEventos:EnvioLoteEventos:Eventos:GetEventoCount
          evento := oESocialEnvioLoteEventos:EnvioLoteEventos:Eventos:GetEvento(i-1)
          tipoEvento := evento:TipoEvento
		  
		  ? "Tipo do evento:", tipoEvento
	  NEXT i
	  ?
	  ?
	  Wait

    * comum daqui para baixo
    * Criar objeto para consumir o serviço de envio de lotes de eventos do eSocial
      oEnviarLoteEventosESocial:= CreateObject("Unimake.Business.DFe.Servicos.ESocial.EnviarLoteEventosESocial")
	
    * Consumir o serviço	
      oEnviarLoteEventosESocial:Executar(oESocialEnvioLoteEventos, oConfiguracao)
	  
      stringXMLLoteAssinado:= oEnviarLoteEventosESocial:GetConteudoXMLAssinado()
      
      ? "StringXMLAssinado:", stringXMLLoteAssinado
      ?
      ?
      Wait
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos2230assinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos-2230-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)
	  
      ? "CdResposta:", oEnviarLoteEventosESocial:Result:RetornoEnvioLoteEventos:Status:CdResposta
	  ? "DescResposta:", oEnviarLoteEventosESocial:Result:RetornoEnvioLoteEventos:Status:DescResposta
	  ?
	  
	  if oEnviarLoteEventosESocial:Result:RetornoEnvioLoteEventos:Status:Ocorrencias != nil
         ? "Ocorrencias:"
         ?

         For x := 1 To oEnviarLoteEventosESocial:Result:RetornoEnvioLoteEventos:Status:Ocorrencias:GetOcorrenciaCount()
            oOcorrencia:= oEnviarLoteEventosESocial:Result:RetornoEnvioLoteEventos:Status:Ocorrencias:GetOcorrencia(x - 1)
		 
		    ? AllTrim(Str(x, 3)) + ")"
		    ? "Tipo:", oOcorrencia:Tipo
		    ? "Codigo:", oOcorrencia:Codigo
		    ? "Descricao:", oOcorrencia:Descricao
		    ? "Localizacao:", oOcorrencia:Localizacao
         Next x
      Endif		 
	  
      Wait
      Cls

   Catch oErro
    * Demonstrar exceções geradas no proprio Harbour, se existir.
      ? "ERRO"
      ? "===="
      ? "Falha ao tentar consultar o status do servico."
      ? oErro:Description
      ? oErro:Operation
    
   * Demonstrar a exceção do CSHARP
      ?
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
      ?     
    
      Wait
      Cls   
   End
Return (Nil)
