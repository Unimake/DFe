* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 2220
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial2220()
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
   oExceptionInterop := CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try         
    * Montando o XML do lote de eventos
      oESocialEnvioLoteEventos := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")      

      oEnvioLoteEventosESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EnvioLoteEventosESocial")
      oEnvioLoteEventosESocial:Grupo := "2"
      
      oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
      oIdeEmpregador:tpInsc := 1 //TiposInscricao.CNPJ
      oIdeEmpregador:nrInsc := "11111111"
      oEnvioLoteEventosESocial:IdeEmpregador := oIdeEmpregador
      
      oIdeTransmissor := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeTransmissor")
      oIdeTransmissor:tpInsc := 1 //TiposInscricao.CNPJ
      oIdeTransmissor:nrInsc := "11111111111111"
      oEnvioLoteEventosESocial:IdeTransmissor := oIdeTransmissor
      
      oEventosESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EventosESocial")
      
      oEventoESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EventoESocial")
      oEventoESocial:Id := "ID1230985630000002024090421022000002"
      
      oESocial2220 := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocial2220")
	 
      oEvtMonit := CreateObject("Unimake.Business.DFe.Xml.ESocial.EvtMonit")
      oEvtMonit:Id := "ID1230985630000002024090421022000002"
	  
	  oIdeEvento2220 := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEvento2220")
	  oIdeEvento2220:IndRetif := 1 //IndicativoRetificacao.ArquivoOriginal
	  oIdeEvento2220:TpAmb   := 2 //TipoAmbiente.Homologacao
	  oIdeEvento2220:ProcEmi := 1 //ProcEmiESocial.AppDoEmpregador 
	  oIdeEvento2220:VerProc := "SGOWIN_Versao24091"
	  oEvtMonit:IdeEvento := oIdeEvento2220
	  
      oEvtMonit:IdeEmpregador := oIdeEmpregador
	  
	  oIdeVinculo := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeVinculo")
	  oIdeVinculo:CpfTrab := "11111111111"
	  oIdeVinculo:Matricula := "73"
	  oEvtMonit:IdeVinculo := oIdeVinculo
	  
	  oExMedOcup := CreateObject("Unimake.Business.DFe.Xml.ESocial.ExMedOcup")
	  oExMedOcup:TpExameOcup := 9 //TpExameOcup.ExameDemissional
	  
	  oAso := CreateObject("Unimake.Business.DFe.Xml.ESocial.Aso")
	  oAso:DtAsoField := "2024-08-30"
	  oAso:ResAso := 1 //ResAso.Apto
      
      * Primeiro Exame
	  
	  oExame := CreateObject("Unimake.Business.DFe.Xml.ESocial.Exame")
	  oExame:DtExmField := "2024-08-30"
	  oExame:procRealizado := "0295"
	  oExame:indResult := 1 //IndResult.Normal
	  oAso:AddExame(oExame)

      * Segundo exame
      oExame:= CreateObject("Unimake.Business.DFe.Xml.ESocial.Exame")
      oExame:DtExmField   := "2024-08-30"
      oExame:procRealizado:= "0704"
      oExame:indResult    := 1 // IndResult.Normal
      oAso:AddExame(oExame)

      * Terceiro exame
      oExame:= CreateObject("Unimake.Business.DFe.Xml.ESocial.Exame")
      oExame:DtExmField   := "2024-08-30"
      oExame:procRealizado:= "0705"
      oExame:indResult    := 1 // IndResult.Normal
      oAso:AddExame(oExame)
                                            
      * Quarto exame
      oExame:DtExmField   := "2024-08-30"
      oExame:procRealizado:= "0733"
      oExame:indResult    := 1 // IndResult.Normal
      oAso:AddExame(oExame)
                                           
      * Quinto exame
      oExame:= CreateObject("Unimake.Business.DFe.Xml.ESocial.Exame")
      oExame:DtExmField   := "2024-08-30"
      oExame:procRealizado:= "0234"
      oExame:indResult    := 1 // IndResult.Normal
      oAso:AddExame(oExame)
	  
	  oMedico := CreateObject("Unimake.Business.DFe.Xml.ESocial.Medico")
	  oMedico:nmMed := "Fulana de Tal"
	  oMedico:nrCRM := "654321"
	  oMedico:ufCRM := 35 //UFBrasil.SP
	  oAso:Medico := oMedico
	  oExMedOcup:Aso := oAso
	  
	  oRespMonit := CreateObject("Unimake.Business.DFe.Xml.ESocial.RespMonit") 
	  oRespMonit:nmResp := "Dr. Medico da Silva"
	  oRespMonit:nrCRM := "123456"
	  oRespMonit:ufCRM := 35 //UFBrasil.SP
	  oExMedOcup:RespMonit := oRespMonit
	  
	  oEvtMonit:ExMedOcup := oExMedOcup
	  
      oESocial2220:EvtMonit := oEvtMonit
      
      oEventoESocial:ESocial2220 := oESocial2220
      oEventosESocial:AddEvento(oEventoESocial)

      * outro evento   
      oEventoESocial:= CreateObject("Unimake.Business.DFe.Xml.ESocial.EventoESocial")
      oEventoESocial:Id:= "ID1230985630000002024091310242800002"
      
      oESocial2220:= CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocial2220")

      oEvtMonit:= CreateObject("Unimake.Business.DFe.Xml.ESocial.EvtMonit")
      oEvtMonit:Id:= "ID1230985630000002024091310242800002"
      
      oEvtMonit:IdeEvento    := oIdeEvento2220
      
      oEvtMonit:IdeEmpregador:= oIdeEmpregador
      
      oIdeVinculo:= CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeVinculo")
      oIdeVinculo:CpfTrab  := "22321798858"
      oIdeVinculo:Matricula:= "66"
      oEvtMonit:IdeVinculo := oIdeVinculo
      
      oExMedOcup:= CreateObject("Unimake.Business.DFe.Xml.ESocial.ExMedOcup")
      oExMedOcup:TpExameOcup:= 1 //TpExameOcup.ExamePeriodico
      
      oAso:= CreateObject("Unimake.Business.DFe.Xml.ESocial.Aso")
      oAso:DtAsoField:= "2024-08-30"
      oAso:ResAso    := 1 //ResAso.Apto
      
      oExame:= CreateObject("Unimake.Business.DFe.Xml.ESocial.Exame")
      oExame:DtExmField   := "2024-08-30"
      oExame:procRealizado:= "0295"
      oExame:indResult    := 1 // IndResult.Normal
      oAso:AddExame(oExame)
    
      oMedico:= CreateObject("Unimake.Business.DFe.Xml.ESocial.Medico")
      oMedico:nmMed := "Fteste"
      oMedico:nrCRM := "654321"
      oMedico:ufCRM := 35 // UFBrasil.SP
      oAso:Medico   := oMedico
      oExMedOcup:Aso:= oAso
      
      oExMedOcup:RespMonit:= oRespMonit
      
      oEvtMonit:ExMedOcup:= oExMedOcup
      
      oESocial2220:EvtMonit:= oEvtMonit

      **** comum daqui para baixo   
      oEventoESocial:ESocial2220:= oESocial2220
      oEventosESocial:AddEvento(oEventoESocial)
      oEnvioLoteEventosESocial:Eventos:= oEventosESocial
      oESocialEnvioLoteEventos:EnvioLoteEventos:= oEnvioLoteEventosESocial

    * Criar objeto para consumir o serviço de envio de lotes de eventos do eSocial
      oEnviarLoteEventosESocial:= CreateObject("Unimake.Business.DFe.Servicos.ESocial.EnviarLoteEventosESocial")
	
    * Consumir o serviço	
      oEnviarLoteEventosESocial:Executar(oESocialEnvioLoteEventos, oConfiguracao)
      
      stringXMLLoteAssinado:= oEnviarLoteEventosESocial:GetConteudoXMLAssinado()
      
      ? "StringXMLAssinado:", stringXMLLoteAssinado
      ?
      ?
      Wait
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos2220assinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos-2220-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)
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

Return

