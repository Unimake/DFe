* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 1210
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial1210()
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
      oEnvioLoteEventosESocial:Grupo := "1"
      
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
      oEventoESocial:Id := "ID1219984720000002024040318014905925"
      
      oESocial1210 := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocial1210")
	 
      oEvtPgtos := CreateObject("Unimake.Business.DFe.Xml.ESocial.EvtPgtos")
      oEvtPgtos:Id := "ID1219984720000002024040318014905925"
	  
	  oIdeEvento := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEvento1210")
	  oIdeEvento:indRetif := 1
	  oIdeEvento:PerApurField := "2024-03"
	  oIdeEvento:TpAmb := 1 //TipoAmbiente.Homologacao
	  oIdeEvento:ProcEmi := 1 //ProcEmiESocial.AppDoEmpregador 
	  oIdeEvento:VerProc := "1.0"
	  oEvtPgtos:IdeEvento := oIdeEvento
	  
      oEvtPgtos:IdeEmpregador := oIdeEmpregador
	  
	  oIdeBenef := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeBenef")
	  oIdeBenef:CpfBenef := "60361011393"
	  
	  oInfoPgto := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoPgto")
	  oInfoPgto:dtPgtoField := "2024-08-05"
	  oInfoPgto:tpPgto := 1 
	  oInfoPgto:perRefField := "2024-08"
	  oInfoPgto:ideDmDev := "P2408" 
	  oInfoPgto:vrLiq := 1530.00
	  oIdeBenef:AddInfoPgto(oInfoPgto)
	  
	  oInfoPgto := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoPgto")
	  oInfoPgto:dtPgtoField := "2024-08-20"
	  oInfoPgto:tpPgto := 1 
	  oInfoPgto:perRefField := "2024-08"
	  oInfoPgto:ideDmDev := "A2408" 
	  oInfoPgto:vrLiq := 920.00
	  oIdeBenef:AddInfoPgto(oInfoPgto)
	  
	  oEvtPgtos:IdeBenef := oIdeBenef
	  
	  oESocial1210:EvtPgtos := oEvtPgtos	  
      	  
      oEventoESocial:ESocial1210 := oESocial1210
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
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos1210assinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos-1210-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)
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

