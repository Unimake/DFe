* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 1200
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial1200()
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
      
      oESocial1200 := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocial1200")
	 
      oEvtRemun := CreateObject("Unimake.Business.DFe.Xml.ESocial.EvtRemun")
      oEvtRemun:Id := "ID1219984720000002024040318014905925"
	  
	  oIdeEvento := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEvento1200")
	  oIdeEvento:indRetif := 1
	  oIdeEvento:indApuracao := 1
	  oIdeEvento:PerApur := "2024-03"
	  oIdeEvento:TpAmb := 1 //TipoAmbiente.Homologacao
	  oIdeEvento:ProcEmi := 1 //ProcEmiESocial.AppDoEmpregador 
	  oIdeEvento:VerProc := "1.0"
	  oEvtRemun:IdeEvento := oIdeEvento
	  
	  oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
      oIdeEmpregador:tpInsc := 1 //TiposInscricao.CNPJ
      oIdeEmpregador:nrInsc := "11111111"
      oEvtRemun:IdeEmpregador := oIdeEmpregador
	  
	  oIdeTrabalhador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhador")
	  oIdeTrabalhador:CpfTrab := "11111111111"
	  oEvtRemun:IdeTrabalhador := oIdeTrabalhador
	  
	  oDmDev := CreateObject("Unimake.Business.DFe.Xml.ESocial.DmDev")
	  oDmDev:ideDmDev := "P2403"
	  oDmDev:CodCateg := 701
	  
	  oInfoPerApur := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoPerApur")
	  
	  oIdeEstabLot := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEstabLot")
	  oIdeEstabLot:tpInsc := 1
	  oIdeEstabLot:nrInsc := "11111111111111"
	  oIdeEstabLot:codLotacao := "LOTA00015501"
	  
	  oRemunPerApur := CreateObject("Unimake.Business.DFe.Xml.ESocial.RemunPerApur1200")
	  
	  oItensRemun := CreateObject("Unimake.Business.DFe.Xml.ESocial.ItensRemun1200")
	  oItensRemun:codRubr := "363"
	  oItensRemun:ideTabRubr := "UNICA"
	  oItensRemun:qtdRubr := 1.00
	  oItensRemun:fatorRubr := 1.00
	  oItensRemun:vrRubr := 2022.47
	  oItensRemun:indApurIR := 0
	  oRemunPerApur:AddItensRemun1200(oItensRemun)
	  
	  oItensRemun := CreateObject("Unimake.Business.DFe.Xml.ESocial.ItensRemun1200")
	  oItensRemun:codRubr := "542"
	  oItensRemun:ideTabRubr := "UNICA"
	  oItensRemun:qtdRubr := 1.00
	  oItensRemun:fatorRubr := 1.00
	  oItensRemun:vrRubr := 222.47
	  oItensRemun:indApurIR := 0
	  oRemunPerApur:AddItensRemun1200(oItensRemun)
	  
	  oItensRemun := CreateObject("Unimake.Business.DFe.Xml.ESocial.ItensRemun1200")
	  oItensRemun:codRubr := "623"
	  oItensRemun:ideTabRubr := "UNICA"
	  oItensRemun:qtdRubr := 1.00
	  oItensRemun:fatorRubr := 1.00
	  oItensRemun:vrRubr := 1800.00
	  oItensRemun:indApurIR := 0
	  oRemunPerApur:AddItensRemun1200(oItensRemun)
	  
	  oItensRemun := CreateObject("Unimake.Business.DFe.Xml.ESocial.ItensRemun1200")
	  oItensRemun:codRubr := "991"
	  oItensRemun:ideTabRubr := "UNICA"
	  oItensRemun:qtdRubr := 1.00
	  oItensRemun:fatorRubr := 1.00
	  oItensRemun:vrRubr := 1.00
	  oItensRemun:indApurIR := 0
	  oRemunPerApur:AddItensRemun1200(oItensRemun)
	  
	  oIdeEstabLot:AddRemunPerApur(oRemunPerApur)
	  
	  oInfoPerApur:IdeEstabLot := oIdeEstabLot
	  
	  oDmDev:InfoPerApur := oInfoPerApur
	  oEvtRemun:AddDmDev(oDmDev)
	  
      oESocial1200:EvtRemun := oEvtRemun
      	  
      oEventoESocial:ESocial1200 := oESocial1200
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
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos1200assinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos-1200-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)
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

