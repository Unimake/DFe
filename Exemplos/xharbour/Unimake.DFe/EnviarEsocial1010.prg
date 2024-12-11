* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 1010
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial1010()
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
      oESocialEnvioLoteEventos:VersaoSchema = "v_S_01_03_00" // ou "v_S_01_02_00"

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
      oEventoESocial:Id := "ID1219984720000002024091914425200001"
      
      oESocial1010 := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocial1010")
	 
      oEvtTabRubrica := CreateObject("Unimake.Business.DFe.Xml.ESocial.EvtTabRubrica")
      oEvtTabRubrica:Id := "ID1219984720000002024091914425200001"
	  
	  oIdeEvento := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEvento1010")
	  oIdeEvento:TpAmb   := 2 //TipoAmbiente.Homologacao
	  oIdeEvento:ProcEmi := 1 //ProcEmiESocial.AppDoEmpregador 
	  oIdeEvento:VerProc := "1.0"
	  oEvtTabRubrica:IdeEvento := oIdeEvento
	  
	  oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
      oIdeEmpregador:tpInsc := 1 //TiposInscricao.CNPJ
      oIdeEmpregador:nrInsc := "21998472"
      oEvtTabRubrica:IdeEmpregador := oIdeEmpregador

	  oInfoRubrica := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoRubrica")
	  
	  oInclusao := CreateObject("Unimake.Business.DFe.Xml.ESocial.Inclusao")
	  
	  oIdeRubrica := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeRubrica")
	  oIdeRubrica:CodRubr := "554"
	  oIdeRubrica:ideTabRubr := "UNICA"
	  oIdeRubrica:iniValidField := "2024-05"
	  oInclusao:IdeRubrica := oIdeRubrica
	  
	  oDadosRubrica := CreateObject("Unimake.Business.DFe.Xml.ESocial.DadosRubrica")
	  oDadosRubrica:dscRubr := "MENSALIDADE SINDICAL"
	  oDadosRubrica:natRubr := 9231
	  oDadosRubrica:tpRubr := 2 //TipoRubrica.Desconto
	  oDadosRubrica:codIncCP := 21 //CodigoIncidenciaTributaria.MaternidadePagoEmpregador
	  oDadosRubrica:codIncIRRF := 11
	  oDadosRubrica:codIncFGTS := 00 //CodIncFGTS.NaoBaseCalculoFGTS
	  oDadosRubrica:observacao := "Inclusao do codigo incidencia"
	  oInclusao:DadosRubrica := oDadosRubrica
	  
	  oInfoRubrica:Inclusao := oInclusao
	  
	  oEvtTabRubrica:InfoRubrica := oInfoRubrica

      oESocial1010:EvtTabRubrica := oEvtTabRubrica
      
      oEventoESocial:ESocial1010 := oESocial1010
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
      hb_MemoWrit("D:\testenfe\esocial\tox-xmlloteeventos1010assinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos-1010-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)
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

