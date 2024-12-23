* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 1000
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial1000()
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
      oESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")     
      oESocial:VersaoSchema = "v_S_01_03_00" // ou "v_S_01_02_00"

      oEnvioLoteEventos := CreateObject("Unimake.Business.DFe.Xml.ESocial.EnvioLoteEventosESocial")
      oEnvioLoteEventos:Grupo := "1"
      
      oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
      oIdeEmpregador:tpInsc := 1 //TiposInscricao.CNPJ
      oIdeEmpregador:nrInsc := "11111111"
      oEnvioLoteEventos:IdeEmpregador := oIdeEmpregador
      
      oIdeTransmissor := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeTransmissor")
      oIdeTransmissor:tpInsc := 1 //TiposInscricao.CNPJ
      oIdeTransmissor:nrInsc := "11111111111111"
      oEnvioLoteEventos:IdeTransmissor := oIdeTransmissor
      
      oEventos := CreateObject("Unimake.Business.DFe.Xml.ESocial.EventosESocial")
      
      oEvento := CreateObject("Unimake.Business.DFe.Xml.ESocial.EventoESocial")
      oEvento:Id := "ID1000000000000002017102608080800001"
      
      oESocial1000 := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocial1000")
	  oESocial1000:VersaoSchema = "v_S_01_03_00"
	 
      oEvtInfoEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.EvtInfoEmpregador")
      oEvtInfoEmpregador:Id := "ID1000000000000002017102608080800001"
	  
	  oIdeEvento := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEvento1000")
	  oIdeEvento:TpAmb   := 1 //TipoAmbiente.Homologacao
	  oIdeEvento:ProcEmi := 1 //ProcEmiESocial.AppDoEmpregador 
	  oIdeEvento:VerProc := "1.0"
	  oEvtInfoEmpregador:IdeEvento := oIdeEvento

      oEvtInfoEmpregador:IdeEmpregador := oIdeEmpregador

	  oInfoEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoEmpregador")
	  
	  oInclusao := CreateObject("Unimake.Business.DFe.Xml.ESocial.InclusaoE1000")
	  
	  oIdePeriodo := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdePeriodo")
	  oIdePeriodo:IniValidField := "2017-10"
	  oIdePeriodo:FimValidField := "2017-10"
	  oInclusao:IdePeriodo := oIdePeriodo
	  
	  oInfoCadastro := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoCadastro")
	  oInfoCadastro:ClassTrib := 1
	  oInfoCadastro:IndCoop := 1
	  oInfoCadastro:IndConstr := 1
	  oInfoCadastro:indDesFolha := 1
	  oInfoCadastro:IndOptRegEletron := 1
	  
	  oDadosIsencao := CreateObject("Unimake.Business.DFe.Xml.ESocial.DadosIsencao")
	  oDadosIsencao:IdeMinLei := "str1234"
      oDadosIsencao:nrCertif := "str1234"
	  oDadosIsencao:dtEmisCertifField := "2200-12-13"
	  oDadosIsencao:dtVencCertifField := "2012-12-13"
	  oDadosIsencao:nrProtRenov := "str1234"
	  oDadosIsencao:dtProtRenovField := "2012-12-13"
	  oDadosIsencao:dtDouField := "2012-12-13"
	  oDadosIsencao:pagDou := "745"	  
	  oInfoCadastro:DadosIsencao = oDadosIsencao
	  
	  oInfoOrgInternacional := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoOrgInternacional")
	  oInfoOrgInternacional:IndAcordoIsenMulta = 1	  
	  oInfoCadastro:InfoOrgInternacional := oInfoOrgInternacional
	  oInclusao:InfoCadastro := oInfoCadastro
	  
	  oInfoEmpregador:Inclusao := oInclusao
	  
	  oEvtInfoEmpregador:InfoEmpregador := oInfoEmpregador

      oESocial1000:EvtInfoEmpregador := oEvtInfoEmpregador
      
      oEvento:ESocial1000 := oESocial1000
      oEventos:AddEvento(oEvento)
      oEnvioLoteEventos:Eventos := oEventos
      oESocial:EnvioLoteEventos := oEnvioLoteEventos

    * Criar objeto para consumir o serviço de envio de lotes de eventos do eSocial
      oEnviarLoteEventosESocial := CreateObject("Unimake.Business.DFe.Servicos.ESocial.EnviarLoteEventosESocial")
	
    * Consumir o serviço	
      oEnviarLoteEventosESocial:Executar(oESocial, oConfiguracao)
      
      stringXMLLoteAssinado := oEnviarLoteEventosESocial:GetConteudoXMLAssinado()
      
      ? "StringXMLAssinado:", stringXMLLoteAssinado
      ?
      ?
      Wait
      hb_MemoWrit("D:\testenfe\esocial\evtInfoEmpregador-xmlloteeventosassinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      hb_MemoWrit("D:\testenfe\esocial\evtInfoEmpregador-xmlloteeventos-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)
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

