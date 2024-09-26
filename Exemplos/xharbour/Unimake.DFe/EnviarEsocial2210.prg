* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 2210
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial2210()
   Local oConfiguracao, oExceptionInterop, oErro
   Local nomeArqLoteEvento, stringXMLLoteAssinado
   Local oESocialEnvioLoteEventos
   Local oEnviarLoteEventosESocial

 * Criar o objeto de configuração mínima
   oConfiguracao:= CreateObject("Unimake.Business.DFe.Servicos.Configuracao")

   oConfiguracao:TipoDFe            := 12 // 12 = eSocial
   oConfiguracao:Servico            := 69 // Servico.ESocialEnviarLoteEventos
   oConfiguracao:CertificadoArquivo := "D:\projetos\Unimake_PV.pfx"
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
	  oEventoESocial:Id = "ID1230985630000002024090421022000001"
	  
	  oESocial2210 := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocial2210")
	 
      oEvtCAT := CreateObject("Unimake.Business.DFe.Xml.ESocial.EvtCAT")
	  oEvtCAT:Id = "ID1230985630000002024091310242800001"
	  
      oIdeEvento2210 := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEvento2210")
      oIdeEvento2210:IndRetif := 1 //IndicativoRetificacao.ArquivoOriginal
      oIdeEvento2210:TpAmb := 2 //TipoAmbiente.Homologacao
      oIdeEvento2210:ProcEmi := 1 //ProcEmiESocial.AppDoEmpregador
      oIdeEvento2210:VerProc := "SGOWIN_Versao24091"
	  oEvtCAT:IdeEvento := oIdeEvento2210

      oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
	  oIdeEmpregador:tpInsc := 1 //TiposInscricao.CNPJ
	  oIdeEmpregador:nrInsc := "23098563"
	  oEvtCAT:IdeEmpregador := oIdeEmpregador
	  
	  oIdeVinculo2210 := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeVinculo2210")
      oIdeVinculo2210:cpfTrab := "11111111111"
	  oIdeVinculo2210:matricula := "1111111111"
	  oEvtCAT:IdeVinculo := oIdeVinculo2210
	  
	  oCat := CreateObject("Unimake.Business.DFe.Xml.ESocial.Cat")
	  oCat:dtAcidField := "2024-09-03"
	  oCat:tpAcid := 3 //TipoAcidenteTrabalho.Trajeto
      oCat:hrAcid := "0810"
      oCat:hrsTrabAntesAcid := "0000"
      oCat:tpCat := 1 //TipoDeCat.Inicial
      oCat:indCatObito := 0 //SimNaoLetra.Nao
      oCat:indComunPolicia := 0 //SimNaoLetra.Nao
      oCat:codSitGeradora := "200012500"
      oCat:iniciatCAT := 1 //IniciativaDaCAT.Empregador
      oCat:obsCAT := "Nil"
      oCat:ultDiaTrabField := "2024-09-03"
      oCat:houveAfast := 1 //SimNaoLetra.Sim
	  
	  oLocalAcidente := CreateObject("Unimake.Business.DFe.Xml.ESocial.LocalAcidente")
      oLocalAcidente:tpLocal := 1 //TipoLocalAcidente.EstabelecimentoBrasil
      oLocalAcidente:dscLocal := "Nil"
      oLocalAcidente:tpLograd := "R"
      oLocalAcidente:dscLograd := "Rua Doutor Joaquim de Abreu Sampaio Vidal"
      oLocalAcidente:nrLograd := "Nil"
      oLocalAcidente:complemento := "de 402/403 ao fim"
      oLocalAcidente:bairro := "Alto Cafezal"
      oLocalAcidente:cep := "17504072"
      oLocalAcidente:codMunic := "3529005"
      oLocalAcidente:uf := 35 //UFBrasil.SP
	  
	  oIdeLocalAcid := CreateObject("Unimake.Business.DFe.Xml.ESocial.ideLocalAcid")
	  oIdeLocalAcid:tpInsc := 1 //TipoInscricaoEstabelecimento.CNPJ
	  oIdeLocalAcid:nrInsc := "23098563000160"
	  oLocalAcidente:IdeLocalAcid := oIdeLocalAcid
	  
	  oCat:LocalAcidente := oLocalAcidente
	  
	  oParteAtingida := CreateObject("Unimake.Business.DFe.Xml.ESocial.ParteAtingida")
	  oParteAtingida:codParteAting := "753510000"
	  oParteAtingida:lateralidade := 0 //Lateralidade.NaoAplicavel
	  oCat:ParteAtingida := oParteAtingida
	  
	  oAgenteCausador := CreateObject("Unimake.Business.DFe.Xml.ESocial.AgenteCausador")
	  oAgenteCausador:codAgntCausador := "303075900"
	  oCat:AgenteCausador := oAgenteCausador
	  
	  oAtestado := CreateObject("Unimake.Business.DFe.Xml.ESocial.Atestado")
      oAtestado:dtAtendimentoField := "2024-09-03"
      oAtestado:hrAtendimento := "0900"
      oAtestado:indInternacao := 1 //SimNaoLetra.Sim
      oAtestado:durTrat := "14"
      oAtestado:indAfast := 1 //SimNaoLetra.Sim
      oAtestado:dscLesao := "702035000"
      oAtestado:codCID := "S52"
	  
	  oEmitente := CreateObject("Unimake.Business.DFe.Xml.ESocial.Emitente")
	  oEmitente:nmEmit := "Doutor Fulano"
	  oEmitente:ideOC := 1 //OrgaoDeClasseMedica.CRM
	  oEmitente:nrOC := "60678"
	  oEmitente:ufOC := 35 //UFBrasil.SP
	  oAtestado:Emitente := oEmitente
	  
	  oCat:Atestado := oAtestado
	  
	  oEvtCAT:Cat := oCat

	  oESocial2210:EvtCAT := oEvtCAT
	  
	  oEventoESocial:ESocial2210 := oESocial2210
	  
	  oEventosESocial:AddEvento(oEventoESocial)
	  
	  oEnvioLoteEventosESocial:Eventos := oEventosESocial
	  
	  oESocialEnvioLoteEventos:EnvioLoteEventos := oEnvioLoteEventosESocial
	  

    * Criar objeto para consumir o serviço de envio de lotes de eventos do eSocial
      oEnviarLoteEventosESocial := CreateObject("Unimake.Business.DFe.Servicos.ESocial.EnviarLoteEventosESocial")
	
    * Consumir o serviço	
      oEnviarLoteEventosESocial:Executar(oESocialEnvioLoteEventos, oConfiguracao)
	  
	  stringXMLLoteAssinado = oEnviarLoteEventosESocial:GetConteudoXMLAssinado()
	  
	  ? "StringXMLAssinado:", stringXMLLoteAssinado
	  ?
	  ?
	  Wait
      hb_MemoWrit("d:\testenfe\esocial\xmlloteeventosassinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
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

