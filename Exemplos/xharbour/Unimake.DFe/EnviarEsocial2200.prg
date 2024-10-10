* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 2200
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial2200()
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
	  oEventoESocial:Id = "ID1111111111111111111111111111111111"
	  
	  oESocial2200 := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocial2200")
	 
      oEvtAdmissao := CreateObject("Unimake.Business.DFe.Xml.ESocial.EvtAdmissao")
	  oEvtAdmissao:Id = "ID1111111111111111111111111111111111"
	  
      oIdeEvento := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEvento2200")
      oIdeEvento:IndRetif := 1 //IndicativoRetificacao.ArquivoOriginal
      oIdeEvento:TpAmb := 1 //TipoAmbiente.Producao
      oIdeEvento:ProcEmi := 1 //ProcEmiESocial.AppDoEmpregador
      oIdeEvento:VerProc := "1.0"
	  oEvtAdmissao:IdeEvento := oIdeEvento

      oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
	  oIdeEmpregador:tpInsc := 1 //TiposInscricao.CNPJ
	  oIdeEmpregador:nrInsc := "11111111"
	  oEvtAdmissao:IdeEmpregador := oIdeEmpregador
	  
	  oTrabalhador := CreateObject("Unimake.Business.DFe.Xml.ESocial.Trabalhador")
	  oTrabalhador:cpfTrab := "11111111111"
      oTrabalhador:nmTrab := "XXXXXXXX XXX XXXXXXX"
      oTrabalhador:sexo := 1 //1=TipoSexo.Masculino 2=TipoSexo.Feminino
      oTrabalhador:racaCor := 3 //RacaCor.Parda
      oTrabalhador:estCiv :=  1 //EstadoCivil.Solteiro
      oTrabalhador:grauInstr :=  7 //GrauDeInstrucao.EnsinoMedioCompleto
	  
	  oNascimento := CreateObject("Unimake.Business.DFe.Xml.ESocial.Nascimento")
	  oNascimento:dtNasctoField := "2004-04-29"
	  oNascimento:paisNascto := "105" 
	  oNascimento:paisNac := "105"
	  oTrabalhador:Nascimento := oNascimento
	  
	  oEndereco := CreateObject("Unimake.Business.DFe.Xml.ESocial.Endereco")
	  oBrasil := CreateObject("Unimake.Business.DFe.Xml.ESocial.Brasil")
	  oBrasil:tpLograd := "R"
	  oBrasil:dscLograd := "RUA XXXXXXXXXX"
	  oBrasil:nrLograd := "11"
	  oBrasil:bairro := "CIDADE NOVA"
	  oBrasil:cep := "11111111"
	  oBrasil:codMunic := "1111111"
	  oBrasil:uf := "SP"
	  oEndereco:Brasil := oBrasil	  
	  oTrabalhador:Endereco := oEndereco
	  
	  oInfoDeficiencia := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoDeficiencia")
	  oInfoDeficiencia:defFisica := 0 //SimNaoLetra.Nao
	  oInfoDeficiencia:defVisual := 0 //SimNaoLetra.Nao
	  oInfoDeficiencia:defAuditiva := 0 //SimNaoLetra.Nao
	  oInfoDeficiencia:defMental := 0 //SimNaoLetra.Nao
	  oInfoDeficiencia:defIntelectual := 0 //SimNaoLetra.Nao
	  oInfoDeficiencia:reabReadap := 0 //SimNaoLetra.Nao
	  oInfoDeficiencia:infoCota := 0 //SimNaoLetra.Nao
	  oInfoDeficiencia:observacao := "sem deficiencia"
	  oTrabalhador:InfoDeficiencia := oInfoDeficiencia
	  
	  oEvtAdmissao:Trabalhador := oTrabalhador
	  
	  oVinculo := CreateObject("Unimake.Business.DFe.Xml.ESocial.Vinculo2200")
	  oVinculo:matricula := "111111"
	  oVinculo:tpRegTrab := 1
	  oVinculo:tpRegPrev := 1
	  oVinculo:cadIni := 0 //SimNaoLetra.Nao
	  
	  oInfoRegimeTrab := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoRegimeTrab2200")
	  
	  oInfoCeletista := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoCeletista2200")
	  oInfoCeletista:dtAdmField := "2024-08-19"
	  oInfoCeletista:tpAdmissao := 1
	  oInfoCeletista:indAdmissao := 1
	  oInfoCeletista:tpRegJor := 1
	  oInfoCeletista:natAtividade := 1
	  oInfoCeletista:cnpjSindCategProf := "11111111111111"
	  oInfoRegimeTrab:InfoCeletista := oInfoCeletista

      oVinculo:InfoRegimeTrab := oInfoRegimeTrab
	  
	  oInfoContrato := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoContrato2200")
	  oInfoContrato:nmCargo := "AUX LOGISTICA"
	  oInfoContrato:CBOCargo := "517220"
	  oInfoContrato:acumCargo := 0 //SimNaoLetra.Nao
	  oInfoContrato:codCateg := "101"
	  
	  oRemuneracao := CreateObject("Unimake.Business.DFe.Xml.ESocial.Remuneracao2200")
	  oRemuneracao:vrSalFx = 1966.00
	  oRemuneracao:undSalFixo = 5
	  oInfoContrato:Remuneracao := oRemuneracao
	  
	  oDuracao := CreateObject("Unimake.Business.DFe.Xml.ESocial.Duracao2200")
	  oDuracao:tpContr = 1
	  oInfoContrato:Duracao := oDuracao
	  
	  oLocalTrabalho := CreateObject("Unimake.Business.DFe.Xml.ESocial.LocalTrabalho2200")
	  
	  oLocalTrabGeral := CreateObject("Unimake.Business.DFe.Xml.ESocial.LocalTrabGeral2200")
	  oLocalTrabGeral:tpInsc := 1
	  oLocalTrabGeral:nrInsc := "11111111111111"
	  oLocalTrabalho:LocalTrabGeral := oLocalTrabGeral
	  oInfoContrato:LocalTrabalho := oLocalTrabalho
	  
	  oHorContratual := CreateObject("Unimake.Business.DFe.Xml.ESocial.HorContratual2200")
	  oHorContratual:qtdHrsSem := "44.00"
	  oHorContratual:tpJornada := 4
	  oHorContratual:tmpParc := 0
	  oHorContratual:horNoturno := 0 // SimNaoLetra.Nao
	  oHorContratual:dscJorn := "domingo"
	  oInfoContrato:HorContratual := oHorContratual
	  
	  oObservacoes := CreateObject("Unimake.Business.DFe.Xml.ESocial.Observacoes2200")
	  oObservacoes:Observacao := "Cadastro do funcionario"
	  oInfoContrato:AddObservacoes(oObservacoes)
	  
	  oVinculo:InfoContrato := oInfoContrato
	  
	  oEvtAdmissao:Vinculo := oVinculo
	  
	  oESocial2200:EvtAdmissao := oEvtAdmissao
	  
	  oEventoESocial:ESocial2200 := oESocial2200
	  
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
      hb_MemoWrit("d:\testenfe\esocial\xmlloteeventos2200assinado.xml", stringXMLLoteAssinado)
	
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

