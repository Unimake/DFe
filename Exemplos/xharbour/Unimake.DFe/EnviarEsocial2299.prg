* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 2299
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial2299()
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
      oIdeEmpregador:nrInsc := "21998472"
      oEnvioLoteEventosESocial:IdeEmpregador := oIdeEmpregador
      
      oIdeTransmissor:= CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeTransmissor")
      oIdeTransmissor:tpInsc := 1 //TiposInscricao.CNPJ
      oIdeTransmissor:nrInsc := "21998472000155"
      oEnvioLoteEventosESocial:IdeTransmissor := oIdeTransmissor

      oEventosESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EventosESocial")
      
      oEventoESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EventoESocial")
      oEventoESocial:Id := "ID1219984720000002024072211433200426"
      
      oESocial2299 := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocial2299")
	 
      oEvtDeslig := CreateObject("Unimake.Business.DFe.Xml.ESocial.EvtDeslig")
      oEvtDeslig:Id := "ID1219984720000002024070512161000143"
      
      oIdeEvento := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEvento2299")
      oIdeEvento:IndRetif := 1 // IndicativoRetificacao.ArquivoOriginal
      oIdeEvento:TpAmb    := 1 // TipoAmbiente.Homologacao
      oIdeEvento:ProcEmi  := 1 // ProcEmiESocial.AppDoEmpregador 
      oIdeEvento:VerProc  := "1.0"
      oEvtDeslig:IdeEvento  := oIdeEvento
      
      oEvtDeslig:IdeEmpregador := oIdeEmpregador
      
      oIdeVinculo := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeVinculo2299")
      oIdeVinculo:CpfTrab     := "48733335800"
      oIdeVinculo:Matricula   := "000242"
      oEvtDeslig:IdeVinculo := oIdeVinculo
      
      oInfoDeslig := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoDeslig")
      oInfoDeslig:mtvDeslig := "02"
	  oInfoDeslig:dtDesligField := "2024-07-01"
	  oInfoDeslig:indPagtoAPI := 1 //SimNaoLetra.Sim
	  oInfoDeslig:DtProjFimAPIField := "2024-07-01"
	  oInfoDeslig:pensAlim := 0 //PensAlim.NaoExistePensaoAlimenticia
	  
      oVerbasResc := CreateObject("Unimake.Business.DFe.Xml.ESocial.VerbasResc")
	  
	  oDmDev := CreateObject("Unimake.Business.DFe.Xml.ESocial.DmDev2299")
	  oDmDev:ideDmDev := "ideDmDev1"
	  
	  oInfoPerAnt := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoPerAnt2299")
	  
	  oIdeADC := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeADC2299")
	  oIdeADC:dtAcConvField := "2022-01-01"
	  oIdeADC:tpAcConv := "A"
	  oIdeADC:dsc := "DESCRICAO"

      oIdePeriodo := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdePeriodo2299")	  
	  oIdePeriodo:PerRefField := "2024-07"
	  
	  oIdeEstabLot := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEstabLot2299")	  
	  oIdeEstabLot:tpInsc := 1
	  oIdeEstabLot:nrInsc := "21998472000155"
	  oIdeEstabLot:codLotacao := "LOTA00015501"
	  
	  oDetVerbas := CreateObject("Unimake.Business.DFe.Xml.ESocial.DetVerbas")	  
	  oDetVerbas:codRubr := "001"
	  oDetVerbas:ideTabRubr := "UNICA"
	  oDetVerbas:qtdRubr := 1.00
	  oDetVerbas:fatorRubr := 1.00
	  oDetVerbas:vrRubr := 87.60
	  oDetVerbas:indApurIR := 0 //IndApurIR.Normal
	  oIdeEstabLot:AddDetVerbas(oDetVerbas)
	             
	  oDetVerbas := CreateObject("Unimake.Business.DFe.Xml.ESocial.DetVerbas")	  
	  oDetVerbas:codRubr := "019"
	  oDetVerbas:ideTabRubr := "UNICA"
	  oDetVerbas:qtdRubr := 6.00
	  oDetVerbas:fatorRubr := 1.00
	  oDetVerbas:vrRubr := 1314.00
	  oDetVerbas:indApurIR := 0 //IndApurIR.Normal
	  oIdeEstabLot:AddDetVerbas(oDetVerbas)

	  oDetVerbas := CreateObject("Unimake.Business.DFe.Xml.ESocial.DetVerbas")	  
	  oDetVerbas:codRubr := "027"
	  oDetVerbas:ideTabRubr := "UNICA"
	  oDetVerbas:qtdRubr := 1.00
	  oDetVerbas:fatorRubr := 1.00
	  oDetVerbas:vrRubr := 219.00
	  oDetVerbas:indApurIR := 0 //IndApurIR.Normal
	  oIdeEstabLot:AddDetVerbas(oDetVerbas)

	  oDetVerbas := CreateObject("Unimake.Business.DFe.Xml.ESocial.DetVerbas")	  
	  oDetVerbas:codRubr := "028"
	  oDetVerbas:ideTabRubr := "UNICA"
	  oDetVerbas:qtdRubr := 30.00
	  oDetVerbas:fatorRubr := 1.00
	  oDetVerbas:vrRubr := 2628.00
	  oDetVerbas:indApurIR := 0 //IndApurIR.Normal
	  oIdeEstabLot:AddDetVerbas(oDetVerbas)

	  oDetVerbas := CreateObject("Unimake.Business.DFe.Xml.ESocial.DetVerbas")	  
	  oDetVerbas:codRubr := "203"
	  oDetVerbas:ideTabRubr := "UNICA"
	  oDetVerbas:qtdRubr := 17.50
	  oDetVerbas:fatorRubr := 1.00
	  oDetVerbas:vrRubr := 1533.00
	  oDetVerbas:indApurIR := 0 //IndApurIR.Normal
	  oIdeEstabLot:AddDetVerbas(oDetVerbas)

	  oDetVerbas := CreateObject("Unimake.Business.DFe.Xml.ESocial.DetVerbas")	  
	  oDetVerbas:codRubr := "205"
	  oDetVerbas:ideTabRubr := "UNICA"
	  oDetVerbas:qtdRubr := 1.00
	  oDetVerbas:fatorRubr := 1.00
	  oDetVerbas:vrRubr := 584.00
	  oDetVerbas:indApurIR := 0 //IndApurIR.Normal
	  oIdeEstabLot:AddDetVerbas(oDetVerbas)

	  oDetVerbas := CreateObject("Unimake.Business.DFe.Xml.ESocial.DetVerbas")	  
	  oDetVerbas:codRubr := "208"
	  oDetVerbas:ideTabRubr := "UNICA"
	  oDetVerbas:qtdRubr := 2.50
	  oDetVerbas:fatorRubr := 1.00
	  oDetVerbas:vrRubr := 219.00
	  oDetVerbas:indApurIR := 0 //IndApurIR.Normal
	  oIdeEstabLot:AddDetVerbas(oDetVerbas)

	  oDetVerbas := CreateObject("Unimake.Business.DFe.Xml.ESocial.DetVerbas")	  
	  oDetVerbas:codRubr := "551"
	  oDetVerbas:ideTabRubr := "UNICA"
	  oDetVerbas:qtdRubr := 1.00
	  oDetVerbas:fatorRubr := 1.00
	  oDetVerbas:vrRubr := 6.56
	  oDetVerbas:indApurIR := 0 //IndApurIR.Normal
	  oIdeEstabLot:AddDetVerbas(oDetVerbas)

	  oDetVerbas := CreateObject("Unimake.Business.DFe.Xml.ESocial.DetVerbas")	  
	  oDetVerbas:codRubr := "556"
	  oDetVerbas:ideTabRubr := "UNICA"
	  oDetVerbas:qtdRubr := 1.00
	  oDetVerbas:fatorRubr := 1.00
	  oDetVerbas:vrRubr := 1.00
	  oDetVerbas:indApurIR := 0 //IndApurIR.Normal
	  oIdeEstabLot:AddDetVerbas(oDetVerbas)

	  oDetVerbas := CreateObject("Unimake.Business.DFe.Xml.ESocial.DetVerbas")	  
	  oDetVerbas:codRubr := "559"
	  oDetVerbas:ideTabRubr := "UNICA"
	  oDetVerbas:qtdRubr := 1.00
	  oDetVerbas:fatorRubr := 1.00
	  oDetVerbas:vrRubr := 98.54
	  oDetVerbas:indApurIR := 0 //IndApurIR.Normal
	  oIdeEstabLot:AddDetVerbas(oDetVerbas)

	  oDetVerbas := CreateObject("Unimake.Business.DFe.Xml.ESocial.DetVerbas")	  
	  oDetVerbas:codRubr := "580"
	  oDetVerbas:ideTabRubr := "UNICA"
	  oDetVerbas:qtdRubr := 1.00
	  oDetVerbas:fatorRubr := 1.00
	  oDetVerbas:vrRubr := 3.31
	  oDetVerbas:indApurIR := 0 //IndApurIR.Normal
	  oIdeEstabLot:AddDetVerbas(oDetVerbas)

	  oDetVerbas := CreateObject("Unimake.Business.DFe.Xml.ESocial.DetVerbas")	  
	  oDetVerbas:codRubr := "991"
	  oDetVerbas:ideTabRubr := "UNICA"
	  oDetVerbas:qtdRubr := 1.00
	  oDetVerbas:fatorRubr := 1.00
	  oDetVerbas:vrRubr := 1401.60
	  oDetVerbas:indApurIR := 0 //IndApurIR.Normal
	  oIdeEstabLot:AddDetVerbas(oDetVerbas)

	  oDetVerbas := CreateObject("Unimake.Business.DFe.Xml.ESocial.DetVerbas")	  
	  oDetVerbas:codRubr := "996"
	  oDetVerbas:ideTabRubr := "UNICA"
	  oDetVerbas:qtdRubr := 1.00
	  oDetVerbas:fatorRubr := 1.00
	  oDetVerbas:vrRubr := 339.89
	  oDetVerbas:indApurIR := 0 //IndApurIR.Normal
	  oIdeEstabLot:AddDetVerbas(oDetVerbas)

	  oDetVerbas := CreateObject("Unimake.Business.DFe.Xml.ESocial.DetVerbas")	  
	  oDetVerbas:codRubr := "993"
	  oDetVerbas:ideTabRubr := "UNICA"
	  oDetVerbas:qtdRubr := 1.00
	  oDetVerbas:fatorRubr := 1.00
	  oDetVerbas:vrRubr := 1664.40
	  oDetVerbas:indApurIR := 0 //IndApurIR.Normal
	  oIdeEstabLot:AddDetVerbas(oDetVerbas)

	  oDetVerbas := CreateObject("Unimake.Business.DFe.Xml.ESocial.DetVerbas")	  
	  oDetVerbas:codRubr := "995"
	  oDetVerbas:ideTabRubr := "UNICA"
	  oDetVerbas:qtdRubr := 1.00
	  oDetVerbas:fatorRubr := 1.00
	  oDetVerbas:vrRubr := 4248.60
	  oDetVerbas:indApurIR := 0 //IndApurIR.Normal
	  oIdeEstabLot:AddDetVerbas(oDetVerbas)

      oInfoAgNocivo := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoAgNocivo2299")	  
	  oInfoAgNocivo:GrauExp := "1"
	  oIdeEstabLot:InfoAgNocivo := oInfoAgNocivo

      oIdePeriodo:AddIdeEstabLot(oIdeEstabLot)

      oIdeADC:AddIdePeriodo(oIdePeriodo)
	  
      oInfoPerAnt:AddIdeADC(oIdeADC)
	  
	  oDmDev:InfoPerAnt := oInfoPerAnt
	  
      oVerbasResc:AddDmDev(oDmDev)
	  
	  oInfoDeslig:VerbasResc := oVerbasResc
	  
	  oEvtDeslig:InfoDeslig := oInfoDeslig
      	  
      oESocial2299:EvtDeslig := oEvtDeslig

      oEventoESocial:ESocial2299 := oESocial2299
      oEventosESocial:AddEvento(oEventoESocial)
      oEnvioLoteEventosESocial:Eventos:= oEventosESocial
      oESocialEnvioLoteEventos:EnvioLoteEventos:= oEnvioLoteEventosESocial

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
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos2299assinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos-2299-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)
	  
      ? "CdResposta:", oEnviarLoteEventosESocial:Result:RetornoEnvioLoteEventos:Status:CdResposta
	  ? "DescResposta:", oEnviarLoteEventosESocial:Result:RetornoEnvioLoteEventos:Status:DescResposta
	  ?
	  
	  //? Numero do protocolo: ", oEnviarLoteEventosESocial:Result:RetornoEnvioLoteEventos:DadosRecepcaoLote:ProtocoloEnvio
	  
	  
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
