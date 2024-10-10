* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 2240
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial2240()
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
      oEnvioLoteEventosESocial:Grupo := "2"
      
      oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
      oIdeEmpregador:tpInsc := 1 //TiposInscricao.CNPJ
      oIdeEmpregador:nrInsc := "23098563"
      oEnvioLoteEventosESocial:IdeEmpregador := oIdeEmpregador
      
      oIdeTransmissor:= CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeTransmissor")
      oIdeTransmissor:tpInsc := 1 //TiposInscricao.CNPJ
      oIdeTransmissor:nrInsc := "15527739000123"
      oEnvioLoteEventosESocial:IdeTransmissor := oIdeTransmissor

      oEventosESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EventosESocial")
      
      oEventoESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EventoESocial")
      oEventoESocial:Id := "ID1230985630000002024091310242800001"
      
      oESocial2240 := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocial2240")
	 
      oEvtExpRisco := CreateObject("Unimake.Business.DFe.Xml.ESocial.EvtExpRisco")
      oEvtExpRisco:Id := "ID1230985630000002024091310242800001"
      
      oIdeEvento2240 := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEvento2240")
      oIdeEvento2240:IndRetif := 1 // IndicativoRetificacao.ArquivoOriginal
      oIdeEvento2240:TpAmb    := 2 // TipoAmbiente.Homologacao
      oIdeEvento2240:ProcEmi  := 1 // ProcEmiESocial.AppDoEmpregador 
      oIdeEvento2240:VerProc  := "SGOWIN_Versao24091"
      oEvtExpRisco:IdeEvento  := oIdeEvento2240
      
      oEvtExpRisco:IdeEmpregador := oIdeEmpregador
      
      oIdeVinculo := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeVinculo2240")
      oIdeVinculo:CpfTrab     := "22321798858"
      oIdeVinculo:Matricula   := "66"
      oEvtExpRisco:IdeVinculo := oIdeVinculo
      
      oInfoExpRisco := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoExpRisco")
      oInfoExpRisco:DtIniCondicaoField := "2022-08-08"
*     oInfoExpRisco:DtFimCondicaoField := "2024-08-08"
     
      oInfoAmb := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoAmb")
      oInfoAmb:LocalAmb := 1 // LocalAmb.EstabelecimentoProprioEmpregador	
      oInfoAmb:DscSetor := "OPERACIONAL"
      oInfoAmb:TpInsc   := 1 // TiposInscricao.CNPJ
      oInfoAmb:NrInsc   := "47592225000148"
      oInfoExpRisco:AddInfoAmb(oInfoAmb)

      oInfoAtiv := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoAtiv")
      oInfoAtiv:DscAtivDes := "Prestar assistencia farmaceutica orientar os pacientes quanto aos exames realizar exames analisar amostras de materiais biologicos verificar resultados das analises"
      oInfoExpRisco:InfoAtiv := oInfoAtiv

      oAgNoc := CreateObject("Unimake.Business.DFe.Xml.ESocial.AgNoc")
      oAgNoc:CodAgNoc := "03.01.001"
      oAgNoc:tpAval  := 2 //TpAval.CriterioQualitativo

      oEpcEpi := CreateObject("Unimake.Business.DFe.Xml.ESocial.EpcEpi")
      oEpcEpi:UtilizEPC := 0
      oEpcEpi:UtilizEPI := 2
  
	  oEpi := CreateObject("Unimake.Business.DFe.Xml.ESocial.Epi")
      oEpi:DocAval := "27785"
      oEpcEpi:AddEpi(oEpi)	  

      oEpi := CreateObject("Unimake.Business.DFe.Xml.ESocial.Epi")
      oEpi:docAval := "9149"
      oEpcEpi:AddEpi(oEpi)

      oEpiCompl:= CreateObject("Unimake.Business.DFe.Xml.ESocial.EpiCompl")
      oEpiCompl:medProtecao   := 1 //SimNaoLetra.Sim
      oEpiCompl:condFuncto    := 1 //SimNaoLetra.Sim
      oEpiCompl:usoInint      := 1 //SimNaoLetra.Sim
      oEpiCompl:przValid      := 1 //SimNaoLetra.Sim
      oEpiCompl:periodicTroca := 1 //SimNaoLetra.Sim
      oEpiCompl:higienizacao  := 1 //SimNaoLetra.Sim
      oEpcEpi:EpiCompl := oEpiCompl
  
      oAgNoc:EpcEpi := oEpcEpi
	  
	  oInfoExpRisco:AddAgNoc(oAgNoc)
      
      oRespReg:= CreateObject("Unimake.Business.DFe.Xml.ESocial.RespReg") 
      oRespReg:cpfResp     := "08417948805"
      oRespReg:ideOC       := 9
      oRespReg:dscOC       := "MTE"
      oRespReg:nrOC        := "0131369"
      oRespReg:ufOC        := 35 // UFBrasil.SP
	  
      oInfoExpRisco:AddRespReg(oRespReg)
	  
      oEvtExpRisco:InfoExpRisco := oInfoExpRisco
      	  
      oESocial2240:EvtExpRisco := oEvtExpRisco

      oEventoESocial:ESocial2240 := oESocial2240
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
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos2240assinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos-2240-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)
	  
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
