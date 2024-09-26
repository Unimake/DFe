* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 2221
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial2221()
   Local oConfiguracao, oExceptionInterop, oErro
   Local nomeArqLoteEvento, stringXMLLoteAssinado
   Local oESocialEnvioLoteEventos
   Local oEnviarLoteEventosESocial

 * Criar o objeto de configuração mínima
   oConfiguracao:= CreateObject("Unimake.Business.DFe.Servicos.Configuracao")

   oConfiguracao:TipoDFe           := 12 // 12 = eSocial
   oConfiguracao:Servico           := 69 // Servico.ESocialEnviarLoteEventos
   oConfiguracao:CertificadoArquivo:= "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha  := "12345678"
   oConfiguracao:TipoAmbiente      := 2  // TipoAmbiente.Homologação

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop:= CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try         
    * Montando o XML do lote de eventos
      oESocialEnvioLoteEventos:= CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")      

      oEnvioLoteEventosESocial:= CreateObject("Unimake.Business.DFe.Xml.ESocial.EnvioLoteEventosESocial")
      oEnvioLoteEventosESocial:Grupo:= "2"
      
      oIdeEmpregador:= CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
      oIdeEmpregador:tpInsc:= 1 //TiposInscricao.CNPJ
      oIdeEmpregador:nrInsc:= "23098563"
      oEnvioLoteEventosESocial:IdeEmpregador:= oIdeEmpregador
      
      oIdeTransmissor:= CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeTransmissor")
      oIdeTransmissor:tpInsc:= 1 //TiposInscricao.CNPJ
      oIdeTransmissor:nrInsc:= "15527739000123"
      oEnvioLoteEventosESocial:IdeTransmissor:= oIdeTransmissor
      
      oEventosESocial:= CreateObject("Unimake.Business.DFe.Xml.ESocial.EventosESocial")
      
      oEventoESocial:= CreateObject("Unimake.Business.DFe.Xml.ESocial.EventoESocial")
      oEventoESocial:Id:= "ID1230985630000002024090421022000001"
      
      oESocial2221:= CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocial2221")
	 
      oEvtToxic:= CreateObject("Unimake.Business.DFe.Xml.ESocial.EvtToxic")
      oEvtToxic:Id:= "ID1230985630000002024091310242800001"
      
      oIdeEvento2221:= CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEvento2221")
      oIdeEvento2221:IndRetif:= 1 //IndicativoRetificacao.ArquivoOriginal
      oIdeEvento2221:TpAmb   := 2 //TipoAmbiente.Homologacao
      oIdeEvento2221:ProcEmi := 1 //ProcEmiESocial.AppDoEmpregador
      oIdeEvento2221:VerProc := "SGOWIN_Versao24091"
      oEvtToxic:IdeEvento:= oIdeEvento2221

      oIdeEmpregador:= CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
      oIdeEmpregador:tpInsc:= 1 //TiposInscricao.CNPJ
      oIdeEmpregador:nrInsc:= "23098563"
      oEvtToxic:IdeEmpregador:= oIdeEmpregador
      
      oIdeVinculo:= CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeVinculo")
      oIdeVinculo:cpfTrab  := "11111111111"
      oIdeVinculo:matricula:= "1111111111"
      oEvtToxic:IdeVinculo:= oIdeVinculo
 
      oToxicologico:= CreateObject("Unimake.Business.DFe.Xml.ESocial.Toxicologico")
      oToxicologico:CnpjLab     := "11111111111111"
      oToxicologico:CodSeqExame := "BB222222222"
      oToxicologico:DtExameField:= "2024-09-03" 
*     oToxicologico:DtExame     := "2024-09-03"
      oToxicologico:NmMed       := "Fulano de Tal"
      oToxicologico:NrCRM       := "59327"
      oToxicologico:UfCRM      := 35 //UFBrasil.SP 
     
      oEvtToxic:Toxicologico:= oToxicologico

      oESocial2221:EvtToxic:= oEvtToxic
      
      oEventoESocial:ESocial2221:= oESocial2221
      
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
      hb_MemoWrit("D:\testenfe\esocial\tox-xmlloteeventosassinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos-Toxicologico-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)
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

