* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento S-3000
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif

Function EnviarEsocial3000()
   Local oConfiguracao, oExceptionInterop, oErro
   Local stringXMLLoteAssinado
   Local oESocialEnvioLoteEventos
   Local oEnvioLoteEventosESocial
   Local oIdeEmpregador
   Local oIdeTransmissor
   Local oEventosESocial
   Local oEventoESocial
   Local oESocial3000
   Local oEvtExclusao
   Local oIdeEvento3000
   Local oInfoExclusao
   Local oIdeTrabalhador3000
   Local oIdeFolhaPagto
   Local oEnviarLoteEventosESocial

   * Criar o objeto de configuração mínima
   oConfiguracao := CreateObject("Unimake.Business.DFe.Servicos.Configuracao")

   oConfiguracao:TipoDFe            := 12 // 12 = eSocial
   oConfiguracao:Servico            := 69 // Servico.ESocialEnviarLoteEventos
   oConfiguracao:CertificadoArquivo := "D:\projetos\Unimake_PV.pfx"
   oConfiguracao:CertificadoSenha   := "12345678"
   oConfiguracao:TipoAmbiente       := 2  // TipoAmbiente.Homologacao

   * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop := CreateObject("Unimake.Exceptions.ThrowHelper")

   Try
      * --------------------------------------------------------------------------
      * Montando o XML do lote de eventos
      * --------------------------------------------------------------------------
      oESocialEnvioLoteEventos := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")

      oEnvioLoteEventosESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EnvioLoteEventosESocial")
      oEnvioLoteEventosESocial:Grupo := "2"

      * --------------------------------------------------------------------------
      * ideEmpregador do lote
      * --------------------------------------------------------------------------
      oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
      oIdeEmpregador:tpInsc := 1 // TiposInscricao.CNPJ
      oIdeEmpregador:nrInsc := "11111111"
      oEnvioLoteEventosESocial:IdeEmpregador := oIdeEmpregador

      * --------------------------------------------------------------------------
      * ideTransmissor do lote
      * --------------------------------------------------------------------------
      oIdeTransmissor := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeTransmissor")
      oIdeTransmissor:tpInsc := 1 // TiposInscricao.CNPJ
      oIdeTransmissor:nrInsc := "11111111111111"
      oEnvioLoteEventosESocial:IdeTransmissor := oIdeTransmissor

      * --------------------------------------------------------------------------
      * Criar coleção de eventos do lote
      * --------------------------------------------------------------------------
      oEventosESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EventosESocial")

      * --------------------------------------------------------------------------
      * Criar item do lote
      * --------------------------------------------------------------------------
      oEventoESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EventoESocial")
      oEventoESocial:Id := "ID1111111100000020260318123000001"

      * --------------------------------------------------------------------------
      * Criar evento S-3000
      * --------------------------------------------------------------------------
      oESocial3000 := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocial3000")

      oEvtExclusao := CreateObject("Unimake.Business.DFe.Xml.ESocial.EvtExclusao")
      oEvtExclusao:Id := "ID1111111100000020260318123000002"

      * --------------------------------------------------------------------------
      * ideEvento
      * --------------------------------------------------------------------------
      oIdeEvento3000 := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEvento3000")
      oIdeEvento3000:TpAmb   := 2 // TipoAmbiente.Homologacao
      oIdeEvento3000:ProcEmi := 1 // ProcEmiESocial.AppDoEmpregador
      oIdeEvento3000:VerProc := "SGOWIN_Versao26031"
      oEvtExclusao:IdeEvento := oIdeEvento3000

      * --------------------------------------------------------------------------
      * ideEmpregador do evento
      * --------------------------------------------------------------------------
      oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
      oIdeEmpregador:tpInsc := 1 // TiposInscricao.CNPJ
      oIdeEmpregador:nrInsc := "11111111"
      oEvtExclusao:IdeEmpregador := oIdeEmpregador

      * --------------------------------------------------------------------------
      * infoExclusao
      * --------------------------------------------------------------------------
      oInfoExclusao := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoExclusao")
      oInfoExclusao:TpEvento := "S-1200"
      oInfoExclusao:NrRecEvt := "1.2.0000000000000000000"

      * --------------------------------------------------------------------------
      * ideTrabalhador
      * Grupo que identifica a qual trabalhador se refere o evento a ser excluído
      * --------------------------------------------------------------------------
      oIdeTrabalhador3000 := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhador3000")
      oIdeTrabalhador3000:CpfTrab := "11111111111"
      oInfoExclusao:IdeTrabalhador := oIdeTrabalhador3000

      * --------------------------------------------------------------------------
      * ideFolhaPagto
      * Grupo que identifica a qual período pertence o evento a ser excluído
      * --------------------------------------------------------------------------
      oIdeFolhaPagto := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeFolhaPagto")
      oIdeFolhaPagto:IndApuracao := 1 // IndApuracao.Mensal
      oIdeFolhaPagto:PerApur     := "2026-03"
      oInfoExclusao:IdeFolhaPagto := oIdeFolhaPagto

      oEvtExclusao:InfoExclusao := oInfoExclusao

      * --------------------------------------------------------------------------
      * Vincular evtExclusao ao evento S-3000
      * --------------------------------------------------------------------------
      oESocial3000:EvtExclusao := oEvtExclusao

      * --------------------------------------------------------------------------
      * Vincular evento S-3000 ao item do lote
      * --------------------------------------------------------------------------
      oEventoESocial:ESocial3000 := oESocial3000

      * --------------------------------------------------------------------------
      * Adicionar item na coleção de eventos
      * --------------------------------------------------------------------------
      oEventosESocial:AddEvento(oEventoESocial)

      * --------------------------------------------------------------------------
      * Vincular coleção ao lote
      * --------------------------------------------------------------------------
      oEnvioLoteEventosESocial:Eventos := oEventosESocial

      * --------------------------------------------------------------------------
      * Vincular lote ao XML principal
      * --------------------------------------------------------------------------
      oESocialEnvioLoteEventos:EnvioLoteEventos := oEnvioLoteEventosESocial

      * --------------------------------------------------------------------------
      * Criar objeto para consumir o serviço de envio de lotes de eventos do eSocial
      * --------------------------------------------------------------------------
      oEnviarLoteEventosESocial := CreateObject("Unimake.Business.DFe.Servicos.ESocial.EnviarLoteEventosESocial")

      * --------------------------------------------------------------------------
      * Consumir o serviço
      * --------------------------------------------------------------------------
      oEnviarLoteEventosESocial:Executar(oESocialEnvioLoteEventos, oConfiguracao)

      * --------------------------------------------------------------------------
      * Recuperar XML assinado do lote
      * --------------------------------------------------------------------------
      stringXMLLoteAssinado := oEnviarLoteEventosESocial:GetConteudoXMLAssinado()

      ? "StringXMLAssinado:", stringXMLLoteAssinado
      ?
      ?
      Wait

      hb_MemoWrit("d:\testenfe\esocial\xmlloteeventos_s3000_assinado.xml", stringXMLLoteAssinado)

      * --------------------------------------------------------------------------
      * Exibir retorno do WebService
      * --------------------------------------------------------------------------
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      Wait
      Cls

   Catch oErro
      * Demonstrar exceções geradas no próprio Harbour, se existir.
      ? "ERRO"
      ? "===="
      ? "Falha ao tentar enviar o lote do evento S-3000."
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