* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 1210
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial1210Desserializando()
   Local oConfiguracao, oExceptionInterop, oErro
   Local xmlString, nomeArqLoteEvento, stringXMLLoteAssinado
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
      xmlString := [<?xml version="1.0" encoding="utf-8"?>] + ;
                  [<eSocial xmlns="http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1">] + ;
                    [<envioLoteEventos grupo="1">] + ;
                      [<ideEmpregador>] + ;
                        [<tpInsc>1</tpInsc>] + ;
                        [<nrInsc>11111111</nrInsc>] + ;
                      [</ideEmpregador>] + ;
                      [<ideTransmissor>] + ;
                        [<tpInsc>1</tpInsc>] + ;
                        [<nrInsc>11111111111111</nrInsc>] + ;
                      [</ideTransmissor>] + ;
                      [<eventos>] + ;
                        [<evento Id="ID1219984720000002024040318014905925">] + ;
                          [<eSocial xmlns="http://www.esocial.gov.br/schema/evt/evtPgtos/v_S_01_02_00">] + ;
                            [<evtPgtos Id="ID1219984720000002024040318014905925">] + ;
                              [<ideEvento>] + ;
                                [<indRetif>1</indRetif>] + ;
                                [<perApur>2024-03</perApur>] + ;
                                [<tpAmb>2</tpAmb>] + ;
                                [<procEmi>1</procEmi>] + ;
                                [<verProc>1.0</verProc>] + ;
                              [</ideEvento>] + ;
                              [<ideEmpregador>] + ;
                                [<tpInsc>1</tpInsc>] + ;
                                [<nrInsc>11111111</nrInsc>] + ;
                              [</ideEmpregador>] + ;
                              [<ideBenef>] + ;
                                [<cpfBenef>60361011393</cpfBenef>] + ;
                                [<infoPgto>] + ;
                                  [<dtPgto>2024-08-05</dtPgto>] + ;
                                  [<tpPgto>1</tpPgto>] + ;
                                  [<perRef>2024-08</perRef>] + ;
                                  [<ideDmDev>P2408</ideDmDev>] + ;
                                  [<vrLiq>1530.00</vrLiq>] + ;
                                [</infoPgto>] + ;
                                [<infoPgto>] + ;
                                  [<dtPgto>2024-08-20</dtPgto>] + ;
                                  [<tpPgto>1</tpPgto>] + ;
                                  [<perRef>2024-08</perRef>] + ;
                                  [<ideDmDev>A2408</ideDmDev>] + ;
                                  [<vrLiq>920.00</vrLiq>] + ;
                                [</infoPgto>] + ;
                              [</ideBenef>] + ;
                            [</evtPgtos>] + ;
                          [</eSocial>] + ;
                        [</evento>] + ;
                      [</eventos>] + ;
                    [</envioLoteEventos>] + ;
                  [</eSocial>]

      ? "String do XML:"
      ?
      ?
      ? xmlString
      ?
      ?

      nomeArqLoteEvento := "D:\testenfe\esocial\1210_teste-esocial-loteevt.xml"
      hb_MemoWrit(nomeArqLoteEvento, xmlString)

      Wait
      Cls 
     * Criar objeto do lote de eventos
      oESocialEnvioLoteEventos := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")
	
    * Desserializar o XML de lote de eventos a partir do arquivo gravado no HD	 
*      oESocialEnvioLoteEventos := oESocialEnvioLoteEventos:LoadFromFile("D:\testenfe\esocial\1210_teste-esocial-loteevt.xml")
	
    * Desserializar o XML de lote de eventos a partir da string do XML
      //oESocialEnvioLoteEventos := oESocialEnvioLoteEventos:LoadFromXML("D:\testenfe\esocial\1210_teste-esocial-loteevt.xml")
      oESocialEnvioLoteEventos := oESocialEnvioLoteEventos:LoadFromXML(xmlString)

    * Criar objeto para consumir o serviço de envio de lotes de eventos do eSocial
      oEnviarLoteEventosESocial := CreateObject("Unimake.Business.DFe.Servicos.ESocial.EnviarLoteEventosESocial")
	
    * Consumir o serviço	
      oEnviarLoteEventosESocial:Executar(oESocialEnvioLoteEventos, oConfiguracao)
      
      stringXMLLoteAssinado = oEnviarLoteEventosESocial:GetConteudoXMLAssinado()
      
      ? "StringXMLAssinado:", stringXMLLoteAssinado
      ?
      ?
      Wait
      hb_MemoWrit("D:\testenfe\esocial\1210-xmlloteeventosassinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      hb_MemoWrit("D:\testenfe\esocial\1210-xmlloteeventos-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)
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