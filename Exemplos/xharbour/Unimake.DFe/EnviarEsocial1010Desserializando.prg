* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 1010
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial1010Desserializando()
   Local oConfiguracao, oExceptionInterop, oErro
   Local xmlString, nomeArqLoteEvento, stringXMLLoteAssinado
   Local oESocialEnvioLoteEventos
   Local oEnviarLoteEventosESocial

 * Criar o objeto de configuração mínima
   oConfiguracao:= CreateObject("Unimake.Business.DFe.Servicos.Configuracao")

   oConfiguracao:TipoDFe           := 12 // 12 = eSocial
   oConfiguracao:Servico           := 69 // Servico.ESocialEnviarLoteEventos
   oConfiguracao:CertificadoArquivo:= "D:\tools\sergio\BACKUP\CERT DIG AGAPE_MEDICINA_DO_TRABALHO_LTDA_15527739000123_1702988563264876200.pfx"
   oConfiguracao:CertificadoSenha  := "1234"
   oConfiguracao:TipoAmbiente      := 2  // TipoAmbiente.Homologação

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop:= CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try         
      xmlString:= [<?xml version="1.0" encoding="UTF-8"?>] + ;
                  [<eSocial xmlns="http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1">] + ;
                    [<envioLoteEventos grupo="1">] + ;
                      [<ideEmpregador>] + ;
                        [<tpInsc>1</tpInsc>] + ;
                        [<nrInsc>15527739</nrInsc>] + ;
                      [</ideEmpregador>] + ;
                      [<ideTransmissor>] + ;
                        [<tpInsc>1</tpInsc>] + ;
                        [<nrInsc>15527739000123</nrInsc>] + ;
                      [</ideTransmissor>] + ;
                      [<eventos>] + ;
                        [<evento Id="ID1155277390000002024090421022000001">] + ;
                          [<eSocial xmlns="http://www.esocial.gov.br/schema/evt/evtTabRubrica/v_S_01_02_00">] + ;
                            [<evtTabRubrica Id="ID1155277390000002024090421022000001">] + ;
                              [<ideEvento>] + ; 
                                [<tpAmb>2</tpAmb>] + ;
                                [<procEmi>1</procEmi>] + ; 
                                [<verProc>SGOWIN_Versao24091</verProc>] + ; 
                              [</ideEvento>] + ;
                              [<ideEmpregador>] + ;
                                [<tpInsc>1</tpInsc>] + ;
                                [<nrInsc>15527739</nrInsc>] + ;
                              [</ideEmpregador>] + ;
                              [<infoRubrica>] + ;
                                [<inclusao>] + ; 
                                  [<ideRubrica>] + ;
                                    [<codRubr>554</codRubr>] + ;
                                    [<ideTabRubr>UNICA</ideTabRubr>] + ;
                                    [<iniValid>2024-06</iniValid>] + ;
                                  [</ideRubrica>] + ;
                                  [<dadosRubrica>] + ;
                                    [<dscRubr>MENSALIDADE SINDICAL</dscRubr>] + ;
                                    [<natRubr>9231</natRubr>] + ;
                                    [<tpRubr>2</tpRubr>] + ;
                                    [<codIncCP>21</codIncCP>] + ;
                                    [<codIncIRRF>11</codIncIRRF>] + ;
                                    [<codIncFGTS>00</codIncFGTS>] + ;
                                    [<observacao>Inclusao do codigo incidencia</observacao>] + ;
                                 [</dadosRubrica>] + ;
                                [</inclusao>] + ;
                               [</infoRubrica>] + ;
                              [</evtTabRubrica>] + ; 
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

      nomeArqLoteEvento := "D:\enzza\unimake_esocial\rubrica_teste-esocial-loteevt.xml"
      hb_MemoWrit(nomeArqLoteEvento, xmlString)

      Wait
      Cls 
     * Criar objeto do lote de eventos
      oESocialEnvioLoteEventos := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")
	
    * Desserializar o XML de lote de eventos a partir do arquivo gravado no HD	 
*      oESocialEnvioLoteEventos := oESocialEnvioLoteEventos:LoadFromFile("D:\enzza\unimake_esocial\cat_teste-esocial-loteevt.xml")
	
    * Desserializar o XML de lote de eventos a partir da string do XML
      //oESocialEnvioLoteEventos := oESocialEnvioLoteEventos:LoadFromXML("D:\enzza\unimake_esocial\cat_teste-esocial-loteevt.xml")
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
      hb_MemoWrit("D:\enzza\unimake_esocial\rubrica-xmlloteeventosassinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      hb_MemoWrit("D:\enzza\unimake_esocial\rubrica-xmlloteeventos-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)
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