* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 2220
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial2220Desserializando()
   Local oConfiguracao
   Local xmlString
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
      xmlString:= [<?xml version="1.0" encoding="utf-8"?>] + ;
                  [<eSocial xmlns="http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1">] + ;
                    [<envioLoteEventos grupo="2">] + ;
                      [<ideEmpregador>] + ;
                        [<tpInsc>1</tpInsc>] + ;
                          [<nrInsc>99999999</nrInsc>] + ;
                              [</ideEmpregador>] + ;
                              [<ideTransmissor>] + ;
                                [<tpInsc>1</tpInsc>] + ;
                                [<nrInsc>99999999999999</nrInsc>] + ;
                              [</ideTransmissor>] + ;
                              [<eventos>] + ;
                                [<evento Id="ID1999999990000002024090421022000001">] + ;
                                  [<eSocial xmlns="http://www.esocial.gov.br/schema/evt/evtMonit/v_S_01_02_00">] + ;
                                    [<evtMonit Id="ID1999999990000002024091310242800001">] + ;
                                      [<ideEvento>] + ;
                                        [<indRetif>1</indRetif>] + ;
                                        [<tpAmb>2</tpAmb>] + ;
                                        [<procEmi>1</procEmi>] + ;
                                        [<verProc>SGOWIN_Versao24091</verProc>] + ;
                                      [</ideEvento>] + ;
                                      [<ideEmpregador>] + ;
                                        [<tpInsc>1</tpInsc>] + ;
                                        [<nrInsc>12345678</nrInsc>] + ;
                                      [</ideEmpregador>] + ;
                                      [<ideVinculo>] + ;
                                        [<cpfTrab>88888888888</cpfTrab>] + ;
                                        [<matricula>52</matricula>] + ;
                                      [</ideVinculo>] + ;
                                      [<exMedOcup>] + ;
                                        [<tpExameOcup>1</tpExameOcup>] + ;
                                        [<aso>] + ;
                                          [<dtAso>2024-08-06</dtAso>] + ;
                                          [<resAso>1</resAso>] + ;
                                          [<exame>] + ;
                                            [<dtExm>2024-08-06</dtExm>] + ;
                                            [<procRealizado>0693</procRealizado>] + ;
                                            [<indResult>1</indResult>] + ;
                                          [</exame>] + ;
                                          [<exame>] + ;
                                            [<dtExm>2024-08-06</dtExm>] + ;
                                            [<procRealizado>0295</procRealizado>] + ;
                                            [<indResult>1</indResult>] + ;
                                          [</exame>] + ;
                                          [<medico>] + ;
                                            [<nmMed>Fulana de Tal Filha</nmMed>] + ;
                                            [<nrCRM>654321</nrCRM>] + ;
                                            [<ufCRM>SP</ufCRM>] + ;
                                          [</medico>] + ;
                                        [</aso>] + ;
                                        [<respMonit>] + ;
                                          [<nmResp>Dr. Medico da Silva</nmResp>] + ;
                                          [<nrCRM>123456</nrCRM>] + ;
                                          [<ufCRM>SP</ufCRM>] + ;
                                        [</respMonit>] + ;
                                      [</exMedOcup>] + ;
                                    [</evtMonit>] + ;
                                  [</eSocial>] + ;
                                [</evento>] + ;
                                [<evento Id="ID1999999990000002024090421022000002">] + ;
                                  [<eSocial xmlns="http://www.esocial.gov.br/schema/evt/evtMonit/v_S_01_02_00">] + ;
                                    [<evtMonit Id="ID1999999990000002024091310242800002">] + ;
                                      [<ideEvento>] + ;
                                        [<indRetif>1</indRetif>] + ;
                                        [<tpAmb>2</tpAmb>] + ;
                                        [<procEmi>1</procEmi>] + ;
                                        [<verProc>SGOWIN_Versao24091</verProc>] + ;
                                      [</ideEvento>] + ;
                                      [<ideEmpregador>] + ;
                                        [<tpInsc>1</tpInsc>] + ;
                                        [<nrInsc>12345678</nrInsc>] + ;
                                      [</ideEmpregador>] + ;
                                      [<ideVinculo>] + ;
                                        [<cpfTrab>88888888888</cpfTrab>] + ;
                                        [<matricula>66</matricula>] + ;
                                     [</ideVinculo>] + ;
                                     [<exMedOcup>] + ;
                                        [<tpExameOcup>1</tpExameOcup>] + ;
                                        [<aso>] + ;
                                          [<dtAso>2024-08-07</dtAso>] + ;
                                          [<resAso>1</resAso>] + ;
                                          [<exame>] + ;
                                            [<dtExm>2024-08-07</dtExm>] + ;
                                            [<procRealizado>0704</procRealizado>] + ;
                                            [<indResult>1</indResult>] + ;
                                          [</exame>] + ;
                                          [<exame>] + ;
                                            [<dtExm>2024-08-07</dtExm>] + ;
                                            [<procRealizado>0705</procRealizado>] + ;
                                            [<indResult>1</indResult>] + ;
                                          [</exame>] + ;
                                          [<exame>] + ;
                                            [<dtExm>2024-08-07</dtExm>] + ;
                                            [<procRealizado>0733</procRealizado>] + ;
                                            [<indResult>1</indResult>] + ;
                                          [</exame>] + ;
                                          [<exame>] + ;
                                            [<dtExm>2024-08-07</dtExm>] + ;
                                            [<procRealizado>0234</procRealizado>] + ;
                                            [<indResult>1</indResult>] + ;
                                          [</exame>] + ;
                                          [<exame>] + ;
                                            [<dtExm>2024-08-07</dtExm>] + ;
                                            [<procRealizado>0693</procRealizado>] + ;
                                            [<indResult>1</indResult>] + ;
                                          [</exame>] + ;
                                          [<exame>] + ;
                                            [<dtExm>2024-08-07</dtExm>] + ;
                                            [<procRealizado>0295</procRealizado>] + ;
                                            [<indResult>1</indResult>] + ;
                                          [</exame>] + ;
                                          [<medico>] + ;
                                            [<nmMed>Fulana de Tal Filha</nmMed>] + ;
                                            [<nrCRM>654321</nrCRM>] + ;
                                            [<ufCRM>SP</ufCRM>] + ;
                                          [</medico>] + ;
                                        [</aso>] + ;
                                        [<respMonit>] + ;
                                          [<nmResp>Dr. Medico da Silva</nmResp>] + ;
                                          [<nrCRM>123456</nrCRM>] + ;
                                          [<ufCRM>SP</ufCRM>] + ;
                                        [</respMonit>] + ;
                                      [</exMedOcup>] + ;
                                    [</evtMonit>] + ;
                                  [</eSocial>] + ;
                                [</evento>] + ;
                                [<evento Id="ID1999999990000002024090421022000003">] + ;
                                  [<eSocial xmlns="http://www.esocial.gov.br/schema/evt/evtMonit/v_S_01_02_00">] + ;
                                    [<evtMonit Id="ID1999999990000002024091310242800003">] + ;
                                     [<ideEvento>] + ;
                                        [<indRetif>1</indRetif>] + ;
                                        [<tpAmb>2</tpAmb>] + ;
                                        [<procEmi>1</procEmi>] + ;
                                        [<verProc>SGOWIN_Versao24091</verProc>] + ;
                                     [</ideEvento>] + ;
                                     [<ideEmpregador>] + ;
                                        [<tpInsc>1</tpInsc>] + ;
                                        [<nrInsc>12345678</nrInsc>] + ;
                                     [</ideEmpregador>] + ;
                                     [<ideVinculo>] + ;
                                        [<cpfTrab>88888888888</cpfTrab>] + ;
                                        [<matricula>72</matricula>] + ;
                                     [</ideVinculo>] + ;
                                     [<exMedOcup>] + ;
                                        [<tpExameOcup>1</tpExameOcup>] + ;
                                        [<aso>] + ;
                                          [<dtAso>2024-08-08</dtAso>] + ;
                                          [<resAso>1</resAso>] + ;
                                          [<exame>] + ;
                                            [<dtExm>2024-08-08</dtExm>] + ;
                                            [<procRealizado>0704</procRealizado>] + ;
                                            [<indResult>1</indResult>] + ;
                                          [</exame>] + ;
                                          [<exame>] + ;
                                            [<dtExm>2024-08-08</dtExm>] + ;
                                            [<procRealizado>0705</procRealizado>] + ;
                                            [<indResult>1</indResult>] + ;
                                          [</exame>] + ;
                                          [<exame>] + ;
                                            [<dtExm>2024-08-08</dtExm>] + ;
                                            [<procRealizado>0733</procRealizado>] + ;
                                            [<indResult>1</indResult>] + ;
                                          [</exame>] + ;
                                          [<exame>] + ;
                                            [<dtExm>2024-08-08</dtExm>] + ;
                                            [<procRealizado>0234</procRealizado>] + ;
                                            [<indResult>1</indResult>] + ;
                                          [</exame>] + ;
                                          [<exame>] + ;
                                            [<dtExm>2024-08-08</dtExm>] + ;
                                            [<procRealizado>0693</procRealizado>] + ;
                                            [<indResult>1</indResult>] + ;
                                          [</exame>] + ;
                                          [<exame>] + ;
                                            [<dtExm>2024-08-08</dtExm>] + ;
                                            [<procRealizado>0295</procRealizado>] + ;
                                            [<indResult>1</indResult>] + ;
                                          [</exame>] + ;
                                          [<medico>] + ;
                                            [<nmMed>Outra Medica</nmMed>] + ;
                                            [<nrCRM>236678</nrCRM>] + ;
                                            [<ufCRM>SP</ufCRM>] + ;
                                          [</medico>] + ;
                                        [</aso>] + ;
                                        [<respMonit>] + ;
                                          [<nmResp>Dr. Medico da Silva</nmResp>] + ;
                                          [<nrCRM>123456</nrCRM>] + ;
                                          [<ufCRM>SP</ufCRM>] + ;
                                        [</respMonit>] + ;
                                      [</exMedOcup>] + ;
                                    [</evtMonit>] + ;
                                  [</eSocial>] + ;
                                [</evento>] + ;
                                [<evento Id="ID1999999990000002024090421022000004">] + ;
                                  [<eSocial xmlns="http://www.esocial.gov.br/schema/evt/evtMonit/v_S_01_02_00">] + ;
                                    [<evtMonit Id="ID1999999990000002024091310242800004">] + ;
                                      [<ideEvento>] + ;
                                        [<indRetif>1</indRetif>] + ;
                                        [<tpAmb>2</tpAmb>] + ;
                                        [<procEmi>1</procEmi>] + ;
                                        [<verProc>SGOWIN_Versao24091</verProc>] + ;
                                      [</ideEvento>] + ;
                                      [<ideEmpregador>] + ;
                                        [<tpInsc>1</tpInsc>] + ;
                                        [<nrInsc>12345678</nrInsc>] + ;
                                      [</ideEmpregador>] + ;
                                      [<ideVinculo>] + ;
                                        [<cpfTrab>88888888888</cpfTrab>] + ;
                                        [<matricula>73</matricula>] + ;
                                      [</ideVinculo>] + ;
                                      [<exMedOcup>] + ;
                                        [<tpExameOcup>9</tpExameOcup>] + ;
                                        [<aso>] + ;
                                          [<dtAso>2024-08-30</dtAso>] + ;
                                          [<resAso>1</resAso>] + ;
                                          [<exame>] + ;
                                            [<dtExm>2024-08-30</dtExm>] + ;
                                            [<procRealizado>0295</procRealizado>] + ;
                                            [<indResult>1</indResult>] + ;
                                          [</exame>] + ;
                                          [<medico>] + ;
                                            [<nmMed>Fulana de Tal</nmMed>] + ;
                                            [<nrCRM>654321</nrCRM>] + ;
                                            [<ufCRM>SP</ufCRM>] + ;
                                          [</medico>] + ;
                                        [</aso>] + ;
                                        [<respMonit>] + ;
                                          [<nmResp>Dr. Medico da Silva</nmResp>] + ;
                                          [<nrCRM>123456</nrCRM>] + ;
                                          [<ufCRM>SP</ufCRM>] + ;
                                        [</respMonit>] + ;
                                      [</exMedOcup>] + ;
                                    [</evtMonit>] + ;
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

      nomeArqLoteEvento:= "D:\enzza\unimake_esocial\cat_teste-esocial-loteevt.xml"
      hb_MemoWrit(nomeArqLoteEvento, xmlString)

      Wait
      Cls 
	
    * Criar objeto do lote de eventos
      oESocialEnvioLoteEventos:= CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")
	
    * Desserializar o XML de lote de eventos a partir do arquivo gravado no HD	 
*      oESocialEnvioLoteEventos:= oESocialEnvioLoteEventos:LoadFromFile("D:\enzza\unimake_esocial\monit_teste-esocial-loteevt.xml")
	
    * Desserializar o XML de lote de eventos a partir da string do XML
      //oESocialEnvioLoteEventos:= oESocialEnvioLoteEventos:LoadFromXML("D:\enzza\unimake_esocial\monit_teste-esocial-loteevt.xml")
      oESocialEnvioLoteEventos:= oESocialEnvioLoteEventos:LoadFromXML(xmlString)

    * Criar objeto para consumir o serviço de envio de lotes de eventos do eSocial
      oEnviarLoteEventosESocial:= CreateObject("Unimake.Business.DFe.Servicos.ESocial.EnviarLoteEventosESocial")
	  
	* Setar o XML e configuração antes de enviar para conseguir resgatar o XML original e assinado antes de chamar o método Executar() para enviar
	  oEnviarLoteEventosESocial:SetXMLConfiguracao(oESocialEnvioLoteEventos, oConfiguracao)
	  
    * Resgatar o conteúdo do XML original ou assinado antes de enviar:
      stringXMLLoteOriginal := oEnviarLoteEventosESocial:GetConteudoXMLOriginal() //Sem assinatura
      ? "stringXMLLoteOriginal:", stringXMLLoteOriginal
      ?
      ?
      Wait
	  
      stringXMLLoteAssinado := oEnviarLoteEventosESocial:GetConteudoXMLAssinado() //Com assinatura
      ? "stringXMLLoteAssinado:", stringXMLLoteAssinado
      ?
      ?
      Wait
	   
    * Consumir o serviço	
      oEnviarLoteEventosESocial:Executar(oESocialEnvioLoteEventos, oConfiguracao)     
      
      hb_MemoWrit("D:\enzza\unimake_esocial\xmlloteeventosassinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      hb_MemoWrit("D:\enzza\unimake_esocial\xmlloteeventos-monit-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)
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

