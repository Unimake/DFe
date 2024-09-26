* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 2210
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial2210Desserializando()
   Local oConfiguracao, oExceptionInterop, oErro
   Local xmlString, nomeArqLoteEvento, stringXMLLoteAssinado
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
      xmlString:= [<?xml version="1.0" encoding="UTF-8"?>] + ;
                  [<eSocial xmlns="http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1">] + ;
                    [<envioLoteEventos grupo="2">] + ;
                      [<ideEmpregador>] + ;
                        [<tpInsc>1</tpInsc>] + ;
                        [<nrInsc>11111111</nrInsc>] + ;
                      [</ideEmpregador>] + ;
                      [<ideTransmissor>] + ;
                        [<tpInsc>1</tpInsc>] + ;
                        [<nrInsc>11111111111111</nrInsc>] + ;
                      [</ideTransmissor>] + ;
                      [<eventos>] + ;
                        [<evento Id="ID1230985630000002024090421022000001">] + ;
                          [<eSocial xmlns="http://www.esocial.gov.br/schema/evt/evtCAT/v_S_01_02_00">] + ;
                            [<evtCAT Id="ID1230985630000002024091310242800001">] + ;
                              [<ideEvento>] + ; 
                                [<indRetif>1</indRetif>] + ; 
                                [<tpAmb>2</tpAmb>] + ; 
                                [<procEmi>1</procEmi>] + ;
                                [<verProc>SGOWIN_Versao24091</verProc>] + ; 
                              [</ideEvento>] + ;
                              [<ideEmpregador>] + ; 
                                [<tpInsc>1</tpInsc>] + ;
                                [<nrInsc>11111111</nrInsc>] + ;
                              [</ideEmpregador>] + ; 
                              [<ideVinculo>] + ;
                                [<cpfTrab>11111111111</cpfTrab>] + ;
                                [<matricula>1111111111</matricula>] + ; 
                              [</ideVinculo>] + ;
                              [<cat>] + ; 
                                [<dtAcid>2024-09-03</dtAcid>] + ;
                                [<tpAcid>3</tpAcid>] + ;
                                [<hrAcid>0810</hrAcid>] + ; 
                                [<hrsTrabAntesAcid>0000</hrsTrabAntesAcid>] + ;
                                [<tpCat>1</tpCat>] + ; 
                                [<indCatObito>N</indCatObito>] + ; 
                                [<indComunPolicia>N</indComunPolicia>] + ; 
                                [<codSitGeradora>200012500</codSitGeradora>] + ;
                                [<iniciatCAT>1</iniciatCAT>] + ; 
                                [<obsCAT>Nil</obsCAT>] + ; 
                                [<ultDiaTrab>2024-09-03</ultDiaTrab>] + ;
                                [<houveAfast>S</houveAfast>] + ; 
                                [<localAcidente>] + ;
                                  [<tpLocal>1</tpLocal>] + ; 
                                  [<dscLocal>Nil</dscLocal>] + ; 
                                  [<tpLograd>R</tpLograd>] + ;
                                  [<dscLograd>Rua Doutor Joaquim de Abreu Sampaio Vidal</dscLograd>] + ; 
                                  [<nrLograd>Nil</nrLograd>] + ;
                                  [<complemento>de 402/403 ao fim</complemento>] + ;
                                  [<bairro>Alto Cafezal</bairro>] + ; 
                                  [<cep>17504072</cep>] + ;
                                  [<codMunic>3529005</codMunic>] + ;
                                  [<uf>SP</uf>] + ; 
                                  [<ideLocalAcid>] + ; 
                                    [<tpInsc>1</tpInsc>] + ; 
                                    [<nrInsc>23098563000160</nrInsc>] + ; 
                                  [</ideLocalAcid>] + ; 
                                [</localAcidente>] + ;
                                [<parteAtingida>] + ; 
                                  [<codParteAting>753510000</codParteAting>] + ;
                                  [<lateralidade>0</lateralidade>] + ; 
                                [</parteAtingida>] + ;
                                [<agenteCausador>] + ;
                                  [<codAgntCausador>303075900</codAgntCausador>] + ;
                                [</agenteCausador>] + ; 
                                [<atestado>] + ;
                                  [<dtAtendimento>2024-09-03</dtAtendimento>] + ; 
                                  [<hrAtendimento>0900</hrAtendimento>] + ; 
                                  [<indInternacao>S</indInternacao>] + ; 
                                  [<durTrat>14</durTrat>] + ; 
                                  [<indAfast>S</indAfast>] + ;
                                  [<dscLesao>702035000</dscLesao>] + ; 
                                  [<codCID>S52</codCID>] + ; 
                                  [<emitente>] + ; 
                                    [<nmEmit>Doutor Fulano</nmEmit>] + ; 
                                    [<ideOC>1</ideOC>] + ;
                                    [<nrOC>60678</nrOC>] + ;
                                    [<ufOC>SP</ufOC>] + ;
                                  [</emitente>] + ;
                                [</atestado>] + ;
                              [</cat>] + ;
                            [</evtCAT>] + ;
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

      nomeArqLoteEvento := "D:\testenfe\esocial\cat_teste-esocial-loteevt.xml"
      hb_MemoWrit(nomeArqLoteEvento, xmlString)

      Wait
      Cls 
	
    * Criar objeto do lote de eventos
      oESocialEnvioLoteEventos := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")
	
    * Desserializar o XML de lote de eventos a partir do arquivo gravado no HD	 
	  oESocialEnvioLoteEventos := oESocialEnvioLoteEventos:LoadFromFile("D:\testenfe\esocial\cat_teste-esocial-loteevt.xml")
	
    * Desserializar o XML de lote de eventos a partir da string do XML
	  //oESocialEnvioLoteEventos := oESocialEnvioLoteEventos:LoadFromXML("D:\testenfe\esocial\cat_teste-esocial-loteevt.xml")

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

