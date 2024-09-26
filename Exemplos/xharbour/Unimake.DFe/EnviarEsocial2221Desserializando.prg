* ---------------------------------------------------------------------------------
* Enviar Nfe de forma síncrona - Deserializando XML
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial2221Desserializando()
   Local oConfiguracao, oExceptionInterop, oErro
   Local xmlString, nomeArqLoteEvento, stringXMLLoteAssinado
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
      xmlString:= [<?xml version="1.0" encoding="UTF-8"?>] + ;
                  [<eSocial xmlns="http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1">] + ;
                    [<envioLoteEventos grupo="2">] + ;
                      [<ideEmpregador>] + ;
                        [<tpInsc>1</tpInsc>] + ;
                        [<nrInsc>99998563</nrInsc>] + ;
                      [</ideEmpregador>] + ;
                      [<ideTransmissor>] + ;
                        [<tpInsc>1</tpInsc>] + ;
                        [<nrInsc>99927739000999</nrInsc>] + ;
                      [</ideTransmissor>] + ;
                      [<eventos>] + ;
                        [<evento Id="ID1999985630000002024090421022000001">] + ;
                      [<eSocial xmlns="http://www.esocial.gov.br/schema/evt/evtToxic/v_S_01_02_00">] + ;
                            [<evtToxic Id="ID1999985630000002024091310242800001">] + ;
                          [<ideEvento>] + ;
                            [<indRetif>1</indRetif>] + ; 
                            [<tpAmb>2</tpAmb>] + ;
                            [<procEmi>1</procEmi>] + ; 
                            [<verProc>SGOWIN_Versao24091</verProc>] + ; 
                          [</ideEvento>] + ;
                          [<ideEmpregador>] + ;
                            [<tpInsc>1</tpInsc>] + ;
                            [<nrInsc>47592225</nrInsc>] + ;
                          [</ideEmpregador>] + ;
                          [<ideVinculo>] + ;
                            [<cpfTrab>11111111111</cpfTrab>] + ; 
                            [<matricula>52</matricula>] + ;
                          [</ideVinculo>] + ;
                          [<toxicologico>] + ;
                            [<dtExame>2024-09-02</dtExame>] + ; 
                            [<cnpjLab>51509164000180</cnpjLab>] + ;
                            [<codSeqExame>BB222222222</codSeqExame>] + ;
                            [<nmMed>Fulano de Tal</nmMed>] + ;
                            [<nrCRM>59327</nrCRM>] + ; 
                            [<ufCRM>SP</ufCRM>] + ; 
                          [</toxicologico>] + ;
                        [</evtToxic>] + ; 
                          [</eSocial>] + ;
                        [</evento>] + ;
                        [<evento Id="ID1999985630000002024090421022000002">] + ;
                          [<eSocial xmlns="http://www.esocial.gov.br/schema/evt/evtToxic/v_S_01_02_00">] + ;
                            [<evtToxic Id="ID1999985630000002024091310242800002">] + ;
                          [<ideEvento>] + ; 
                            [<indRetif>1</indRetif>] + ;
                            [<tpAmb>2</tpAmb>] + ;
                            [<procEmi>1</procEmi>] + ;
                            [<verProc>SGOWIN_Versao24091</verProc>] + ; 
                          [</ideEvento>] + ;
                          [<ideEmpregador>] + ;
                            [<tpInsc>1</tpInsc>] + ;
                            [<nrInsc>47592225</nrInsc>] + ; 
                          [</ideEmpregador>] + ;
                          [<ideVinculo>] + ; 
                            [<cpfTrab>11111111111</cpfTrab>] + ; 
                            [<matricula>66</matricula>] + ;
                          [</ideVinculo>] + ;
                          [<toxicologico>] + ;
                            [<dtExame>2024-09-01</dtExame>] + ;
                            [<cnpjLab>11111111111111</cnpjLab>] + ; 
                            [<codSeqExame>AA111111111</codSeqExame>] + ;
                            [<nmMed>Sicrano de Tal</nmMed>] + ;
                            [<nrCRM>0075866</nrCRM>] + ; 
                            [<ufCRM>SP</ufCRM>] + ;
                          [</toxicologico>] + ;
                        [</evtToxic>] + ;
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

      nomeArqLoteEvento := "D:\enzza\unimake_esocial\toxic_teste-esocial-loteevt.xml"
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
      hb_MemoWrit("D:\testenfe\esocial\toxic-xmlloteeventosassinado.xml", stringXMLLoteAssinado)
	
	  ? oEnviarLoteEventosESocial:RetornoWSString
	  ?
	  ?
      hb_MemoWrit("D:\testenfe\esocial\toxic-xmlloteeventos-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)
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

