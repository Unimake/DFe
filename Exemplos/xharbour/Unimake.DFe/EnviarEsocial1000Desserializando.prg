* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 1000
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial1000Desserializando()
   Local oConfiguracao, oExceptionInterop, oErro
   Local xmlString, nomeArqLoteEvento, stringXMLLoteAssinado
   Local oESocialEnvioLoteEventos
   Local oEnviarLoteEventosESocial

 * Criar o objeto de configuração mínima
   oConfiguracao:= CreateObject("Unimake.Business.DFe.Servicos.Configuracao")

   oConfiguracao:TipoDFe            := 12 // 12 = eSocial
   oConfiguracao:Servico            := 69 // Servico.ESocialEnviarLoteEventos
   oConfiguracao:CertificadoArquivo := "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha   := "12345678"
   oConfiguracao:TipoAmbiente       := 2  // TipoAmbiente.Homologação

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop:= CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try        
      xmlString := [<?xml version="1.0" encoding="UTF-8"?>] +;
                   [<eSocial xmlns="http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1">] +;
                   	[<envioLoteEventos grupo="1">] + ;
                   		[<ideEmpregador>] + ;
                   			[<tpInsc>1</tpInsc>] + ;
                   			[<nrInsc>00000000</nrInsc>] + ;
                   		[</ideEmpregador>] + ;
                   		[<ideTransmissor>] + ;
                   			[<tpInsc>1</tpInsc>] + ;
                   			[<nrInsc>00000000000000</nrInsc>] + ;
                   		[</ideTransmissor>] + ;
                   		[<eventos>] + ;
                   			[<evento Id="ID1000000000000002017102608080800001">] + ;
                   				[<eSocial xmlns="http://www.esocial.gov.br/schema/evt/evtInfoEmpregador/v_S_01_03_00">] + ;
					                   [<evtInfoEmpregador Id="ID1000000000000002017102608080800001">] + ;
                   						[<ideEvento>] + ;
                   							[<tpAmb>1</tpAmb>] + ;
                   							[<procEmi>1</procEmi>] + ;
                   							[<verProc>str1234</verProc>] + ;
                   						[</ideEvento>] + ;
                   						[<ideEmpregador>] + ;
                   							[<tpInsc>1</tpInsc>] + ;
                   							[<nrInsc>00000000000000</nrInsc>] + ;
                   						[</ideEmpregador>] + ;
                   						[<infoEmpregador>] + ;
                   							[<inclusao>] + ;
                   								[<idePeriodo>] + ;
                   									[<iniValid>2017-10</iniValid>] + ;
                   									[<fimValid>2017-10</fimValid>] + ;
                   								[</idePeriodo>] + ;
                   								[<infoCadastro>] + ;
                   									[<classTrib>01</classTrib>] + ;
                   									[<indCoop>1</indCoop>] + ;
                   									[<indConstr>1</indConstr>] + ;
                   									[<indDesFolha>1</indDesFolha>] + ;
                   									[<indOptRegEletron>1</indOptRegEletron>] + ;
                   									[<dadosIsencao>] + ;
                   										[<ideMinLei>str1234</ideMinLei>] + ;
                   										[<nrCertif>str1234</nrCertif>] + ;
                   										[<dtEmisCertif>2200-12-13</dtEmisCertif>] + ;
                   										[<dtVencCertif>2012-12-13</dtVencCertif>] + ;
                   										[<nrProtRenov>str1234</nrProtRenov>] + ;
                   										[<dtProtRenov>2012-12-13</dtProtRenov>] + ;
                   										[<dtDou>2012-12-13</dtDou>] + ;
                   										[<pagDou>745</pagDou>] + ;
                   									[</dadosIsencao>] + ;
                   									[<infoOrgInternacional>] + ;
                   										[<indAcordoIsenMulta>1</indAcordoIsenMulta>] + ;
                   									[</infoOrgInternacional>] + ;
                   								[</infoCadastro>] + ;
                   							[</inclusao>] + ;
                   						[</infoEmpregador>] + ;
					                   [</evtInfoEmpregador>] + ;
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

      nomeArqLoteEvento := "D:\testenfe\esocial\evtInfoEmpregador_teste-esocial-loteevt.xml"
      hb_MemoWrit(nomeArqLoteEvento, xmlString)

      Wait
      Cls 
     * Criar objeto do lote de eventos
      oESocialEnvioLoteEventos := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")
	
    * Desserializar o XML de lote de eventos a partir do arquivo gravado no HD	 
*      oESocialEnvioLoteEventos := oESocialEnvioLoteEventos:LoadFromFile("D:\enzza\unimake_esocial\cat_teste-esocial-loteevt.xml")
	
    * Desserializar o XML de lote de eventos a partir da string do XML
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
      hb_MemoWrit("D:\testenfe\esocial\evtInfoEmpregador-xmlloteeventosassinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      hb_MemoWrit("D:\testenfe\esocial\evtInfoEmpregador-xmlloteeventos-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)
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