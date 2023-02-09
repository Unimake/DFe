* ---------------------------------------------------------------------------------
* Enviar evento de cancelamento da CTe - Desserializando XML
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarCancCTeDesserializando()
   Local oErro, oExceptionInterop
   Local oConfiguracao
   Local oDetEventoCanc, oEventoCancCTe, oInfCorrecao, oInfEvento, oRecepcaoEvento
   
 * Criar configuraçao básica para consumir o serviço
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDfe = 2 // 0=CTe
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   
 * Criar tag do lote de eventos <eventoCTe>
   oEventoCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.EventoCTe")
   
 * Desserializar o XML no objeto a partir do arquivo no HD
*  oEventoCTe = oEventoCTe:LoadFromFile("D:\testenfe\xharbour\Unimake.DFe\XmlCancCTe.xml") 

 * Desserializar o objeto a partir de uma string
   xmlString = '<?xml version="1.0" encoding="utf-8"?><eventoCTe versao="3.00" xmlns="http://www.portalfiscal.inf.br/cte"><infEvento Id="ID1101114119100611747300015055001000057928177984361001"><cOrgao>41</cOrgao><tpAmb>2</tpAmb><CNPJ>06117473000150</CNPJ><chCTe>41191006117473000150550010000579281779843610</chCTe><dhEvento>2022-10-27T08:15:44-03:00</dhEvento><tpEvento>110111</tpEvento><nSeqEvento>1</nSeqEvento><detEvento versaoEvento="3.00"><evCancCTe><descEvento>Cancelamento</descEvento><nProt>141190000660363</nProt><xJust>Justificativa para cancelamento da NFe de teste</xJust></evCancCTe></detEvento></infEvento></eventoCTe>'
   oEventoCTe = oEventoCTe:LoadFromXML(xmlString) 

 * Resgatando alguns dados do objeto do XML do evento
   ? oEventoCTe:Versao
   ? oEventoCTe:InfEvento:COrgao
   ? oEventoCTe:InfEvento:CNPJ //Demonstrar o CNPJ infomrado no XML
   ? oEventoCTe:InfEvento:DhEvento //Demonstrar o CNPJ infomrado no XML   
   ?
   Wait   
   Cls 

   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
   
   Try 
    * Enviar evento
      oRecepcaoEvento = CreateObject("Unimake.Business.DFe.Servicos.CTe.RecepcaoEvento")
      oRecepcaoEvento:SetXMLConfiguracao(oEventoCTe, oConfiguracao)
	  
	  ? oRecepcaoEvento:GetConteudoXMLAssinado()
	  ?
	  ?
	  ?
	  Wait
	  
	  cls
	  
	  oRecepcaoEvento:Executar(oEventoCTe, oConfiguracao)
	  
	  ? "CStat Retornado:", oRecepcaoEvento:Result:InfEvento:CStat
	  ? "XMotivo Retornado: ", oRecepcaoEvento:Result:InfEvento:XMotivo
	  ?
	  ?
	  Wait	
	  
	  Cls
	  
	  ? oRecepcaoEvento:RetornoWSString
	  ?
	  ?
	  Wait
	  Cls

      SWITCH oRecepcaoEvento:Result:InfEvento:CStat
         CASE 134 //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo CT-e com situação diferente de Autorizada.
         CASE 135 //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo CTe.
         CASE 156 //Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo CT-e prejudicado.
              oRecepcaoEvento:GravarXmlDistribuicao("tmp\testenfe") //Grava o XML de distribuição
              Exit
   				 
        #Ifdef __XHARBOUR__
         DEFAULT
        #Else
         OTHERWISE    
        #endif
              // Evento rejeitado
              // Realizar as ações necessárias
              Exit
       END
	  
   Catch oErro
      //Demonstrar exceções geradas no proprio Harbour, se existir.
	  ? "ERRO"
	  ? "===="
	  ? "Falha ao tentar consultar o status do servico."
      ? oErro:Description
      ? oErro:Operation
	  
      //Demonstrar a exceção do CSHARP
	  ?
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
      ?     
	  
	  Wait
	  cls   
   End	   
Return