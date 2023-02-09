* ---------------------------------------------------------------------------------
* Enviar evento de cancelamento da NFe
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function CancelarNFeDesserializando()
   Local oErro, oExceptionInterop
   Local oConfiguracao
   Local oEnvEvento, xmlString
   
 * Criar configuraçao básica para consumir o serviço
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDfe = 0 // 0=nfe
   oConfiguracao:Servico = 5 // 5=Envio de evento
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
   
   Try 
    * Criar tag EnvEvento
      oEnvEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.EnvEvento")   
   
    * Desserializar o XML no objeto a partir do arquivo no HD
      oEnvEvento = oEnvEvento:LoadFromFile("D:\testenfe\xharbour\Unimake.DFe\xmlcancelamento-ped-eve.xml") 
  
    * Desserializar o objeto a partir de uma string
      xmlString = '<?xml version="1.0" encoding="utf-8"?><envEvento versao="1.00" xmlns="http://www.portalfiscal.inf.br/nfe"><idLote>000000000000001</idLote><evento versao="1.00" xmlns="http://www.portalfiscal.inf.br/nfe"><infEvento Id="ID1101114119100611747300015055001000057928177984361001"><cOrgao>41</cOrgao><tpAmb>2</tpAmb><CNPJ>06117473000150</CNPJ><chNFe>41191006117473000150550010000579281779843610</chNFe><dhEvento>2022-07-06T08:06:00-03:00</dhEvento><tpEvento>110111</tpEvento><nSeqEvento>1</nSeqEvento><verEvento>1.00</verEvento><detEvento versao="1.00"><descEvento>Cancelamento</descEvento><nProt>010101010101010</nProt><xJust>Justificativa do cancelamento</xJust></detEvento></infEvento></evento></envEvento>'
*     oEnvEvento = oEnvEvento:LoadFromXML(xmlString)
 
    * Resgatando alguns dados do objeto do XML do evento
      ? oEnvEvento:Versao, oEnvEvento:IdLote
      ? "Qde eventos:", oEnvEvento:GetEventoCount()
	  
      For I = 1 To oEnvEvento:GetEventoCount()
         oTagEvento := oEnvEvento:GetEvento(I - 1) 
         ? I, oTagEvento:InfEvento:NSeqEvento, oTagEvento:InfEvento:COrgao
      Next I    

	  ?
	  ?
	  Wait   
   
    * Enviar evento
      oRecepcaoEvento = CreateObject("Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento")
      oRecepcaoEvento:Executar(oEnvEvento,  oConfiguracao)
	  
	  cls
	  ? "Demonstrar o xml gerado e assinado"
	  ?
	  ? oRecepcaoEvento:GetConteudoXMLAssinado()
	  ?
	  ?	  
	  Wait
	  
	  cls
	  
      ? "CStat do Lote Retornado:", oRecepcaoEvento:Result:CStat, "- XMotivo:", oRecepcaoEvento:Result:XMotivo
 
      if oRecepcaoEvento:Result:CStat == 128 //128 = Lote de evento processado com sucesso.
       * Como pode existir vários eventos no XML (Caso da carta de correção que posso enviar várias sequencias de evento)
       * é necessário fazer um loop para ver a autorização de cada um deles
         For I = 1 To oRecepcaoEvento:Result:GetRetEventoCount()
             oRetEvento = oRecepcaoEvento:Result:GetRetEvento(I - 1)
   		  
             SWITCH oRetEvento:InfEvento:CStat
               CASE 135 //Evento homologado com vinculação da respectiva NFe
               CASE 136 //Evento homologado sem vinculação com a respectiva NFe (SEFAZ não encontrou a NFe na base dela)
               CASE 155 //Evento de Cancelamento homologado fora do prazo permitido para cancelamento 
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
   		
             ? "CStat do evento", AllTrim(Str(I,10)), "retornado:", oRetEvento:InfEvento:CStat, "- xMotivo:", oRetEvento:InfEvento:XMotivo
         Next
      EndIf 
	  ?
	  ?
	  Wait

   Catch oErro
      //Demonstrar exceções geradas no proprio Harbour, se existir.
	  ? "ERRO"
	  ? "===="
	  ? "Falha ao tentar enviar o xml de cancelamento da nfe."
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