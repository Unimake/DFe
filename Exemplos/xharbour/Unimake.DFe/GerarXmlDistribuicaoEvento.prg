* ---------------------------------------------------------------------------------
* Gerar XML de distribuição de eventos a partir da consulta situação 
* Pode ser utilizado no caso de não ter recebido a autorização do evento no envio 
* do mesmo, ou no caso de enviar o evento 2x e dar duplicidade, assim geramos 
* somente o XML de distribuição para finalizar a transação.
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function GerarXmlDistribuicaoEvento()
   Local oErro, oExceptionInterop
   Local oConfiguracao
   Local oEnvEvento, oEvento, oDetEventoCanc, oInfEvento
   Local oRecepcaoEvento
   Local oTagEvento, oRetEvento
   Local I, B
   Local oConsSitNFe, oConfigConsSitNFe, oConsultaProtocolo, oProcEventoNFe
   Local nomeArqDistribEventoCanc, xmlEventoCanc
   
 * Criar configuraçao básica para consumir o serviço
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDfe = 0 // 0=nfe
   oConfiguracao:Servico = 5 // 5=Envio de evento
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

 * Criar tag EnvEvento
   oEnvEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.EnvEvento")
   oEnvEvento:Versao = "1.00"
   oEnvEvento:IdLote = "000000000000001"

 * -------------------------------------------------
 * Criar tags do evento sequencia 1
 * -------------------------------------------------
 * Criar tag Evento
   oEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.Evento")
   oEvento:Versao = "1.00"
 
 * Criar tag DetEventoCanc
   oDetEventoCanc = CreateObject("Unimake.Business.DFe.Xml.NFe.DetEventoCanc")
   oDetEventoCanc:Versao = "1.00"
   oDetEventoCanc:NProt = "141230000370455"
   oDetEventoCanc:XJust = "Justificativa para cancelamento da NFe de teste"

 * Criar tag InfEvento
   oInfEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.InfEvento")
 
 * Adicionar a tag DetEventoCanc dentro da Tag DetEvento
   oInfEvento:DetEvento = oDetEventoCanc
 
 * Atualizar propriedades da oInfEvento
 * IMPORTANTE: Atualização da propriedade TpEvento deve acontecer depois que o DetEvento recebeu o oDetEventoCanc para que funcione sem erro
 
   chaveNFe := "41230606117473000150550010000600001245094621"
   
   oInfEvento:COrgao = 41 // UFBrasil.PR
   oInfEvento:ChNFe = chaveNFe
   oInfEvento:CNPJ = "06117473000150"
   oInfEvento:DhEvento = DateTime()
   oInfEvento:TpEvento = 110111 // TipoEventoNFe.Cancelamento
   oInfEvento:NSeqEvento = 1
   oInfEvento:VerEvento = "1.00"
   oInfEvento:TpAmb = 2 // TipoAmbiente.Homologacao

 * Adicionar a tag InfEvento dentro da tag Evento
   oEvento:InfEvento = oInfEvento

 * Adicionar a tag Evento dentro da tag EnvEvento
   oEnvEvento:AddEvento(oEvento)

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
   
   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
   
   Try 
    * Enviar evento
      oRecepcaoEvento = CreateObject("Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento")
      oRecepcaoEvento:Executar(oEnvEvento,  oConfiguracao)

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

               CASE 573 //Duplicidade de evento (já foi enviado anteriormente, vamos gerar o XML de distribuição a partir da consulta situação)
			        oConsSitNFe := CreateObject("Unimake.Business.DFe.Xml.NFe.ConsSitNFe")
					oConsSitNFe:Versao := "4.00"
					oConsSitNFe:TpAmb := 2 //TipoAmbiente.Homologacao
					oConsSitNFe:ChNFe := chaveNFe
										
					oConfigConsSitNFe := CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
                    oConfigConsSitNFe:CertificadoSenha   := "12345678"
                    oConfigConsSitNFe:CertificadoArquivo := "C:\Projetos\certificados\UnimakePV.pfx"

                    oConsultaProtocolo = CreateObject("Unimake.Business.DFe.Servicos.NFe.ConsultaProtocolo")
					oConsultaProtocolo:Executar(oConsSitNFe,  oConfigConsSitNFe)
					
                    if oConsultaProtocolo:Result:GetProcEventoNFeCount() > 0
					   For B = 1 To oConsultaProtocolo:Result:GetProcEventoNFeCount()
					      oProcEventoNFe := oConsultaProtocolo:Result:GetProcEventoNFe(B - 1)

                          if oProcEventoNFe:Evento:InfEvento:TpEvento == 110111 //TipoEventoNFe.Cancelamento
						     xmlEventoCanc := oProcEventoNFe:GerarXMLString()
							 
							 ? xmlEventoCanc
							 
							 nomeArqDistribEventoCanc := "d:\testenfe\" + oProcEventoNFe:NomeArquivoDistribuicao
							 ? nomeArqDistribEventoCanc

                             nHandle := fCreate(nomeArqDistribEventoCanc)
                             fwrite(nHandle, xmlEventoCanc)
                             fClose(nHandle)
						  Endif
					   Next B										   
					Endif
					
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
         Next I
      EndIf 
	  ?
	  ?
	  Wait

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