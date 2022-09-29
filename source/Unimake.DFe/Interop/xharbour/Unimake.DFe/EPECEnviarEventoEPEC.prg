* ---------------------------------------------------------------------------------
* EPEC - Enviar o Evento do EPEC
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EPECEnviarEventoEPEC()
   Local oErro, oExceptionInterop
   Local oConfiguracao
   Local oEnvEvento, oEvento, oDetEventoEPEC, oInfEvento
   
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
 
 * Criar tag DetEventoEPEC
   oDetEventoEPEC = CreateObject("Unimake.Business.DFe.Xml.NFe.DetEventoEPEC")
   oDetEventoEPEC:Versao = "1.00"
   oDetEventoEPEC:COrgaoAutor = 41 //UFBrasil.PR
   oDetEventoEPEC:TpAutor = 1 //TipoAutor.EmpresaEmitente
   oDetEventoEPEC:VerAplic = "1.00"
   oDetEventoEPEC:TpNF = 1 //TipoOperacao.Saida
   oDetEventoEPEC:DhEmi = DateTime()
   oDetEventoEPEC:IE = "9032000301"

 * Criar a tag DetEventoEPECDest
   oDetEventoEPECDest = CreateObject("Unimake.Business.DFe.Xml.NFe.DetEventoEPECDest")
   oDetEventoEPECDest:CNPJ = "06117473000150"
   oDetEventoEPECDest:UF = 41 //UFBrasil.PR
   oDetEventoEPECDest:VNF = 86.00
   oDetEventoEPECDest:VICMS = 6.02
   oDetEventoEPECDest:VST = 0.00
   
   oDetEventoEPEC:Dest = oDetEventoEPECDest
   
 * Criar tag InfEvento
   oInfEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.InfEvento")
 
 * Adicionar a tag DetEventoEPEC dentro da Tag DetEvento
   oInfEvento:DetEvento = oDetEventoEPEC
 
 * Atualizar propriedades da oInfEvento
 * IMPORTANTE: Atualização da propriedade TpEvento deve acontecer depois que o DetEvento recebeu o oDetEventoEPEC para que funcione sem erro
   oInfEvento:COrgao = 91 // UFBrasil.PR
   oInfEvento:ChNFe = "41220906117473000150550010000580154230845951"
   oInfEvento:CNPJ = "06117473000150"
   oInfEvento:DhEvento = DateTime()
   oInfEvento:TpEvento = 110140 // TipoEventoNFe.EPEC
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