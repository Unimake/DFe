* ---------------------------------------------------------------------------------
* Enviar evento de cancelamento da NFCe
* ---------------------------------------------------------------------------------
Function EnviarEventoCancelamentoNFCe()
   Local oErro, oExceptionInterop 
   Local oConfiguracao, I, eventoAssinado
   Local oEnvEvento, oEvento, oDetEventoCanc, oInfEvento, oRecepcaoEvento, oRetEvento
   
 * Criar configuraçao básica para consumir o serviço
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDfe = 1 && 1=nfce ###
   oConfiguracao.CertificadoSenha = "12345678"
   oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

 * Criar tag EnvEvento
   oEnvEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.EnvEvento")
   oEnvEvento.Versao = "1.00"
   oEnvEvento.IdLote = "000000000000001"

 * -------------------------------------------------
 * Criar tags do evento sequencia 1
 * -------------------------------------------------
 * Criar tag Evento
   oEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.Evento")
   oEvento.Versao = "1.00"
 
 * Criar tag DetEventoCanc
   oDetEventoCanc = CreateObject("Unimake.Business.DFe.Xml.NFe.DetEventoCanc")
   oDetEventoCanc.Versao = "1.00"
   oDetEventoCanc.NProt = "141190000660363"
   oDetEventoCanc.XJust = "Justificativa para cancelamento da NFe de teste"

 * Criar tag InfEvento
   oInfEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.InfEvento")
 
 * Adicionar a tag DetEventoCanc dentro da Tag DetEvento
   oInfEvento.DetEvento = oDetEventoCanc
 
 * Atualizar propriedades da oInfEvento
 * IMPORTANTE: Atualização da propriedade TpEvento deve acontecer depois que o DetEvento recebeu o oDetEventoCanc para que funcione sem erro
   oInfEvento.COrgao = 41 && UFBrasil.PR
   oInfEvento.ChNFe = "41191006117473000150550010000579281779843610"
   oInfEvento.CNPJ = "06117473000150"
   oInfEvento.DhEvento = DateTime()
   oInfEvento.TpEvento = 110111 && TipoEventoNFe.Cancelamento
   oInfEvento.NSeqEvento = 1
   oInfEvento.VerEvento = "1.00"
   oInfEvento.TpAmb = 2 && TipoAmbiente.Homologacao

 * Adicionar a tag InfEvento dentro da tag Evento
   oEvento.InfEvento = oInfEvento

 * Adicionar a tag Evento dentro da tag EnvEvento
   oEnvEvento.AddEvento(oEvento)

 * Resgatando alguns dados do objeto do XML do evento
   MESSAGEBOX("Versao schema: " + oEnvEvento.Versao + " - LOTE: " + oEnvEvento.IdLote)
   MESSAGEBOX("Qde eventos: " + ALLTRIM(STR(oEnvEvento.GetEventoCount(),10)))
  
   For I = 1 To oEnvEvento.GetEventoCount()
       oTagEvento = oEnvEvento.GetEvento(I - 1) 
       MESSAGEBOX("SEQUENCIA EVENTO: " + ALLTRIM(STR(oTagEvento.InfEvento.NSeqEvento,10)) + " - ORGAO: " + ALLTRIM(STR(oTagEvento.InfEvento.COrgao,10)))
   Next I    
  
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
   
   Try 
    * Enviar evento
      oRecepcaoEvento = CreateObject("Unimake.Business.DFe.Servicos.NFCe.RecepcaoEvento") && ###
      oRecepcaoEvento.Executar(oEnvEvento,  oConfiguracao)

      eventoAssinado = oRecepcaoEvento.GetConteudoXMLAssinado()
      MESSAGEBOX(eventoAssinado)
      
    * Gravar o XML assinado no HD, antes de enviar.
      DELETE FILE 'd:\testenfe\CancelamentoNFCe.xml'
	  StrToFile(eventoAssinado, 'd:\testenfe\CancelamentoNFCe.xml', 0)    

      MESSAGEBOX("CStat do Lote Retornado: " + ALLTRIM(STR(oRecepcaoEvento.Result.CStat,10)) + " - XMotivo: " + oRecepcaoEvento.Result.XMotivo)
 
      if oRecepcaoEvento.Result.CStat == 128 && 128 = Lote de evento processado com sucesso.
       * Como pode existir vários eventos no XML (Caso da carta de correção que posso enviar várias sequencias de evento)
       * é necessário fazer um loop para ver a autorização de cada um deles
         For I = 1 To oRecepcaoEvento.Result.GetRetEventoCount()
             oRetEvento = oRecepcaoEvento.Result.GetRetEvento(I - 1)
   		  
             DO CASE
                CASE oRetEvento.InfEvento.CStat = 135 && Evento homologado com vinculação da respectiva NFe
                CASE oRetEvento.InfEvento.CStat = 136 && Evento homologado sem vinculação com a respectiva NFe (SEFAZ não encontrou a NFe na base dela)
                CASE oRetEvento.InfEvento.CStat = 155 && Evento de Cancelamento homologado fora do prazo permitido para cancelamento 
                     oRecepcaoEvento.GravarXmlDistribuicao("tmp\testenfe") && Grava o XML de distribuição
   				 
               OTHERWISE    
                    * Evento rejeitado
                    * Realizar as ações necessárias
             ENDCASE
   		
             MESSAGEBOX("CStat do evento " + AllTrim(Str(I,10)) + ": " + ALLTRIM(STR(oRetEvento.InfEvento.CStat,10)) + " - xMotivo: " + oRetEvento.InfEvento.XMotivo)
         Next
      EndIf 

   Catch To oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MessageBox("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MessageBox("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())
   EndTry
Return