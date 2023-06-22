* ---------------------------------------------------------------------------------
* Enviar evento de EPEC da NFe
* ---------------------------------------------------------------------------------
Function EPECEnviarEventoEPEC()
   Local oErro, oExceptionInterop 
   Local oConfiguracao, I, eventoAssinado
   Local oEnvEvento, oEvento, oDetEventoEPEC, oDetEventoEPECDest, oInfEvento, oRecepcaoEvento, oRetEvento 
     
 * Criar configuraçao básica para consumir o serviço
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDfe = 0 && 0=nfe
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
   oDetEventoEPEC = CreateObject("Unimake.Business.DFe.Xml.NFe.DetEventoEPEC")
   oDetEventoEPEC.Versao = "1.00"
   oDetEventoEPEC.COrgaoAutor = 41 && UFBrasil.PR
   oDetEventoEPEC.TpAutor = 1 && TipoAutor.EmpresaEmitente
   oDetEventoEPEC.VerAplic = "1.00"
   oDetEventoEPEC.TpNF = 1 && TipoOperacao.Saida
   oDetEventoEPEC.DhEmiField = "2023-06-22T09:20:16-03:00"
   oDetEventoEPEC.IE = "9032000301"
   
 * Criar a tag DetEventoEPECDest
   oDetEventoEPECDest = CreateObject("Unimake.Business.DFe.Xml.NFe.DetEventoEPECDest")
   oDetEventoEPECDest.CNPJ = "04218457000128"
   oDetEventoEPECDest.IE = "582614838110"
   oDetEventoEPECDest.UF = 35 && UFBrasil.SP
   oDetEventoEPECDest.VNF = 254.70
   oDetEventoEPECDest.VICMS = 0.00
   oDetEventoEPECDest.VST = 0.00
   
   oDetEventoEPEC.Dest = oDetEventoEPECDest   

 * Criar tag InfEvento
   oInfEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.InfEvento")
 
 * Adicionar a tag DetEventoEPEC dentro da Tag DetEvento
   oInfEvento.DetEvento = oDetEventoEPEC
 
 * Atualizar propriedades da oInfEvento
 * IMPORTANTE: Atualização da propriedade TpEvento deve acontecer depois que o DetEvento recebeu o oDetEventoEPEC para que funcione sem erro
   oInfEvento.COrgao = 91 && UFBrasil.AN = Ambiente Nacional
   oInfEvento.ChNFe = "41230606117473000150550030000000064860795147"
   oInfEvento.CNPJ = "06117473000150"
   oInfEvento.DhEvento = DateTime()
   oInfEvento.TpEvento = 110140 && TipoEventoNFe.EPEC
   oInfEvento.NSeqEvento = 1
   oInfEvento.VerEvento = "1.00"
   oInfEvento.TpAmb = 2 && TipoAmbiente.Homologacao

 * Adicionar a tag InfEvento dentro da tag Evento
   oEvento.InfEvento = oInfEvento

 * Adicionar a tag Evento dentro da tag EnvEvento
   oEnvEvento.AddEvento(oEvento)   
  
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
  
   TRY
    * Enviar evento
      oRecepcaoEvento = CreateObject("Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento")
      oRecepcaoEvento.Executar(oEnvEvento, oConfiguracao)

      eventoAssinado = oRecepcaoEvento.GetConteudoXMLAssinado()
      MESSAGEBOX(eventoAssinado)
  
    * Gravar o XML assinado no HD, antes de enviar.
      DELETE FILE 'd:\testenfe\epec\' + oInfEvento.ChNFe + '-EPECNFe.xml'
	  StrToFile(eventoAssinado, 'd:\testenfe\epec\' + oInfEvento.ChNFe + '-EPECNFe.xml', 0)	  

      MESSAGEBOX("CStat do Lote Retornado: " + ALLTRIM(STR(oRecepcaoEvento.Result.CStat,10)) + " - XMotivo: " + oRecepcaoEvento.Result.XMotivo)
      MESSAGEBOX(oRecepcaoEvento.RetornoWSString)
      
      if oRecepcaoEvento.Result.CStat == 128 && 128 = Lote de evento processado com sucesso.
       * Como pode existir vários eventos no XML (Caso da carta de correção que posso enviar várias sequencias de evento)
       * é necessário fazer um loop para ver a autorização de cada um deles
         For I = 1 To oRecepcaoEvento.Result.GetRetEventoCount()
             oRetEvento = oRecepcaoEvento.Result.GetRetEvento(I - 1)
   		  
             IF oRetEvento.InfEvento.CStat = 136 && Evento homologado sem vinculação com a respectiva NFe (SEFAZ não encontrou a NFe na base dela)
                oRecepcaoEvento.GravarXmlDistribuicao("d:\testenfe\epec") && Grava o XML de distribuição

              * Criar as configurações para imprimir o DANFE
                oUnidanfeConfiguration = CreateObject("Unimake.Unidanfe.Configurations.UnidanfeConfiguration")   
                oUnidanfeConfiguration.Arquivo = "D:\testenfe\epec\" + oInfEvento.ChNFe + "-nfe.xml" 
                oUnidanfeConfiguration.ArquivoEPEC = "D:\testenfe\epec\" + oInfEvento.ChNFe + "_" + STR(oInfEvento.TpEvento,6) + "_0" + STR(oInfEvento.nSeqEvento,1) + "-proceventonfe.xml"
                oUnidanfeConfiguration.Visualizar = .T.
                oUnidanfeConfiguration.Imprimir = .F.
                oUnidanfeConfiguration.EnviaEmail = .F.    
     
              * Disparar a impressao DANFe NFe com EPEC
                oUnidanfeServices = CreateObject("Unimake.Unidanfe.UnidanfeServices")
                oUnidanfeServices.Execute(oUnidanfeConfiguration)
   			 ELSE
                * Evento rejeitado
                * Realizar as ações necessárias
                MESSAGEBOX("CStat do evento " + AllTrim(Str(I,10)) + ": " + ALLTRIM(STR(oRetEvento.InfEvento.CStat,10)) + " - xMotivo: " + oRetEvento.InfEvento.XMotivo)
             ENDIF   		
         Next
      EndIf 

   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MessageBox("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MessageBox("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())
   ENDTRY
RETURN

