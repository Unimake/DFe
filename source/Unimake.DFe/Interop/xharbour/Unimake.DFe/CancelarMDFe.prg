* ---------------------------------------------------------------------------------
* Enviar evento de cancelamento do MDFe
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function CancelarMDFe()
   Local oErro, oExceptionInterop
   Local oConfiguracao
   Local oEnvEvento, oEventoMDFe, oDetEventoCanc, oInfEvento
   
 * Criar configuraçao básica para consumir o serviço
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDfe = 4 // 4=MDFe
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

 * Criar tag EnvEvento
   oEnvEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.EnvEvento")
   oEnvEvento:Versao = "3.00"
   oEnvEvento:IdLote = "000000000000001"

 * -------------------------------------------------
 * Criar tags do evento sequencia
 * -------------------------------------------------
 * Criar tag Evento
   oEventoMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.EventoMDFe")
   oEventoMDFe:Versao = "3.00"
 
 * Criar tag DetEventoCanc
   oDetEventoCanc = CreateObject("Unimake.Business.DFe.Xml.MDFe.DetEventoCanc")
   oDetEventoCanc:VersaoEvento = "3.00"
   oDetEventoCanc:NProt = "141200000007987"
   oDetEventoCanc:XJust = "Justificativa para cancelamento do MDFe de teste"

 * Criar tag InfEvento
   oInfEvento = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfEvento")
 
 * Adicionar a tag DetEventoCanc dentro da Tag DetEvento
   oInfEvento:DetEvento = oDetEventoCanc
 
 * Atualizar propriedades da oInfEvento
 * IMPORTANTE: Atualização da propriedade TpEvento deve acontecer depois que o DetEvento recebeu o oDetEventoCanc para que funcione sem erro
   oInfEvento:COrgao = 41 // UFBrasil.PR
   oInfEvento:ChMDFe = "41200210859283000185570010000005671227070615"
   oInfEvento:CNPJ = "10859283000185"
   oInfEvento:DhEvento = DateTime()
   oInfEvento:TpEvento = 110111 // TipoEventoNFe.Cancelamento
   oInfEvento:NSeqEvento = 1
   oInfEvento:TpAmb = 2 // TipoAmbiente.Homologacao

 * Adicionar a tag InfEvento dentro da tag Evento
   oEventoMDFe:InfEvento = oInfEvento

 * Resgatando alguns dados do objeto do XML do evento
   ? "<versao>:", oEventoMDFe:Versao
   ? "<cOrgao>:", oEventoMDFe:InfEvento:COrgao
   ? "<chMDFe>:", oEventoMDFe:InfEvento:ChMDFe
   ? "<nProt>: ", oEventoMDFe:InfEvento:DetEvento:NProt
   ? "<xJust>: ", oEventoMDFe:InfEvento:DetEvento:XJust
   ?
   ?
   Wait   
   
   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
   
   Try 
    * Enviar evento
      oRecepcaoEvento = CreateObject("Unimake.Business.DFe.Servicos.MDFe.RecepcaoEvento")
      oRecepcaoEvento:Executar(oEventoMDFe,  oConfiguracao)
      
	  //Demonstrar o XML retornado pela SEFAZ
      ? oRecepcaoEvento:RetornoWSString
	  ?
	  ?
	  Wait
	  
      ? "CStat do Lote Retornado:", oRecepcaoEvento:Result:InfEvento:CStat, "- XMotivo:", oRecepcaoEvento:Result:InfEvento:XMotivo
 
      if oRecepcaoEvento:Result:InfEvento:CStat == 135 //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo MDFe
         oRecepcaoEvento:GravarXmlDistribuicao("d:\testenfe") //Grava o XML de distribuição
      Else
         //Foi rejeitado, fazer devidos tratamentos	  
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