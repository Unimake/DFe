* ---------------------------------------------------------------------------------
* Enviar evento de cancelamento da CTe
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarCancCTe()
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
   oEventoCTe:Versao = "3.00"

 * Criar tag <detEvento>
   oDetEventoCanc = CreateObject("Unimake.Business.DFe.Xml.CTe.DetEventoCanc")
   oDetEventoCanc:VersaoEvento = "3.00"
   oDetEventoCanc:NProt = "141190000660363"
   oDetEventoCanc:XJust = "Justificativa para cancelamento da NFe de teste"
   
 * Criar tag <infEvento>
   oInfEvento = CreateObject("Unimake.Business.DFe.Xml.CTe.InfEvento")   
   
 * Adicionar o Objeto oDetEventoCCE dentro do objeto DetEvento   
   oInfEvento:DetEvento = oDetEventoCanc
   
 * Atualizar propriedades da oInfEvento
 * IMPORTANTE: Atualização da propriedade TpEvento deve acontecer depois que o DetEvento recebeu o oDetEventoCCE para que funcione sem erro
   oInfEvento:COrgao = 41 //UFBrasil.PR
   oInfEvento:ChCTe = "41191006117473000150550010000579281779843610"
   oInfEvento:CNPJ = "06117473000150"
   oInfEvento:DhEvento = DateTime()
   oInfEvento:TpEvento = 110111 //TipoEventoCTe.Cancelamento
   oInfEvento:NSeqEvento = 1
   oInfEvento:TpAmb = 2 //TipoAmbiente.Homologacao   
    
 * Adicionar a tag <infEvento> dentro da tag <eventoCTe>   
   oEventoCte:InfEvento = oInfEvento 

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