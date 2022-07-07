* ---------------------------------------------------------------------------------
* Enviar evento de cancelamento da NFe
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarCCeCTeDesserializacao()
   Local oErro, oExceptionInterop
   Local oConfiguracao
   Local oDetEventoCCE, oEventoCCeCTe, oInfCorrecao, oInfEvento, oRecepcaoEvento
   
 * Criar configuraçao básica para consumir o serviço
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDfe = 2 // 0=CTe
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

 * Criar tag do lote de eventos <eventoCTe>
   oEventoCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.EventoCTe")
   
 * Desserializar o XML no Objeto  
   oEventoCTe = oEventoCTe:LoadFromFile("C:\projetos\uninfe\exemplos\CTe 3.00\cce35150107565416000104570000000012301000012300-ped-eve.xml")
   
 * Resgatando alguns dados do objeto do XML do evento
   ? oEventoCTe:Versao
   ? oEventoCTe:InfEvento:COrgao
   ? oEventoCTe:InfEvento:CNPJ //Demonstrar o CNPJ infomrado no XML
   ? oEventoCTe:InfEvento:DhEvento //Demonstrar o CNPJ infomrado no XML   
   ?
   Wait   
   
   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
   
   Try 
    * Enviar evento
      oRecepcaoEvento = CreateObject("Unimake.Business.DFe.Servicos.CTe.RecepcaoEvento")
      oRecepcaoEvento:SetXMLConfiguracao(oEventoCTe, oConfiguracao)
	  
	  ? oRecepcaoEvento:GetConteudoXMLAssinado()
	  ?
	  ?
	  Wait
	  
	  cls
	  
      oRecepcaoEvento:Executar(oEventoCTe, oConfiguracao)

      ? "CStat retornado:", oRecepcaoEvento:Result:InfEvento:CStat, "- XMotivo:", oRecepcaoEvento:Result:InfEvento:XMotivo
	  ? 
	  ?
	  Wait
  		  
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