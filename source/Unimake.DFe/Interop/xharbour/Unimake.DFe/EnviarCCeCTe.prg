* ---------------------------------------------------------------------------------
* Enviar evento de cancelamento da NFe
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarCCeCTe()
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
   oEventoCTe:Versao = "3.00"
   
 * Criar tag <detEvento>
   oDetEventoCCE = CreateObject("Unimake.Business.DFe.Xml.CTe.DetEventoCCE")
   oDetEventoCCE:VersaoEvento = "3.00"
    
 * Criar várias correções dentro do evento
 * Criar a tag <evCCeCTe>
   oEventoCCeCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.EventoCCeCTe")
 
 * Criar a tag <infCorrecao> da 1a correção
   oInfCorrecao = CreateObject("Unimake.Business.DFe.Xml.CTe.InfCorrecao")
   oInfCorrecao:GrupoAlterado = "ide"
   oInfCorrecao:CampoAlterado = "cfop"
   oInfCorrecao:ValorAlterado = "6353"
   oInfCorrecao:NroItemAlterado = ""
 
 * Adicionar o conteúdo da 1a tag de correção <infCorrecao> dentro da tag <evCCeCTe>
   oEventoCCeCTe:AddInfCorrecao(oInfCorrecao)
 
 * Criar a tag <infCorrecao> da 2a correção
   oInfCorrecao = CreateObject("Unimake.Business.DFe.Xml.CTe.InfCorrecao")
   oInfCorrecao:GrupoAlterado = "ide"
   oInfCorrecao:CampoAlterado = "cfop"
   oInfCorrecao:ValorAlterado = "6352"
   oInfCorrecao:NroItemAlterado = ""  
 
 * Adicionar o conteúdo da 2a tag de correção <infCorrecao> dentro da tag <evCCeCTe>
   oEventoCCeCTe:AddInfCorrecao (oInfCorrecao)  
 
 * Criar a tag <infCorrecao> da 3a correção
   oInfCorrecao = CreateObject("Unimake.Business.DFe.Xml.CTe.InfCorrecao")
   oInfCorrecao:GrupoAlterado = "ide"
   oInfCorrecao:CampoAlterado = "cfop"
   oInfCorrecao:ValorAlterado = "6351"
   oInfCorrecao:NroItemAlterado = ""
 
 * Adicionar o conteúdo da 3a tag de correção <infCorrecao> dentro da tag <evCCeCTe>
   oEventoCCeCTe:AddInfCorrecao(oInfCorrecao)
   
 * Atualizar o conteúdo da tag de detalhes do evento com o objeto criado.  
   oDetEventoCCe:EventoCCeCTe = oEventoCCeCTe
 
 * Criar tag <infEvento>
   oInfEvento = CreateObject("Unimake.Business.DFe.Xml.CTe.InfEvento")
 
 * Adicionar o Objeto oDetEventoCCE dentro do objeto DetEvento
   oInfEvento:DetEvento = oDetEventoCCE
 
 * Atualizar propriedades da oInfEvento
 * IMPORTANTE: Atualização da propriedade TpEvento deve acontecer depois que o DetEvento recebeu o oDetEventoCCE para que funcione sem erro
   oInfEvento:COrgao = 41 //UFBrasil.PR
   oInfEvento:ChCTe = "41191006117473000150550010000579281779843610"
   oInfEvento:CNPJ = "06117473000150"
   oInfEvento:DhEvento = DateTime()
   oInfEvento:TpEvento = 110110 //TipoEventoCTe.CartaCorrecao
   oInfEvento:NSeqEvento = 1
   oInfEvento:TpAmb = 2 //TipoAmbiente.Homologacao

 * Adicionar a tag <infEvento> dentro da tag <eventoCTe>
   oEventoCTe:InfEvento = oInfEvento   

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