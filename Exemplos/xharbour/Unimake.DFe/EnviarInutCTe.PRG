* ---------------------------------------------------------------------------------
* Enviar xml de inutilização CTe
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarInutCTe()
   Local oErro, oExceptionInterop
   Local oConfiguracao
   Local oInutCTe, oInfInut, oInutilizacao
   
 * Criar configuraçao básica para consumir o serviço
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDfe = 2 // 0=CTe
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"   
   
 * Criar o grupo de tag <inutCTe>
   oInutCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.InutCTe")
   oInutCTe:Versao = "3.00"
   
 * Criar tag do lote de eventos <infInut>   
   oInfInut = CreateObject("Unimake.Business.DFe.Xml.CTe.InutCTeInfInut")
   oInfInut:Ano = "19"
   oInfInut:CNPJ = "06117473000150"
   oInfInut:CUF = 31 //UFBrasil.MG
   oInfInut:Mod = 57 //ModeloDFe.CTe
   oInfInut:NCTIni = 57919
   oInfInut:NCTFin = 57919
   oInfInut:Serie = 1
   oInfInut:TpAmb = 2 //TipoAmbiente.Homologacao
   oInfInut:XJust = "Justificativa da inutilizacao de teste"   
   
 * Adicionar o conteúdo da tag <infInut> dentro do grupo de tag <infCTe>   
   oInutCte:InfInut := oInfInut

 * Resgatando alguns dados do objeto do XML de inutilização
   ? oInutCTe:Versao
   ? oInutCTe:InfInut:Ano
   ? oInutCTe:InfInut:CNPJ
   ? oInutCTe:InfInut:CUF
   ? oInutCTe:InfInut:XJust
   ?
   Wait   
   Cls 


   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
   
   Try 
    * Enviar a Inutilização
      oInutilizacao = CreateObject("Unimake.Business.DFe.Servicos.CTe.Inutilizacao")
      oInutilizacao:SetXMLConfiguracao(oInutCTe, oConfiguracao)

	  cls
	  ? oInutilizacao:GetConteudoXMLAssinado()
	  ?
	  ?
	  ?
	  Wait
	  
	  cls
	  
	  oInutilizacao:Executar(oInutCTe, oConfiguracao)
	  
	  ? "CStat Retornado:", oInutilizacao:Result:InfInut:CStat
	  ? "XMotivo Retornado: ", oInutilizacao:Result:InfInut:XMotivo
	  ?
	  ?
	  Wait	
	  
	  Cls
	  
	  ? oInutilizacao:RetornoWSString
	  ?
	  ?
	  Wait
	  Cls
	  
      SWITCH oInutilizacao:Result:InfInut:CStat
         CASE 102 //Inutilização Homologada
              oInutilizacao:GravarXmlDistribuicao("tmp\testenfe") //Grava o XML de distribuição
              Exit
   				 
        #Ifdef __XHARBOUR__
         DEFAULT
        #Else
         OTHERWISE    
        #endif
              // Inutilização rejeitada
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