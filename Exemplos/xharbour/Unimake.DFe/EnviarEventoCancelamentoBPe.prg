* ---------------------------------------------------------------------------------
* Enviar evento de cancelamento do BP-e
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif

Function EnviarEventoCancelamentoBPe()
   Local oConfiguracao, oEventoBPe, oRecepcaoEvento
   Local oErro, oExceptionInterop

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CreateObject( "Unimake.Business.DFe.Servicos.Configuracao" )
   oConfiguracao:TipoDFe = 22 //22=BPe
   oConfiguracao:TipoEmissao = 1 //Normal
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"

 * Criar XML
   oEventoBPe = CreateObject( "Unimake.Business.DFe.Xml.BPe.EventoBPe" )
   oEventoBPe:Versao = "1.00"
   oEventoBPe:InfEvento = CreateObject( "Unimake.Business.DFe.Xml.BPe.InfEventoBPe" )
   oEventoBPe:InfEvento:COrgao = 41 //Parana
   oEventoBPe:InfEvento:TpAmb = 2 //Homologacao
   oEventoBPe:InfEvento:CNPJ = "00000000000199"
   oEventoBPe:InfEvento:ChBPe = "35260712345678000195630010000000011123456780"
   oEventoBPe:InfEvento:DhEvento = DateTime()
   oEventoBPe:InfEvento:TpEvento = 110111 //Cancelamento
   oEventoBPe:InfEvento:NSeqEvento = 1
   oEventoBPe:InfEvento:DetEvento = CreateObject( "Unimake.Business.DFe.Xml.BPe.DetEventoBPe" )
   oEventoBPe:InfEvento:DetEvento:VersaoEvento = "1.00"
   oEventoBPe:InfEvento:DetEvento:EvCancBPe = CreateObject( "Unimake.Business.DFe.Xml.BPe.EvCancBPe" )
   oEventoBPe:InfEvento:DetEvento:EvCancBPe:NProt = "123456789012345"
   oEventoBPe:InfEvento:DetEvento:EvCancBPe:XJust = "Justificativa de teste valida"

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CreateObject( "Unimake.Exceptions.ThrowHelper" )

   Try
      ? oEventoBPe:GerarXMLString()

    * Consumir o servico
      oRecepcaoEvento = CreateObject( "Unimake.Business.DFe.Servicos.BPe.RecepcaoEvento" )
      oRecepcaoEvento:Executar( oEventoBPe, oConfiguracao )

      ? oRecepcaoEvento:RetornoWSString
	  
      ? AllTrim(Str(oRecepcaoEvento:Result:InfEvento:CStat)) + " - " + oRecepcaoEvento:Result:InfEvento:XMotivo

      Do Case
      Case oRecepcaoEvento:Result:InfEvento:CStat = 134
         oRecepcaoEvento:GravarXmlDistribuicao( "d:\testenfe" )
      Case oRecepcaoEvento:Result:InfEvento:CStat = 135
         oRecepcaoEvento:GravarXmlDistribuicao( "d:\testenfe" )
      Case oRecepcaoEvento:Result:InfEvento:CStat = 136
         oRecepcaoEvento:GravarXmlDistribuicao( "d:\testenfe" )
      EndCase

   Catch oErro
      ? "Erro Harbour: " + oErro:Description
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
   End

   Wait
Return
