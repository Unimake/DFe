* ---------------------------------------------------------------------------------
* Consulta situacao do BP-e
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif

Function ConsultarSituacaoBPe()
   Local oConfiguracao, oConsSitBPe, oConsultaProtocolo
   Local oErro, oExceptionInterop

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CreateObject( "Unimake.Business.DFe.Servicos.Configuracao" )
   oConfiguracao:TipoDFe = 22 //22=BPe
   oConfiguracao:TipoAmbiente = 2 //Homologacao
   oConfiguracao:CodigoUF = 41 //Parana
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"

 * Criar XML
   oConsSitBPe = CreateObject( "Unimake.Business.DFe.Xml.BPe.ConsSitBPe" )
   oConsSitBPe:ChBPe = "35260712345678000195630010000000011123456780"
   oConsSitBPe:TpAmb = 2 //Homologacao
   oConsSitBPe:Versao = "1.00"

   ? oConsSitBPe:ChBPe
   ? oConsSitBPe:TpAmb
   ? oConsSitBPe:Versao

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CreateObject( "Unimake.Exceptions.ThrowHelper" )

   Try
    * Consumir o servico
      oConsultaProtocolo = CreateObject( "Unimake.Business.DFe.Servicos.BPe.ConsultaProtocolo" )
      oConsultaProtocolo:Executar( oConsSitBPe, oConfiguracao )

      ? oConsultaProtocolo:RetornoWSString
      ? AllTrim(Str(oConsultaProtocolo:Result:CStat)) + " - " + oConsultaProtocolo:Result:XMotivo

   Catch oErro
      ? "Erro Harbour: " + oErro:Description
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
   End

   Wait
Return
