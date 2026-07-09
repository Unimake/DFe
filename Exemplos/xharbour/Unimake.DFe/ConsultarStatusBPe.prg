* ---------------------------------------------------------------------------------
* Consulta status do servico do BP-e
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif

Function ConsultarStatusBPe()
   Local oConfiguracao, oConsStatServBPe, oStatusServico
   Local oErro, oExceptionInterop

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CreateObject( "Unimake.Business.DFe.Servicos.Configuracao" )
   oConfiguracao:TipoDFe = 22 //22=BPe
   oConfiguracao:TipoAmbiente = 2 //Homologacao
   oConfiguracao:CodigoUF = 41 //Parana
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"

 * Criar XML
   oConsStatServBPe = CreateObject( "Unimake.Business.DFe.Xml.BPe.ConsStatServBPe" )
   oConsStatServBPe:TpAmb = 2 //Homologacao
   oConsStatServBPe:Versao = "1.00"

   ? oConsStatServBPe:TpAmb
   ? oConsStatServBPe:Versao

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CreateObject( "Unimake.Exceptions.ThrowHelper" )

   Try
    * Consumir o servico
      oStatusServico = CreateObject( "Unimake.Business.DFe.Servicos.BPe.StatusServico" )
      oStatusServico:Executar( oConsStatServBPe, oConfiguracao )

      ? oStatusServico:RetornoWSString
      ? AllTrim(Str(oStatusServico:Result:CStat)) + " - " + oStatusServico:Result:XMotivo

   Catch oErro
      ? "Erro Harbour: " + oErro:Description
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
   End

   Wait
Return
