* ---------------------------------------------------------------------------------
* CIOT - Consultar situacao do transportador
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif

Function CIOTConsultarSituacaoTransportador()
   Local oConfiguracao, oXmlCIOT, oServico
   Local oErro, oExceptionInterop

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CreateObject( "Unimake.Business.DFe.Servicos.Configuracao" )
   oConfiguracao:TipoDFe = 19 //19=CIOT
   oConfiguracao:TipoEmissao = 1 //Normal
   oConfiguracao:TipoAmbiente = 2 //Homologacao
   oConfiguracao:CodigoUF = 91 //AN
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"

 * Criar XML
   oXmlCIOT = CreateObject( "Unimake.Business.DFe.Xml.CIOT.ConsultarSituacaoTransportador" )
   oXmlCIOT:CpfCnpjInteressado = "12345678000195"
   oXmlCIOT:CpfCnpjTransportador = "12345678901"
   oXmlCIOT:RNTRCTransportador = "012345678"

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CreateObject( "Unimake.Exceptions.ThrowHelper" )

   Try
    * Consumir o servico
      oServico = CreateObject( "Unimake.Business.DFe.Servicos.CIOT.ConsultarSituacaoTransportador" )
      oServico:Executar( oXmlCIOT, oConfiguracao )

      If ValType( oServico:Result:Temp ) = "O"
         ? oServico:Result:Temp:Error + " - " + oServico:Result:Temp:Message
      Else
         ? "CPF/CNPJ Transportador: " + oServico:Result:CpfCnpjTransportador
         ? "RNTRC Transportador: " + oServico:Result:RNTRCTransportador
         ? "Nome/Razao Social: " + oServico:Result:NomeRazaoSocialTransportador
         ? "Mensagem: " + oServico:Result:Mensagem
      Endif

   Catch oErro
      ? "Erro Harbour: " + oErro:Description
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
   End

   Wait
Return
