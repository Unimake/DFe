* ---------------------------------------------------------------------------------
* CIOT - Encerramento da operacao de transporte
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif

Function CIOTEncerramentoOperacaoTransporte()
   Local oConfiguracao, oXmlCIOT, oOrigemDestino, oDadosCarga, oServico
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
   oXmlCIOT = CreateObject( "Unimake.Business.DFe.Xml.CIOT.EncerramentoOperacaoTransporte" )
   oXmlCIOT:CodigoIdentificacaoOperacao = "1234567890123456"

   oOrigemDestino = CreateObject( "Unimake.Business.DFe.Xml.CIOT.OrigemDestino" )
   oOrigemDestino:Origem = CreateObject( "Unimake.Business.DFe.Xml.CIOT.Origem" )
   oOrigemDestino:Origem:CodigoMunicipioOrigem = "4118402"
   oOrigemDestino:Origem:CepOrigem = "87700000"
   oOrigemDestino:Destino = CreateObject( "Unimake.Business.DFe.Xml.CIOT.Destino" )
   oOrigemDestino:Destino:CodigoMunicipioDestino = "4106902"
   oOrigemDestino:Destino:CepDestino = "80000000"
   oOrigemDestino:DistanciaPercorrida = "500"
   oOrigemDestino:QtdViagens = "1"
   oXmlCIOT:AddOrigemDestino( oOrigemDestino )

   oDadosCarga = CreateObject( "Unimake.Business.DFe.Xml.CIOT.DadosCargaEncerramento" )
   oDadosCarga:PesoTotalCarga = "1000.00"
   oXmlCIOT:DadosCarga = oDadosCarga

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CreateObject( "Unimake.Exceptions.ThrowHelper" )

   Try
    * Consumir o servico
      oServico = CreateObject( "Unimake.Business.DFe.Servicos.CIOT.EncerramentoOperacaoTransporte" )
      oServico:Executar( oXmlCIOT, oConfiguracao )

      If ValType( oServico:Result:Temp ) = "O"
         ? oServico:Result:Temp:Error + " - " + oServico:Result:Temp:Message
      Else
         ? "Protocolo: " + oServico:Result:Protocolo
         ? "Codigo Identificacao Operacao: " + oServico:Result:CodigoIdentificacaoOperacao
         ? "Mensagem: " + oServico:Result:Mensagem

         oServico:GravarXmlDistribuicao( "d:\testenfe\xmlciot" )
         ? oServico:GetEncerramentoOperacaoTransporteProcResult()
      Endif

   Catch oErro
      ? "Erro Harbour: " + oErro:Description
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
   End

   Wait
Return
