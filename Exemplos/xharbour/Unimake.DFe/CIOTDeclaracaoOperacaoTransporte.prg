* ---------------------------------------------------------------------------------
* CIOT - Declaracao da operacao de transporte
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif

Function CIOTDeclaracaoOperacaoTransporte()
   Local oConfiguracao, oXmlCIOT, oVeiculo, oOrigemDestino, oDadosCarga
   Local oInfPagamento, oIndicadoresOperacionais, oServico, oExceptionInterop
   Local oConfigID, oServicoID, oXmlID, oErro
   Local xml, idOperacaoTransporte, xmlDistribuicao

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CreateObject( "Unimake.Exceptions.ThrowHelper" )

   Try
    * Gerar previamente o IdOperacaoTransporte
      oConfigID = CreateObject( "Unimake.Business.DFe.Servicos.Configuracao" )
      oConfigID:TipoDFe = 19 //19=CIOT
      oConfigID:TipoEmissao = 1 //Normal
      oConfigID:TipoAmbiente = 2 //Homologacao
      oConfigID:CodigoUF = 91 //AN
      oConfigID:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
      oConfigID:CertificadoSenha = "12345678"

      oXmlID = CreateObject( "Unimake.Business.DFe.Xml.CIOT.GerarIdOperacaoTransporte" )
      oXmlID:CpfCnpj = "06117473000150"

      oServicoID = CreateObject( "Unimake.Business.DFe.Servicos.CIOT.GerarIdOperacaoTransporte" )
      oServicoID:Executar( oXmlID, oConfigID )

      ? "Id Operacao Transporte: " + oServicoID:Result:IdOperacaoTransporte

    * Criar configuracao basica para consumir o servico
      oConfiguracao = CreateObject( "Unimake.Business.DFe.Servicos.Configuracao" )
      oConfiguracao:TipoDFe = 19 //19=CIOT
      oConfiguracao:TipoEmissao = 1 //Normal
      oConfiguracao:TipoAmbiente = 2 //Homologacao
      oConfiguracao:CodigoUF = 91 //AN
      oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
      oConfiguracao:CertificadoSenha = "12345678"

    * Criar XML
      oXmlCIOT = CreateObject( "Unimake.Business.DFe.Xml.CIOT.DeclaracaoOperacaoTransporte" )
      oXmlCIOT:IdOperacaoTransporte = oServicoID:Result:IdOperacaoTransporte
      oXmlCIOT:TipoOperacao = 1 //Carga lotacao
      oXmlCIOT:CpfCnpjContratado = "12345678901"
      oXmlCIOT:RNTRCContratado = "012345678"
      oXmlCIOT:CpfCnpjContratante = "12345678000195"
      oXmlCIOT:RNTRCContratante = "987654321"
      oXmlCIOT:CpfCnpjDestinatario = "98765432000110"
      oXmlCIOT:ValorFrete = 1500.50
      oXmlCIOT:DataDeclaracao = DateTime()
      oXmlCIOT:IndContingencia = .F.
      oXmlCIOT:DataInicioViagem = Left(DtoS(Date()),4)+"-"+SubStr(DtoS(Date()),5,2)+"-"+Right(DtoS(Date()),2)
      oXmlCIOT:DataFimViagem = Left(DtoS(Date()),4)+"-"+SubStr(DtoS(Date()),5,2)+"-"+Right(DtoS(Date()),2)

      oVeiculo = CreateObject( "Unimake.Business.DFe.Xml.CIOT.Veiculo" )
      oVeiculo:Placa = "ABC1D23"
      oVeiculo:RNTRCVeiculo = "012345678"
      oVeiculo:NumeroEixos = "1"
      oXmlCIOT:AddVeiculos( oVeiculo )

      oOrigemDestino = CreateObject( "Unimake.Business.DFe.Xml.CIOT.OrigemDestino" )
      oOrigemDestino:Origem = CreateObject( "Unimake.Business.DFe.Xml.CIOT.Origem" )
      oOrigemDestino:Origem:CodigoMunicipioOrigem = "4118402"
      oOrigemDestino:Origem:CepOrigem = "87700000"
      oOrigemDestino:Origem:LatitudeOrigem = "-23.073300"
      oOrigemDestino:Origem:LongitudeOrigem = "-52.465300"
      oOrigemDestino:Destino = CreateObject( "Unimake.Business.DFe.Xml.CIOT.Destino" )
      oOrigemDestino:Destino:CodigoMunicipioDestino = "4106902"
      oOrigemDestino:Destino:CepDestino = "80000000"
      oOrigemDestino:Destino:LatitudeDestino = "-25.428400"
      oOrigemDestino:Destino:LongitudeDestino = "-49.273300"
      oOrigemDestino:DistanciaPercorrida = "500"
      oXmlCIOT:AddOrigemDestino( oOrigemDestino )

      oDadosCarga = CreateObject( "Unimake.Business.DFe.Xml.CIOT.DadosCarga" )
      oDadosCarga:CodigoNaturezaCarga = "0001"
      oDadosCarga:PesoCarga = "1000.00"
      oDadosCarga:CodigoTipoCarga = 5 //Carga geral
      oDadosCarga:AddContratantesCargFrac( "12345678000195" )
      oDadosCarga:AddContratantesCargFrac( "98765432000110" )
      oXmlCIOT:DadosCarga = oDadosCarga

      oInfPagamento = CreateObject( "Unimake.Business.DFe.Xml.CIOT.InfPagamento" )
      oInfPagamento:TipoPagamento = 6 //Conta corrente
      oInfPagamento:ChavePix = "financeiro@example.com"
      oInfPagamento:CpfCnpjCreditado = "12345678901"
      oInfPagamento:IdentificadorPix = "PIX123456789"
      oInfPagamento:IndPagamento = 0
      oXmlCIOT:AddInfPagamento( oInfPagamento )

      oIndicadoresOperacionais = CreateObject( "Unimake.Business.DFe.Xml.CIOT.IndicadoresOperacionais" )
      oIndicadoresOperacionais:IndAltoDesempenho = .F.
      oIndicadoresOperacionais:IndRetornoVazio = .F.
      oIndicadoresOperacionais:ComposicaoVeicular = .F.
      oXmlCIOT:InfIndicadoresOperacionais = oIndicadoresOperacionais

      xml = oXmlCIOT:GerarXMLString()
      ? xml

    * Consumir o servico
      oServico = CreateObject( "Unimake.Business.DFe.Servicos.CIOT.DeclaracaoOperacaoTransporte" )
      oServico:Executar( oXmlCIOT, oConfiguracao )

      If oServico:Result:Codigo = "110" //110=Dados inseridos com sucesso
         ? "Protocolo: " + oServico:Result:Protocolo
         ? "Aviso Transportador: " + oServico:Result:AvisoTransportador
         ? "Codigo Verificador: " + oServico:Result:CodigoVerificador
         ? "Id Operacao Transporte: " + oServico:Result:IdOperacaoTransporte

         oServico:GravarXmlDistribuicao( "d:\testenfe\xmlciot" )

         idOperacaoTransporte = oServico:Result:IdOperacaoTransporte
         xmlDistribuicao = oServico:GetDeclaracaoOperacaoTransporteProcResult()
         ? xmlDistribuicao
      Else
         If ValType( oServico:Result:Temp ) = "O"
            ? oServico:Result:Temp:Error + " - " + oServico:Result:Temp:Message
         Else
            ? oServico:Result:Codigo + " - " + oServico:Result:Mensagem
         Endif
      Endif

   Catch oErro
      ? "Erro Harbour: " + oErro:Description
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
   End

   Wait
Return
