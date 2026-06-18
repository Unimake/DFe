* ---------------------------------------------------------------------------------
* CIOT - Declaracao da operacao de transporte
* ---------------------------------------------------------------------------------
FUNCTION CIOTDeclaracaoOperacaoTransporte()
   LOCAL oConfiguracao, oXmlCIOT, oVeiculo, oOrigemDestino, oDadosCarga
   LOCAL oInfPagamento, oIndicadoresOperacionais, oServico, oExceptionInterop
   LOCAL oConfigID, oServicoID, oXmlID, oErro
   LOCAL xml, idOperacaoTransporte, xmlDistribuicao

 * Gerar previamente o IdOperacaoTransporte
   oConfigID = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfigID.TipoDFe = 19 && 19=CIOT
   oConfigID.TipoEmissao = 1 && Normal
   oConfigID.TipoAmbiente = 2 && Homologacao
   oConfigID.CodigoUF = 91 && AN
   oConfigID.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfigID.CertificadoSenha = "12345678"

   oXmlID = CREATEOBJECT("Unimake.Business.DFe.Xml.CIOT.GerarIdOperacaoTransporte")
   oXmlID.CpfCnpj = "06117473000150"

   oServicoID = CREATEOBJECT("Unimake.Business.DFe.Servicos.CIOT.GerarIdOperacaoTransporte")
   oServicoID.Executar(oXmlID, oConfigID)

   MESSAGEBOX("Id Operacao Transporte: " + oServicoID.Result.IdOperacaoTransporte)

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDFe = 19 && 19=CIOT
   oConfiguracao.TipoEmissao = 1 && Normal
   oConfiguracao.TipoAmbiente = 2 && Homologacao
   oConfiguracao.CodigoUF = 91 && AN
   oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao.CertificadoSenha = "12345678"

 * Criar XML
   oXmlCIOT = CREATEOBJECT("Unimake.Business.DFe.Xml.CIOT.DeclaracaoOperacaoTransporte")
   oXmlCIOT.IdOperacaoTransporte = oServicoID.Result.IdOperacaoTransporte
   oXmlCIOT.TipoOperacao = 1 && Carga lotacao
   oXmlCIOT.CpfCnpjContratado = "12345678901"
   oXmlCIOT.RNTRCContratado = "012345678"
   oXmlCIOT.CpfCnpjContratante = "12345678000195"
   oXmlCIOT.RNTRCContratante = "987654321"
   oXmlCIOT.CpfCnpjDestinatario = "98765432000110"
   oXmlCIOT.ValorFrete = 1500.50
   oXmlCIOT.DataDeclaracao = DATETIME()
   oXmlCIOT.IndContingencia = .F.
   oXmlCIOT.DataInicioViagem = DATETIME(2026, 5, 25, 0, 0, 0)
   oXmlCIOT.DataFimViagem = DATETIME(2026, 5, 26, 0, 0, 0)

   oVeiculo = CREATEOBJECT("Unimake.Business.DFe.Xml.CIOT.Veiculo")
   oVeiculo.Placa = "ABC1D23"
   oVeiculo.RNTRCVeiculo = "012345678"
   oVeiculo.NumeroEixos = "1"
   oXmlCIOT.AddVeiculos(oVeiculo)

   oOrigemDestino = CREATEOBJECT("Unimake.Business.DFe.Xml.CIOT.OrigemDestino")
   oOrigemDestino.Origem = CREATEOBJECT("Unimake.Business.DFe.Xml.CIOT.Origem")
   oOrigemDestino.Origem.CodigoMunicipioOrigem = "4118402"
   oOrigemDestino.Origem.CepOrigem = "87700000"
   oOrigemDestino.Origem.LatitudeOrigem = "-23.073300"
   oOrigemDestino.Origem.LongitudeOrigem = "-52.465300"
   oOrigemDestino.Destino = CREATEOBJECT("Unimake.Business.DFe.Xml.CIOT.Destino")
   oOrigemDestino.Destino.CodigoMunicipioDestino = "4106902"
   oOrigemDestino.Destino.CepDestino = "80000000"
   oOrigemDestino.Destino.LatitudeDestino = "-25.428400"
   oOrigemDestino.Destino.LongitudeDestino = "-49.273300"
   oOrigemDestino.DistanciaPercorrida = "500"
   oXmlCIOT.AddOrigemDestino(oOrigemDestino)

   oDadosCarga = CREATEOBJECT("Unimake.Business.DFe.Xml.CIOT.DadosCarga")
   oDadosCarga.CodigoNaturezaCarga = "0001"
   oDadosCarga.PesoCarga = "1000.00"
   oDadosCarga.CodigoTipoCarga = 5 && Carga geral
   oDadosCarga.AddContratantesCargFrac("12345678000195")
   oDadosCarga.AddContratantesCargFrac("98765432000110")
   oXmlCIOT.DadosCarga = oDadosCarga

   oInfPagamento = CREATEOBJECT("Unimake.Business.DFe.Xml.CIOT.InfPagamento")
   oInfPagamento.TipoPagamento = 6 && Conta corrente
   oInfPagamento.ChavePix = "financeiro@example.com"
   oInfPagamento.CpfCnpjCreditado = "12345678901"
   oInfPagamento.IdentificadorPix = "PIX123456789"
   oInfPagamento.IndPagamento = 0
   oXmlCIOT.AddInfPagamento(oInfPagamento)

   oIndicadoresOperacionais = CREATEOBJECT("Unimake.Business.DFe.Xml.CIOT.IndicadoresOperacionais")
   oIndicadoresOperacionais.IndAltoDesempenho = .F.
   oIndicadoresOperacionais.IndRetornoVazio = .F.
   oIndicadoresOperacionais.ComposicaoVeicular = .F.
   oXmlCIOT.InfIndicadoresOperacionais = oIndicadoresOperacionais

   xml = oXmlCIOT.GerarXMLString()
   MESSAGEBOX(xml)

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")

   TRY
    * Consumir o servico
      oServico = CREATEOBJECT("Unimake.Business.DFe.Servicos.CIOT.DeclaracaoOperacaoTransporte")
      oServico.Executar(oXmlCIOT, oConfiguracao)

      IF oServico.Result.Codigo == "110" && 110=Dados inseridos com sucesso
         MESSAGEBOX("Protocolo: " + oServico.Result.Protocolo + CHR(13) + ;
            "Aviso Transportador: " + oServico.Result.AvisoTransportador + CHR(13) + ;
            "Codigo Verificador: " + oServico.Result.CodigoVerificador + CHR(13) + ;
            "Id Operacao Transporte: " + oServico.Result.IdOperacaoTransporte)

         oServico.GravarXmlDistribuicao("d:\testenfe\xmlciot")

         idOperacaoTransporte = oServico.Result.IdOperacaoTransporte
         xmlDistribuicao = oServico.GetDeclaracaoOperacaoTransporteProcResult()
      ELSE
         IF VARTYPE(oServico.Result.Temp) == "O"
            MESSAGEBOX(oServico.Result.Temp.Error + " - " + oServico.Result.Temp.Message)
         ELSE
            MESSAGEBOX(oServico.Result.Codigo + " - " + oServico.Result.Mensagem)
         ENDIF
      ENDIF

   CATCH TO oErro
    * Excecao do FOXPRO
      MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10)) + " - Message: " + oErro.Message)

    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())
   ENDTRY
RETURN
