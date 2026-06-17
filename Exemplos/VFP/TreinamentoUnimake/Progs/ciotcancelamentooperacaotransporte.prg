* ---------------------------------------------------------------------------------
* CIOT - Cancelamento da operacao de transporte
* ---------------------------------------------------------------------------------
FUNCTION CIOTCancelamentoOperacaoTransporte()
   LOCAL oConfiguracao, oXmlCIOT, oServico
   LOCAL oErro, oExceptionInterop

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDFe = 19 && 19=CIOT
   oConfiguracao.TipoEmissao = 1 && Normal
   oConfiguracao.TipoAmbiente = 2 && Homologacao
   oConfiguracao.CodigoUF = 91 && AN
   oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao.CertificadoSenha = "12345678"

 * Criar XML
   oXmlCIOT = CREATEOBJECT("Unimake.Business.DFe.Xml.CIOT.CancelamentoOperacaoTransporte")
   oXmlCIOT.CodigoIdentificacaoOperacao = "1234567890123456"
   oXmlCIOT.MotivoCancelamento = "Operacao nao realizada"

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")

   TRY
    * Consumir o servico
      oServico = CREATEOBJECT("Unimake.Business.DFe.Servicos.CIOT.CancelamentoOperacaoTransporte")
      oServico.Executar(oXmlCIOT, oConfiguracao)

      IF VARTYPE(oServico.Result.Temp) == "O"
         MESSAGEBOX(oServico.Result.Temp.Error + " - " + oServico.Result.Temp.Message)
      ELSE
         MESSAGEBOX("Protocolo: " + oServico.Result.Protocolo + CHR(13) + ;
            "Codigo Identificacao Operacao: " + oServico.Result.CodigoIdentificacaoOperacao + CHR(13) + ;
            "Mensagem: " + oServico.Result.Mensagem)

         oServico.GravarXmlDistribuicao("d:\testenfe\xmlciot")
         MESSAGEBOX(oServico.GetCancelamentoOperacaoTransporteProcResult())
      ENDIF

   CATCH TO oErro
    * Excecao do FOXPRO
      MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10)) + " - Message: " + oErro.Message)

    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())
   ENDTRY
RETURN

