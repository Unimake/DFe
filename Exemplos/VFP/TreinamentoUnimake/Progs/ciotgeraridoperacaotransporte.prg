* ---------------------------------------------------------------------------------
* CIOT - Gerar identificador da operacao de transporte
* ---------------------------------------------------------------------------------
FUNCTION CIOTGerarIdOperacaoTransporte()
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
   oXmlCIOT = CREATEOBJECT("Unimake.Business.DFe.Xml.CIOT.GerarIdOperacaoTransporte")
   oXmlCIOT.CpfCnpj = "06117473000150"

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")

   TRY
    * Consumir o servico
      oServico = CREATEOBJECT("Unimake.Business.DFe.Servicos.CIOT.GerarIdOperacaoTransporte")
      oServico.Executar(oXmlCIOT, oConfiguracao)

      MESSAGEBOX("Id Operacao Transporte: " + oServico.Result.IdOperacaoTransporte)

      oServico.GravarXmlDistribuicao("d:\testenfe\xmlciot")
      MESSAGEBOX(oServico.GetGerarIdOperacaoTransporteProcResult())

   CATCH TO oErro
    * Excecao do FOXPRO
      MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10)) + " - Message: " + oErro.Message)

    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())
   ENDTRY
RETURN

