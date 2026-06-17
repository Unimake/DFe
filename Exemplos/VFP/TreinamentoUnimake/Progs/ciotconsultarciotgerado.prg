* ---------------------------------------------------------------------------------
* CIOT - Consultar CIOT gerado
* ---------------------------------------------------------------------------------
FUNCTION CIOTConsultarCIOTGerado()
   LOCAL oConfiguracao, oXmlCIOT, oServico
   LOCAL oErro, oExceptionInterop
   LOCAL I, mensagens

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDFe = 19 && 19=CIOT
   oConfiguracao.TipoEmissao = 1 && Normal
   oConfiguracao.TipoAmbiente = 2 && Homologacao
   oConfiguracao.CodigoUF = 91 && AN
   oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao.CertificadoSenha = "12345678"

 * Criar XML
   oXmlCIOT = CREATEOBJECT("Unimake.Business.DFe.Xml.CIOT.ConsultarCIOTGerado")
   oXmlCIOT.CodigoIdentificacaoOperacao = "123456789012"
   oXmlCIOT.AnoDeclaracao = 2026

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")

   TRY
    * Consumir o servico
      oServico = CREATEOBJECT("Unimake.Business.DFe.Servicos.CIOT.ConsultarCIOTGerado")
      oServico.Executar(oXmlCIOT, oConfiguracao)

      IF VARTYPE(oServico.Result.Temp) == "O"
         MESSAGEBOX(oServico.Result.Temp.Error + " - " + oServico.Result.Temp.Message)
      ELSE
         mensagens = ""

         FOR I = 0 TO oServico.Result.GetMensagemCount() - 1
            mensagens = mensagens + oServico.Result.GetMensagem(I) + CHR(13) + CHR(10)
         ENDFOR

         MESSAGEBOX("Codigo Identificacao Operacao: " + oServico.Result.CodigoIdentificacaoOperacao + CHR(13) + ;
            "Mensagens: " + CHR(13) + mensagens)
      ENDIF

   CATCH TO oErro
    * Excecao do FOXPRO
      MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10)) + " - Message: " + oErro.Message)

    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())
   ENDTRY
RETURN

