* ---------------------------------------------------------------------------------
* CIOT - Consultar situacao do transportador
* ---------------------------------------------------------------------------------
FUNCTION CIOTConsultarSituacaoTransportador()
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
   oXmlCIOT = CREATEOBJECT("Unimake.Business.DFe.Xml.CIOT.ConsultarSituacaoTransportador")
   oXmlCIOT.CpfCnpjInteressado = "12345678000195"
   oXmlCIOT.CpfCnpjTransportador = "12345678901"
   oXmlCIOT.RNTRCTransportador = "012345678"

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")

   TRY
    * Consumir o servico
      oServico = CREATEOBJECT("Unimake.Business.DFe.Servicos.CIOT.ConsultarSituacaoTransportador")
      oServico.Executar(oXmlCIOT, oConfiguracao)

      IF VARTYPE(oServico.Result.Temp) == "O"
         MESSAGEBOX(oServico.Result.Temp.Error + " - " + oServico.Result.Temp.Message)
      ELSE
         MESSAGEBOX("CPF/CNPJ Transportador: " + oServico.Result.CpfCnpjTransportador + CHR(13) + ;
            "RNTRC Transportador: " + oServico.Result.RNTRCTransportador + CHR(13) + ;
            "Nome/Razao Social: " + oServico.Result.NomeRazaoSocialTransportador + CHR(13) + ;
            "Mensagem: " + oServico.Result.Mensagem)
      ENDIF

   CATCH TO oErro
    * Excecao do FOXPRO
      MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10)) + " - Message: " + oErro.Message)

    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())
   ENDTRY
RETURN

