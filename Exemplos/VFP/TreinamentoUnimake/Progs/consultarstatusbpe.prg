* ---------------------------------------------------------------------------------
* Consulta status do servico do BP-e
* ---------------------------------------------------------------------------------
FUNCTION ConsultarStatusBPe()
   LOCAL oConfiguracao, oConsStatServBPe, oStatusServico
   LOCAL oErro, oExceptionInterop

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDFe = 22 && 22=BPe
   oConfiguracao.TipoAmbiente = 2 && Homologacao
   oConfiguracao.CodigoUF = 41 && Parana
   oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao.CertificadoSenha = "12345678"

 * Criar XML
   oConsStatServBPe = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.ConsStatServBPe")
   oConsStatServBPe.TpAmb = 2 && Homologacao
   oConsStatServBPe.Versao = "1.00"

   MESSAGEBOX("Tipo ambiente: " + TRANSFORM(oConsStatServBPe.TpAmb) + CHR(13) + ;
      "Versao: " + oConsStatServBPe.Versao)

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")

   TRY
    * Consumir o servico
      oStatusServico = CREATEOBJECT("Unimake.Business.DFe.Servicos.BPe.StatusServico")
      oStatusServico.Executar(oConsStatServBPe, oConfiguracao)

      MESSAGEBOX(oStatusServico.RetornoWSString)
      MESSAGEBOX(ALLTRIM(STR(oStatusServico.Result.CStat, 10)) + " - " + oStatusServico.Result.XMotivo)

   CATCH TO oErro
    * Excecao do FOXPRO
      MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo, 10)) + " - Message: " + oErro.Message)

    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(), 20)) + " - Message: " + oExceptionInterop.GetMessage())
   ENDTRY
RETURN
