* ---------------------------------------------------------------------------------
* Consulta situacao do BP-e
* ---------------------------------------------------------------------------------
FUNCTION ConsultarSituacaoBPe()
   LOCAL oConfiguracao, oConsSitBPe, oConsultaProtocolo
   LOCAL oErro, oExceptionInterop

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDFe = 22 && 22=BPe
   oConfiguracao.TipoAmbiente = 2 && Homologacao
   oConfiguracao.CodigoUF = 41 && Parana
   oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao.CertificadoSenha = "12345678"

 * Criar XML
   oConsSitBPe = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.ConsSitBPe")
   oConsSitBPe.ChBPe = "35260712345678000195630010000000011123456780"
   oConsSitBPe.TpAmb = 2 && Homologacao
   oConsSitBPe.Versao = "1.00"

   MESSAGEBOX("Chave BP-e: " + oConsSitBPe.ChBPe + CHR(13) + ;
      "Tipo ambiente: " + TRANSFORM(oConsSitBPe.TpAmb) + CHR(13) + ;
      "Versao: " + oConsSitBPe.Versao)

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")

   TRY
    * Consumir o servico
      oConsultaProtocolo = CREATEOBJECT("Unimake.Business.DFe.Servicos.BPe.ConsultaProtocolo")
      oConsultaProtocolo.Executar(oConsSitBPe, oConfiguracao)

      MESSAGEBOX(oConsultaProtocolo.RetornoWSString)
      MESSAGEBOX(ALLTRIM(STR(oConsultaProtocolo.Result.CStat, 10)) + " - " + oConsultaProtocolo.Result.XMotivo)

   CATCH TO oErro
    * Excecao do FOXPRO
      MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo, 10)) + " - Message: " + oErro.Message)

    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(), 20)) + " - Message: " + oExceptionInterop.GetMessage())
   ENDTRY
RETURN
