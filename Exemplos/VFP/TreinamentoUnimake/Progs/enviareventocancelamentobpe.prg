* ---------------------------------------------------------------------------------
* Enviar evento de cancelamento do BP-e
* ---------------------------------------------------------------------------------
FUNCTION EnviarEventoCancelamentoBPe()
   LOCAL oConfiguracao, oEventoBPe, oRecepcaoEvento
   LOCAL oErro, oExceptionInterop

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDFe = 22 && 22=BPe
   oConfiguracao.TipoEmissao = 1 && Normal
   oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao.CertificadoSenha = "12345678"

 * Criar XML
   oEventoBPe = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.EventoBPe")
   oEventoBPe.Versao = "1.00"
   oEventoBPe.InfEvento = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.InfEventoBPe")
   oEventoBPe.InfEvento.COrgao = 41 && Parana
   oEventoBPe.InfEvento.TpAmb = 2 && Homologacao
   oEventoBPe.InfEvento.CNPJ = "00000000000199"
   oEventoBPe.InfEvento.ChBPe = "35260712345678000195630010000000011123456780"
   oEventoBPe.InfEvento.DhEvento = DATETIME()
   oEventoBPe.InfEvento.TpEvento = 110111 && Cancelamento
   oEventoBPe.InfEvento.NSeqEvento = 1
   oEventoBPe.InfEvento.DetEvento = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.DetEventoBPe")
   oEventoBPe.InfEvento.DetEvento.VersaoEvento = "1.00"
   oEventoBPe.InfEvento.DetEvento.EvCancBPe = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.EvCancBPe")
   oEventoBPe.InfEvento.DetEvento.EvCancBPe.NProt = "123456789012345"
   oEventoBPe.InfEvento.DetEvento.EvCancBPe.XJust = "Justificativa de teste valida"

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")

   TRY
      MESSAGEBOX(oEventoBPe.GerarXMLString())

    * Consumir o servico
      oRecepcaoEvento = CREATEOBJECT("Unimake.Business.DFe.Servicos.BPe.RecepcaoEvento")
      oRecepcaoEvento.Executar(oEventoBPe, oConfiguracao)

      MESSAGEBOX(oRecepcaoEvento.RetornoWSString)
      MESSAGEBOX(ALLTRIM(STR(oRecepcaoEvento.Result.InfEvento.CStat, 10)) + " - " + oRecepcaoEvento.Result.InfEvento.XMotivo)

      DO CASE
      CASE oRecepcaoEvento.Result.InfEvento.CStat = 134
         oRecepcaoEvento.GravarXmlDistribuicao("d:\testenfe")
      CASE oRecepcaoEvento.Result.InfEvento.CStat = 135
         oRecepcaoEvento.GravarXmlDistribuicao("d:\testenfe")
      CASE oRecepcaoEvento.Result.InfEvento.CStat = 136
         oRecepcaoEvento.GravarXmlDistribuicao("d:\testenfe")
      ENDCASE

   CATCH TO oErro
    * Excecao do FOXPRO
      MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo, 10)) + " - Message: " + oErro.Message)

    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(), 20)) + " - Message: " + oExceptionInterop.GetMessage())
   ENDTRY
RETURN
