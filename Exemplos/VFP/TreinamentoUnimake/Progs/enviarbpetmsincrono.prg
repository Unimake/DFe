* ---------------------------------------------------------------------------------
* Enviar BP-e TM modo sincrono
* ---------------------------------------------------------------------------------
FUNCTION EnviarBPeTMSincrono()
   LOCAL oConfiguracao, oBPeTM, oDetBPeTM, oDet, oComp, oPgto, oAutorizacao
   LOCAL oErro, oExceptionInterop, xmlDistribuicao

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDFe = 22 && 22=BPe
   oConfiguracao.TipoEmissao = 1 && Normal
   oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao.CertificadoSenha = "12345678"

 * Criar XML
   oBPeTM = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.BPeTM")
   oBPeTM.InfBPe = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.InfBPe")
   oBPeTM.InfBPe.Versao = "1.00"

   oBPeTM.InfBPe.Ide = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.Ide")
   oBPeTM.InfBPe.Ide.CUF = 35 && Sao Paulo
   oBPeTM.InfBPe.Ide.TpAmb = 2 && Homologacao
   oBPeTM.InfBPe.Ide.Mod = 63 && BP-e
   oBPeTM.InfBPe.Ide.Serie = 1
   oBPeTM.InfBPe.Ide.NBP = 1
   oBPeTM.InfBPe.Ide.CBP = "12345678"
   oBPeTM.InfBPe.Ide.Modal = 1
   oBPeTM.InfBPe.Ide.DhEmi = DATETIME()
   oBPeTM.InfBPe.Ide.DCompet = DATE(2026, 7, 6)
   oBPeTM.InfBPe.Ide.TpEmis = 1
   oBPeTM.InfBPe.Ide.VerProc = "1.0"
   oBPeTM.InfBPe.Ide.TpBPe = 4
   oBPeTM.InfBPe.Ide.CFOP = "5353"

   oBPeTM.InfBPe.Emit = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.Emit")
   oBPeTM.InfBPe.Emit.CNPJ = "12345678000195"
   oBPeTM.InfBPe.Emit.IE = "123456789012"
   oBPeTM.InfBPe.Emit.XNome = "EMPRESA BP-E TM"
   oBPeTM.InfBPe.Emit.CRT = 3 && Regime normal
   oBPeTM.InfBPe.Emit.EnderEmit = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.EnderEmit")
   oBPeTM.InfBPe.Emit.EnderEmit.XLgr = "RUA TESTE"
   oBPeTM.InfBPe.Emit.EnderEmit.Nro = "100"
   oBPeTM.InfBPe.Emit.EnderEmit.XBairro = "CENTRO"
   oBPeTM.InfBPe.Emit.EnderEmit.CMun = "3550308"
   oBPeTM.InfBPe.Emit.EnderEmit.XMun = "SAO PAULO"
   oBPeTM.InfBPe.Emit.EnderEmit.UF = 35 && Sao Paulo

   oDetBPeTM = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.DetBPeTM")
   oDetBPeTM.IdEqpCont = 1
   oDetBPeTM.UFIniViagem = 35 && Sao Paulo

   oDet = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.Det")
   oDet.NViagem = 1
   oDet.CMunIni = "3550308"
   oDet.QPass = "1"
   oDet.VBP = 10.00
   oDet.Imp = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.Imp")
   oDet.Imp.ICMS = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.ICMS")
   oDet.Imp.ICMS.ICMS00 = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.ICMS00")
   oDet.Imp.ICMS.ICMS00.CST = 0
   oDet.Imp.ICMS.ICMS00.VBC = 10.00
   oDet.Imp.ICMS.ICMS00.PICMS = 18.00
   oDet.Imp.ICMS.ICMS00.VICMS = 1.80

   oComp = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.Comp")
   oComp.XNome = "TARIFA"
   oComp.QComp = "00001"
   oDet.AddComp(oComp)
   oDetBPeTM.AddDet(oDet)
   oBPeTM.InfBPe.AddDetBPeTM(oDetBPeTM)

   oBPeTM.InfBPe.Total = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.Total")
   oBPeTM.InfBPe.Total.QPass = 1
   oBPeTM.InfBPe.Total.VBP = 10.00
   oBPeTM.InfBPe.Total.ICMSTot = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.ICMSTot")
   oBPeTM.InfBPe.Total.ICMSTot.VBC = 10.00
   oBPeTM.InfBPe.Total.ICMSTot.VICMS = 1.80
   oBPeTM.InfBPe.Total.VTotDFe = 10.00

   oBPeTM.InfBPe.PgtoVinc = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.PgtoVinc")
   oPgto = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.Pgto")
   oPgto.NPag = "1"
   oPgto.IdTransacao = "TRANSACAO123"
   oPgto.TpMeioPgto = 17
   oPgto.CNPJReceb = "12345678000195"
   oPgto.CNPJBasePSP = "12345678"
   oBPeTM.InfBPe.PgtoVinc.AddPgto(oPgto)

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")

   TRY
      MESSAGEBOX(oBPeTM.GerarXMLString())

    * Consumir o servico
      oAutorizacao = CREATEOBJECT("Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPeTM")
      oAutorizacao.Executar(oBPeTM, oConfiguracao)

      MESSAGEBOX(oAutorizacao.RetornoWSString)

      IF oAutorizacao.Result.CStat = 100 AND VARTYPE(oAutorizacao.Result.ProtBPe) = "O" AND ;
            oAutorizacao.Result.ProtBPe.InfProt.CStat = 100

         MESSAGEBOX(oAutorizacao.Result.ProtBPe.InfProt.NProt)
         oAutorizacao.GravarXmlDistribuicao("d:\testenfe")
         xmlDistribuicao = oAutorizacao.GetBPeTMProcResults(oBPeTM.InfBPe.Chave)
         MESSAGEBOX(xmlDistribuicao)
      ENDIF

   CATCH TO oErro
    * Excecao do FOXPRO
      MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo, 10)) + " - Message: " + oErro.Message)

    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(), 20)) + " - Message: " + oExceptionInterop.GetMessage())
   ENDTRY
RETURN
