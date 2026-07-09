* ---------------------------------------------------------------------------------
* Enviar BP-e modo sincrono
* ---------------------------------------------------------------------------------
FUNCTION EnviarBPeSincrono()
   LOCAL oConfiguracao, oBPe, oInfViagem, oCompValor, oPag, oAutorizacao
   LOCAL oErro, oExceptionInterop, xmlDistribuicao

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDFe = 22 && 22=BPe
   oConfiguracao.TipoEmissao = 1 && Normal
   oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao.CertificadoSenha = "12345678"

 * Criar XML
   oBPe = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.BPe")
   oBPe.InfBPe = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.InfBPe")
   oBPe.InfBPe.Versao = "1.00"

   oBPe.InfBPe.Ide = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.Ide")
   oBPe.InfBPe.Ide.CUF = 35 && Sao Paulo
   oBPe.InfBPe.Ide.TpAmb = 2 && Homologacao
   oBPe.InfBPe.Ide.Mod = 63 && BP-e
   oBPe.InfBPe.Ide.Serie = 1
   oBPe.InfBPe.Ide.NBP = 1
   oBPe.InfBPe.Ide.CBP = "12345678"
   oBPe.InfBPe.Ide.Modal = 1
   oBPe.InfBPe.Ide.DhEmi = DATETIME()
   oBPe.InfBPe.Ide.TpEmis = 1
   oBPe.InfBPe.Ide.VerProc = "1.0"
   oBPe.InfBPe.Ide.TpBPe = 0
   oBPe.InfBPe.Ide.IndPres = 1
   oBPe.InfBPe.Ide.UFIni = 35 && Sao Paulo
   oBPe.InfBPe.Ide.CMunIni = "3550308"
   oBPe.InfBPe.Ide.UFFim = 33 && Rio de Janeiro
   oBPe.InfBPe.Ide.CMunFim = "3304557"

   oBPe.InfBPe.Emit = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.Emit")
   oBPe.InfBPe.Emit.CNPJ = "12345678000195"
   oBPe.InfBPe.Emit.IE = "123456789012"
   oBPe.InfBPe.Emit.XNome = "EMPRESA BP-E"
   oBPe.InfBPe.Emit.CRT = 3 && Regime normal
   oBPe.InfBPe.Emit.EnderEmit = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.EnderEmit")
   oBPe.InfBPe.Emit.EnderEmit.XLgr = "RUA TESTE"
   oBPe.InfBPe.Emit.EnderEmit.Nro = "100"
   oBPe.InfBPe.Emit.EnderEmit.XBairro = "CENTRO"
   oBPe.InfBPe.Emit.EnderEmit.CMun = "3550308"
   oBPe.InfBPe.Emit.EnderEmit.XMun = "SAO PAULO"
   oBPe.InfBPe.Emit.EnderEmit.UF = 35 && Sao Paulo

   oBPe.InfBPe.InfPassagem = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.InfPassagem")
   oBPe.InfBPe.InfPassagem.CLocOrig = "3550308"
   oBPe.InfBPe.InfPassagem.XLocOrig = "SAO PAULO"
   oBPe.InfBPe.InfPassagem.CLocDest = "3304557"
   oBPe.InfBPe.InfPassagem.XLocDest = "RIO DE JANEIRO"
   oBPe.InfBPe.InfPassagem.DhEmb = DATETIME()
   oBPe.InfBPe.InfPassagem.DhValidade = DATETIME()
   oBPe.InfBPe.InfPassagem.InfPassageiro = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.InfPassageiro")
   oBPe.InfBPe.InfPassagem.InfPassageiro.XNome = "PASSAGEIRO TESTE"
   oBPe.InfBPe.InfPassagem.InfPassageiro.CPF = "12345678901"
   oBPe.InfBPe.InfPassagem.InfPassageiro.TpDoc = 1
   oBPe.InfBPe.InfPassagem.InfPassageiro.NDoc = "12345678"

   oInfViagem = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.InfViagem")
   oInfViagem.CPercurso = "001"
   oInfViagem.XPercurso = "SAO PAULO - RIO DE JANEIRO"
   oInfViagem.TpViagem = 0
   oInfViagem.TpServ = 1
   oInfViagem.TpAcomodacao = 1
   oInfViagem.TpTrecho = 1
   oInfViagem.DhViagem = DATETIME()
   oBPe.InfBPe.AddInfViagem(oInfViagem)

   oBPe.InfBPe.InfValorBPe = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.InfValorBPe")
   oBPe.InfBPe.InfValorBPe.VBP = 100.00
   oBPe.InfBPe.InfValorBPe.VDesconto = 0.00
   oBPe.InfBPe.InfValorBPe.VPgto = 100.00
   oBPe.InfBPe.InfValorBPe.VTroco = 0.00

   oCompValor = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.CompValor")
   oCompValor.TpComp = 1
   oCompValor.VComp = 100.00
   oBPe.InfBPe.InfValorBPe.AddComp(oCompValor)

   oBPe.InfBPe.Imp = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.Imp")
   oBPe.InfBPe.Imp.ICMS = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.ICMS")
   oBPe.InfBPe.Imp.ICMS.ICMS00 = CREATEOBJECT("Unimake.Business.DFe.Xml.BPeTM.ICMS00")
   oBPe.InfBPe.Imp.ICMS.ICMS00.CST = 0
   oBPe.InfBPe.Imp.ICMS.ICMS00.VBC = 100.00
   oBPe.InfBPe.Imp.ICMS.ICMS00.PICMS = 18.00
   oBPe.InfBPe.Imp.ICMS.ICMS00.VICMS = 18.00

   oPag = CREATEOBJECT("Unimake.Business.DFe.Xml.BPe.Pag")
   oPag.TPag = 1
   oPag.VPag = 100.00
   oBPe.InfBPe.AddPag(oPag)

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")

   TRY
      MESSAGEBOX(oBPe.GerarXMLString())

    * Consumir o servico
      oAutorizacao = CREATEOBJECT("Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPe")
      oAutorizacao.Executar(oBPe, oConfiguracao)

      MESSAGEBOX(oAutorizacao.RetornoWSString)

      IF oAutorizacao.Result.CStat = 100 AND VARTYPE(oAutorizacao.Result.ProtBPe) = "O" AND ;
            oAutorizacao.Result.ProtBPe.InfProt.CStat = 100

         MESSAGEBOX(oAutorizacao.Result.ProtBPe.InfProt.NProt)
         oAutorizacao.GravarXmlDistribuicao("d:\testenfe")
         xmlDistribuicao = oAutorizacao.GetBPeProcResults(oBPe.InfBPe.Chave)
         MESSAGEBOX(xmlDistribuicao)
      ENDIF

   CATCH TO oErro
    * Excecao do FOXPRO
      MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo, 10)) + " - Message: " + oErro.Message)

    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(), 20)) + " - Message: " + oExceptionInterop.GetMessage())
   ENDTRY
RETURN
